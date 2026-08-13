module Language.PureScript.CoreFn.Desugar (moduleToCoreFn) where

import Prelude
import Protolude (ordNub, orEmpty)

import Control.Arrow (second)

import Data.Function (on)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (pattern NullSourceSpan, SourceSpan(..))
import Language.PureScript.AST.Traversals (everythingOnValues)
import Data.List (nubBy)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Ann (Ann, ssAnn, CoreFnType(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.CoreFn.Expr (Bind(..), CaseAlternative(..), Expr(..), Guard)
import Language.PureScript.CoreFn.Meta (ConstructorType(..), Meta(..))
import Language.PureScript.CoreFn.Module (Module(..), DataDecl(..), DataConstructor(..), ClassDecl(..))
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), TypeKind(..), Environment(..), NameKind(..), isDictTypeName, lookupConstructor, lookupValue)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), getQual)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Types (pattern REmptyKinded, SourceType, Type(..), replaceAllTypeVars, constraintClass, constraintArgs, Constraint(..))
import Language.PureScript.AST qualified as A
import Language.PureScript.Constants.Prim qualified as C

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module modSS coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      reExps = M.map ordNub $ M.unionsWith (++) (mapMaybe (fmap reExportsToCoreFn . toReExportRef) exps)
      externs = nubBy ((==) `on` snd) $ mapMaybe (externToCoreFn env) decls
      decls' = concatMap declToCoreFn decls
      dataDecls' = concatMap dataDeclToCoreFn decls
      classDecls' = concatMap classDeclToCoreFn decls
  in Module modSS coms mn (spanName modSS) imports' exps' reExps externs decls' dataDecls' classDecls'
  where
  -- Creates a map from a module name to the re-export references defined in
  -- that module.
  reExportsToCoreFn :: (ModuleName, A.DeclarationRef) -> M.Map ModuleName [Ident]
  reExportsToCoreFn (mn', ref') = M.singleton mn' (exportToCoreFn ref')

  toReExportRef :: A.DeclarationRef -> Maybe (ModuleName, A.DeclarationRef)
  toReExportRef (A.ReExportRef _ src ref) =
      fmap
        (, ref)
        (A.exportSourceImportedFrom src)
  toReExportRef _ = Nothing

  -- Remove duplicate imports
  dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
  dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

  ssA :: SourceSpan -> Ann
  ssA ss = (ss, [], Nothing, Nothing)

  -- Extracts data declarations (schema) from AST to CoreFn representation.
  dataDeclToCoreFn :: A.Declaration -> [DataDecl]
  dataDeclToCoreFn (A.DataDeclaration _ Data tyName tyVars ctors) =
    [DataDecl tyName (fmap fst tyVars) (fmap (\ctorDecl -> DataConstructor (A.dataCtorName ctorDecl) (fmap (simplifyType env . snd) (A.dataCtorFields ctorDecl))) ctors)]
  dataDeclToCoreFn (A.DataBindingGroupDeclaration ds) =
    concatMap dataDeclToCoreFn ds
  dataDeclToCoreFn _ = []

  -- Extracts type class declarations from AST to CoreFn representation.
  classDeclToCoreFn :: A.Declaration -> [ClassDecl]
  classDeclToCoreFn (A.TypeClassDeclaration _ className tyVars superclasses _ members) =
    let typeVars' = fmap fst tyVars
        superclasses' = fmap (\c -> (constraintClass c, fmap (simplifyType env) (constraintArgs c))) superclasses
        methods' = mapMaybe (fmap (\td -> let (ident, ty) = A.unwrapTypeDeclaration td in (ident, simplifyType env ty)) . A.getTypeDeclaration) members
    in [ClassDecl className typeVars' superclasses' methods']
  classDeclToCoreFn _ = []

  -- Desugars member declarations from AST to CoreFn representation.
  declToCoreFn :: A.Declaration -> [Bind Ann]
  declToCoreFn (A.DataDeclaration (ss, com) Newtype _ _ [ctor]) =
    [NonRec (ss, [], Nothing, declMeta) (properToIdent $ A.dataCtorName ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var (ssAnn ss) $ Qualified ByNullSourcePos (Ident "x"))]
    where
    declMeta = isDictTypeName (A.dataCtorName ctor) `orEmpty` IsTypeClassConstructor
  declToCoreFn d@(A.DataDeclaration _ Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn (A.DataDeclaration (ss, com) Data tyName _ ctors) =
    flip fmap ctors $ \ctorDecl ->
      let
        ctor = A.dataCtorName ctorDecl
        (_, _, _, fields) = lookupConstructor env (Qualified (ByModuleName mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
  declToCoreFn (A.DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn ds
  declToCoreFn (A.ValueDecl (ss, com) name _ _ [A.MkUnguarded e]) =
    [NonRec (ssA ss) name (exprToCoreFn ss com Nothing e)]
  declToCoreFn (A.BindingGroupDeclaration ds) =
    [Rec . NEL.toList $ fmap (\(((ss, com), name), _, e) -> ((ssA ss, name), exprToCoreFn ss com Nothing e)) ds]
  declToCoreFn _ = []

  -- Desugars expressions from AST to CoreFn representation.
  exprToCoreFn :: SourceSpan -> [Comment] -> Maybe SourceType -> A.Expr -> Expr Ann
  exprToCoreFn _ com ty (A.Literal ss lit) =
    Literal (ss, com, simplifyType env <$> ty, Nothing) (fmap (exprToCoreFn ss com Nothing) lit)
  exprToCoreFn ss com ty (A.Accessor name v) =
    Accessor (ss, com, simplifyType env <$> ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, simplifyType env <$> ty, Nothing) (exprToCoreFn ss [] Nothing obj) (ty >>= unchangedRecordFields (fmap fst vs)) $ fmap (second (exprToCoreFn ss [] Nothing)) vs
    where
    -- Return the unchanged labels of a closed record, or Nothing for other types or open records.
    unchangedRecordFields :: [PSString] -> Type a -> Maybe [PSString]
    unchangedRecordFields updated (TypeApp _ (TypeConstructor _ C.Record) row) =
      collect row
      where
        collect :: Type a -> Maybe [PSString]
        collect (REmptyKinded _ _) = Just []
        collect (RCons _ (Label l) _ r) = (if l `elem` updated then id else (l :)) <$> collect r
        collect _ = Nothing
    unchangedRecordFields _ _ = Nothing
  exprToCoreFn ss com ty (A.Abs (A.VarBinder _ name) v) =
    Abs (ss, com, simplifyType env <$> ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (A.Abs _ _) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com ty (A.App v1 v2) =
    App (ss, com, simplifyType env <$> ty, (isDictCtor v1 || isSynthetic v2) `orEmpty` IsSyntheticApp) v1' v2'
    where
    v1' = exprToCoreFn ss [] Nothing v1
    v2' = exprToCoreFn ss [] Nothing v2
    isDictCtor = \case
      A.Constructor _ (Qualified _ name) -> isDictTypeName name
      _ -> False
    isSynthetic = \case
      A.App v3 v4            -> isDictCtor v3 || isSynthetic v3 && isSynthetic v4
      A.Accessor _ v3        -> isSynthetic v3
      A.Var NullSourceSpan _ -> True
      A.Unused{}             -> True
      _                      -> False
  exprToCoreFn ss com _ (A.Unused _) =
    Var (ss, com, Nothing, Nothing) C.I_undefined
  exprToCoreFn _ com ty (A.Var ss ident) =
    Var (ss, com, simplifyType env <$> ty, getValueMeta ident) ident
  exprToCoreFn ss com ty (A.IfThenElse v1 v2 v3) =
    Case (ss, com, simplifyType env <$> ty, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                        (Right $ exprToCoreFn ss [] Nothing v2)
      , CaseAlternative [NullBinder (ssAnn ss)]
                        (Right $ exprToCoreFn ss [] Nothing v3) ]
  exprToCoreFn _ com ty (A.Constructor ss name) =
    Var (ss, com, simplifyType env <$> ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com ty (A.Case vs alts) =
    Case (ss, com, simplifyType env <$> ty, Nothing) (fmap (exprToCoreFn ss [] Nothing) vs) (fmap (altToCoreFn ss) alts)
  exprToCoreFn ss com _ (A.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com ty (A.Let w ds v) =
    Let (ss, com, simplifyType env <$> ty, getLetMeta w) (concatMap declToCoreFn ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ com ty (A.PositionedValue ss com1 v) =
    exprToCoreFn ss (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- Desugars case alternatives from AST to CoreFn representation.
  altToCoreFn :: SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: [A.GuardedExpr] -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go [A.MkUnguarded e]
      = Right (exprToCoreFn ss [] Nothing e)
    go gs
      = Left [ (exprToCoreFn ss [] Nothing cond, exprToCoreFn ss [] Nothing e)
             | A.GuardedExpr g e <- gs
             , let cond = guardToExpr g
             ]

    guardToExpr [A.ConditionGuard cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- Desugars case binders from AST to CoreFn representation.
  binderToCoreFn :: SourceSpan -> [Comment] -> A.Binder -> Binder Ann
  binderToCoreFn _ com (A.LiteralBinder ss lit) =
    LiteralBinder (ss, com, Nothing, Nothing) (fmap (binderToCoreFn ss com) lit)
  binderToCoreFn ss com A.NullBinder =
    NullBinder (ss, com, Nothing, Nothing)
  binderToCoreFn _ com (A.VarBinder ss name) =
    VarBinder (ss, com, Nothing, Nothing) name
  binderToCoreFn _ com (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (fmap (binderToCoreFn ss []) bs)
  binderToCoreFn _ com (A.NamedBinder ss name b) =
    NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn ss (com ++ com1) b
  binderToCoreFn ss com (A.TypedBinder _ b) =
    binderToCoreFn ss com b
  binderToCoreFn _ _ A.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- Gets metadata for let bindings.
  getLetMeta :: A.WhereProvenance -> Maybe Meta
  getLetMeta A.FromWhere = Just IsWhere
  getLetMeta A.FromLet = Nothing

  -- Gets metadata for values.
  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    case lookupValue env name of
      Just (_, External, _) -> Just IsForeign
      _ -> Nothing

  -- Gets metadata for data constructors.
  getConstructorMeta :: Qualified (ProperName 'ConstructorName) -> Meta
  getConstructorMeta ctor =
    case lookupConstructor env ctor of
      (Newtype, _, _, _) -> IsNewtype
      dc@(Data, _, _, fields) ->
        let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
        in IsConstructor constructorType fields
    where

    numConstructors
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> Int
    numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

    typeConstructor
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> (ModuleName, ProperName 'TypeName)
    typeConstructor (Qualified (ByModuleName mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
    typeConstructor _ = internalError "Invalid argument to typeConstructor"

-- | Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
  in f `concatMap` decls
  where
  fqDecls :: A.Declaration -> [ModuleName]
  fqDecls (A.TypeInstanceDeclaration _ _ _ _ _ _ q _ _) = getQual' q
  fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
  fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var _ q) = getQual' q
  fqValues (A.Constructor _ q) = getQual' q
  fqValues _ = []

  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: Environment -> A.Declaration -> Maybe (Ann, Ident)
externToCoreFn env (A.ExternDeclaration (ss, com) name ty) = Just ((ss, com, Just (simplifyType env ty), Just IsForeign), name)
externToCoreFn _ _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, instances and
-- values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.TypeRef _ _ Nothing) = []
exportToCoreFn (A.TypeOpRef _ _) = []
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.ValueOpRef _ _) = []
exportToCoreFn (A.TypeClassRef _ _) = []
exportToCoreFn (A.TypeInstanceRef _ name _) = [name]
exportToCoreFn (A.ModuleRef _ _) = []
exportToCoreFn (A.ReExportRef _ _ _) = []

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName

-- | Simplifies a SourceType into a CoreFnType
simplifyType :: Environment -> SourceType -> CoreFnType
simplifyType = simplifyType' S.empty S.empty

stripDictTypeName :: ProperName a -> ProperName a
stripDictTypeName (ProperName n) = ProperName (fromMaybe n (T.stripSuffix "$Dict" n))

disqual :: Qualified a -> a
disqual (Qualified _ a) = a

simplifyType' :: S.Set (Qualified (ProperName 'TypeName)) -> S.Set (Qualified (ProperName 'ClassName)) -> Environment -> SourceType -> CoreFnType
simplifyType' visited visitedClasses env (ForAll _ _ ident _ ty _) = 
  let (vars, body) = collectForAlls [ident] ty
  in CFForAll vars (simplifyType' visited visitedClasses env body)
  where
    collectForAlls vars (ForAll _ _ i _ t _) = collectForAlls (vars ++ [i]) t
    collectForAlls vars t = (vars, t)
simplifyType' visited visitedClasses env (ConstrainedType _ constraint ty) =
  let (constraints, body) = collectConstraints [constraint] ty
      constraints' = map (\c -> (constraintClass c, map (simplifyType' visited visitedClasses env) (constraintArgs c))) constraints
  in CFConstrainedType constraints' (simplifyType' visited visitedClasses env body)
  where
    collectConstraints constraints (ConstrainedType _ c t) = collectConstraints (constraints ++ [c]) t
    collectConstraints constraints t = (constraints, t)
simplifyType' visited visitedClasses env (ParensInType _ ty) = simplifyType' visited visitedClasses env ty
simplifyType' _ _ _ (TypeLevelString _ s) = CFTypeLevelString s
simplifyType' visited visitedClasses env (TypeConstructor _ qname@(Qualified _ (ProperName name)))
  | name == "Int"     = CFInt
  | name == "Number"  = CFNumber
  | name == "String"  = CFString
  | name == "Boolean" = CFBoolean
  | name == "Char"    = CFChar
  | name == "Unit"    = CFUnit
  | otherwise =
      if isDictTypeName (disqual qname)
      then CFAdt (fmap stripDictTypeName qname) []
      else case M.lookup qname (types env) of
        Just (_, DataType Data _ _) -> CFAdt qname []
        Just (_, ExternData _) -> CFAdt qname []
        Just (_, DataType Newtype _ [(_, [underlyingType])]) -> 
            if S.member qname visited then CFAdt qname []
            else simplifyType' (S.insert qname visited) visitedClasses env underlyingType
        _ -> CFAny
simplifyType' visited visitedClasses env (TypeApp _ (TypeConstructor _ (Qualified _ (ProperName name))) inner)
  | name == "Array" = CFArray (simplifyType' visited visitedClasses env inner)
simplifyType' visited visitedClasses env (TypeApp _ (TypeApp _ (TypeConstructor _ (Qualified _ (ProperName name))) arg) ret)
  | name == "Function" =
      let
        argT = simplifyType' visited visitedClasses env arg
        retT = simplifyType' visited visitedClasses env ret
      in case retT of
           CFFunc args' ret' -> CFFunc (argT : args') ret'
           _ -> CFFunc [argT] retT
simplifyType' visited visitedClasses env (TypeApp _ (TypeConstructor _ (Qualified _ (ProperName name))) row)
  | name == "Record" =
      let
        rowToFields (RCons _ (Label l) ty rest) = do
          (fields, tailTy) <- rowToFields rest
          return ((l, simplifyType' visited visitedClasses env ty) : fields, tailTy)
        rowToFields (REmpty _) = Just ([], Nothing)
        rowToFields (TypeVar _ tv) = Just ([], Just (CFTypeVar tv))
        rowToFields (Skolem _ tv _ _ _) = Just ([], Just (CFTypeVar tv))
        rowToFields _ = Just ([], Just CFAny)
      in case rowToFields row of
           Just (fields, tailTy) -> CFRecord (CFRow fields tailTy)
           Nothing -> CFAny
simplifyType' visited visitedClasses env tApp@(TypeApp _ _ _) =
  let (base, args) = collectTypeArgs tApp
   in case base of
        TypeConstructor _ qname -> 
          if isDictTypeName (disqual qname)
          then CFAdt (fmap stripDictTypeName qname) (map (simplifyType' visited visitedClasses env) args)
          else case M.lookup qname (types env) of
            Just (_, DataType Data _ _) -> CFAdt qname (map (simplifyType' visited visitedClasses env) args)
            Just (_, ExternData _) -> CFAdt qname (map (simplifyType' visited visitedClasses env) args)
            Just (_, DataType Newtype typeVars [(_, [underlyingType])]) -> 
                if S.member qname visited then CFAdt qname (map (simplifyType' visited visitedClasses env) args)
                else let subst = zip (map (\(v, _, _) -> v) typeVars) args
                         underlyingTypeSubst = replaceAllTypeVars subst underlyingType
                     in simplifyType' (S.insert qname visited) visitedClasses env underlyingTypeSubst
            _ -> CFTypeApp (simplifyType' visited visitedClasses env base) (map (simplifyType' visited visitedClasses env) args)
        _ -> CFTypeApp (simplifyType' visited visitedClasses env base) (map (simplifyType' visited visitedClasses env) args)
  where
    collectTypeArgs :: SourceType -> (SourceType, [SourceType])
    collectTypeArgs (TypeApp _ t1 t2) =
      let (base, args) = collectTypeArgs t1
      in (base, args ++ [t2])
    collectTypeArgs t = (t, [])
simplifyType' visited visitedClasses env row@(RCons _ _ _ _) =
  let
    rowToFields (RCons _ (Label l) ty rest) = do
      (fields, tailTy) <- rowToFields rest
      return ((l, simplifyType' visited visitedClasses env ty) : fields, tailTy)
    rowToFields (REmpty _) = Just ([], Nothing)
    rowToFields (TypeVar _ tv) = Just ([], Just (CFTypeVar tv))
    rowToFields (Skolem _ tv _ _ _) = Just ([], Just (CFTypeVar tv))
    rowToFields _ = Just ([], Just CFAny)
  in case rowToFields row of
       Just (fields, tailTy) -> CFRow fields tailTy
       Nothing -> CFAny
simplifyType' _ _ _ (REmpty _) = CFRow [] Nothing
simplifyType' visited visitedClasses env (KindApp _ ty _) = simplifyType' visited visitedClasses env ty
simplifyType' visited visitedClasses env (KindedType _ ty _) = simplifyType' visited visitedClasses env ty
simplifyType' _ _ _ (TypeVar _ name) = CFTypeVar name
simplifyType' _ _ _ (Skolem _ name _ _ _) = CFTypeVar name
simplifyType' _ _ _ _ = CFAny