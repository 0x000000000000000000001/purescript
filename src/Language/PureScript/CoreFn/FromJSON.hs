-- |
-- Read the core functional representation from JSON format
--

module Language.PureScript.CoreFn.FromJSON
  ( moduleFromJSON
  , parseVersion'
  ) where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, when)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify, lift)

import Data.Aeson (FromJSON(..), Object, Value(..), withObject, withText, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser, listParser)
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Version (Version, parseVersion)

import Language.PureScript.AST.SourcePos (SourceSpan(..))
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CoreFn.Ann (Ann, ssAnn, BindingId(..), BindingUsage(..), LastLocalUse(..), VariableUse(..), UsageInfo(..), emptyUsageInfo)
import Language.PureScript.CoreFn (Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Guard, Meta(..), Module(..), CoreFnType(..))
import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..), unusedIdent, runIdent, pattern ByNullSourcePos)
import Language.PureScript.PSString (PSString, mkString)

import Text.ParserCombinators.ReadP (readP_to_S)

-- | Source path and shared type table used by every annotation reader.
data JSONContext = JSONContext
  { contextPath :: FilePath
  , contextTypes :: V.Vector Value
  }

parseVersion' :: String -> Maybe Version
parseVersion' str =
  case filter (null . snd) $ readP_to_S parseVersion str of
    [(vers, "")] -> Just vers
    _            -> Nothing

constructorTypeFromJSON :: Value -> Parser ConstructorType
constructorTypeFromJSON v = do
  t <- parseJSON v
  case t of
    "ProductType" -> return ProductType
    "SumType"     -> return SumType
    _             -> fail ("not recognized ConstructorType: " ++ T.unpack t)

metaFromJSON :: Value -> Parser (Maybe Meta)
metaFromJSON Null = return Nothing
metaFromJSON v = withObject "Meta" metaFromObj v
  where
    metaFromObj o = do
      type_ <- o .: "metaType"
      case type_ of
        "IsConstructor" -> isConstructorFromJSON o
        "IsNewtype"     -> return $ Just IsNewtype
        "IsTypeClassConstructor"
                        -> return $ Just IsTypeClassConstructor
        "IsForeign"     -> return $ Just IsForeign
        "IsWhere"       -> return $ Just IsWhere
        "IsSyntheticApp"
                        -> return $ Just IsSyntheticApp
        _               -> fail ("not recognized Meta: " ++ T.unpack type_)

    isConstructorFromJSON o = do
      ct <- o .: "constructorType" >>= constructorTypeFromJSON
      is <- o .: "identifiers" >>= listParser identFromJSON
      return $ Just (IsConstructor ct is)

coreFnTypeFromJSON :: V.Vector Value -> Value -> Parser (Maybe CoreFnType)
coreFnTypeFromJSON table = go S.empty
  where
  go seen value@(Number _) = do
    index <- parseJSON value
    when (S.member index seen) $ fail "cyclic CoreFn type-table reference"
    case table V.!? index of
      Nothing -> fail "invalid CoreFn type-table reference"
      Just entry -> go (S.insert index seen) entry
  go _ Null = return Nothing
  go seen (Object o) = do
    typ :: Text <- o .: "type"
    case typ of
      "Int" -> return $ Just CFInt
      "Number" -> return $ Just CFNumber
      "String" -> return $ Just CFString
      "Boolean" -> return $ Just CFBoolean
      "Char" -> return $ Just CFChar
      "Unit" -> return $ Just CFUnit
      "Any" -> return $ Just CFAny
      "TypeLevelString" -> do
        val <- o .: "value"
        return $ Just (CFTypeLevelString (Language.PureScript.PSString.mkString val))
      "Array" -> do
        inner <- o .: "element" >>= go seen
        return $ CFArray <$> inner
      "TypeVar" -> do
        name <- o .: "name"
        return $ Just (CFTypeVar name)
      "Adt" -> do
        fqn <- o .: "fqn"
        argsVal <- o .: "args"
        args <- mapM (\v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType") argsVal
        let qname = case fqn of
              [name] -> Qualified ByNullSourcePos (ProperName name)
              _ -> let mn = ModuleName (T.intercalate "." (init fqn))
                       name = ProperName (last fqn)
                   in Qualified (ByModuleName mn) name
        return $ Just (CFAdt qname args)
      "TypeApp" -> do
        constructor <- o .: "constructor" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        argsVal <- o .: "args"
        args <- mapM (\v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType") argsVal
        return $ Just (CFTypeApp constructor args)
      "Func" -> do
        argsVal <- o .: "args"
        args <- mapM (\v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType") argsVal
        ret <- o .: "ret" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        return $ Just (CFFunc args ret)
      "Row" -> do
        fieldsVal <- o .: "fields"
        fields <- mapM parseField fieldsVal
        tailTy <- o .:? "tail" >>= \case
                    Just Null -> return Nothing
                    Just v -> go seen v
                    Nothing -> return Nothing
        return $ Just (CFRow fields tailTy)
      "Record" -> do
        row <- o .: "row" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        return $ Just (CFRecord row)
      "ForAll" -> do
        vars <- o .: "vars"
        body <- o .: "body" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        return $ Just (CFForAll vars body)
      "ConstrainedType" -> do
        constraintsVal <- o .: "constraints"
        constraints <- mapM parseConstraint constraintsVal
        body <- o .: "body" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        return $ Just (CFConstrainedType constraints body)
      _ -> return $ Just CFAny
    where
      parseField = withObject "Field" $ \obj -> do
        l <- obj .: "label"
        t <- obj .: "type" >>= \v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType"
        return (Language.PureScript.PSString.mkString l, t)

      parseConstraint = withObject "Constraint" $ \obj -> do
        fqn <- obj .: "fqn"
        argsVal <- obj .: "args"
        args <- mapM (\v -> go seen v >>= \case Just a -> return a; Nothing -> fail "Expected CoreFnType") argsVal
        let qname = case fqn of
              [name] -> Qualified ByNullSourcePos (ProperName name)
              _ -> let mn = ModuleName (T.intercalate "." (init fqn))
                       name = ProperName (last fqn)
                   in Qualified (ByModuleName mn) name
        return (qname, args)
  go _ _ = return $ Just CFAny

annFromJSON :: JSONContext -> Value -> Parser Ann
annFromJSON context = withObject "Ann" annFromObj
  where
  annFromObj o = do
    ss <- o .: "sourceSpan" >>= sourceSpanFromJSON (contextPath context)
    mtyVal <- o .:? "type"
    mty <- case mtyVal of
             Nothing -> return Nothing
             Just v -> coreFnTypeFromJSON (contextTypes context) v
    mm <- o .: "meta" >>= metaFromJSON
    binding <- o .:? "bindingUsage" >>= traverse bindingUsageFromJSON
    variable <- o .:? "variableUse" >>= traverse variableUseFromJSON
    let info = UsageInfo binding variable
        mUsage = if info == emptyUsageInfo then Nothing else Just info
    return (ss, [], mty, mm, mUsage)

bindingIdFromJSON :: Value -> Parser BindingId
bindingIdFromJSON value = do
  ident <- parseJSON value
  when (ident < 0) $ fail "bindingId must be nonnegative"
  pure (BindingId ident)

bindingUsageFromJSON :: Value -> Parser BindingUsage
bindingUsageFromJSON = withObject "BindingUsage" $ \o -> do
  ident <- o .: "bindingId" >>= bindingIdFromJSON
  maxUses <- o .:? "maxUses"
  when (maybe False (< 0) maxUses) $ fail "maxUses must be nonnegative or null"
  escapingContext <- o .:? "hasEscapingUseContext"
  pure (BindingUsage ident maxUses escapingContext)

variableUseFromJSON :: Value -> Parser VariableUse
variableUseFromJSON = withObject "VariableUse" $ \o -> do
  ident <- o .: "bindingId" >>= bindingIdFromJSON
  lastUse <- o .:? "lastLocalUse" >>= \case
    Nothing -> pure UnknownLastLocalUse
    Just (Bool True) -> pure ProvenLastLocalUse
    _ -> fail "lastLocalUse must be true or null"
  pure (VariableUse ident lastUse)

sourceSpanFromJSON :: FilePath -> Value -> Parser SourceSpan
sourceSpanFromJSON modulePath = withObject "SourceSpan" $ \o ->
  SourceSpan modulePath <$>
    o .: "start" <*>
    o .: "end"

literalFromJSON :: (Value -> Parser a) -> Value -> Parser (Literal a)
literalFromJSON t = withObject "Literal" literalFromObj
  where
  literalFromObj o = do
    type_ <- o .: "literalType" :: Parser Text
    case type_ of
      "IntLiteral"      -> NumericLiteral . Left <$> o .: "value"
      "NumberLiteral"   -> NumericLiteral . Right <$> o .: "value"
      "StringLiteral"   -> StringLiteral <$> o .: "value"
      "CharLiteral"     -> CharLiteral <$> o .: "value"
      "BooleanLiteral"  -> BooleanLiteral <$> o .: "value"
      "ArrayLiteral"    -> parseArrayLiteral o
      "ObjectLiteral"   -> parseObjectLiteral o
      _                 -> fail ("error parsing Literal: " ++ show o)

  parseArrayLiteral o = do
    val <- o .: "value"
    as <- mapM t (V.toList val)
    return $ ArrayLiteral as

  parseObjectLiteral o = do
    val <- o .: "value"
    ObjectLiteral <$> recordFromJSON t val

identFromJSON :: Value -> Parser Ident
identFromJSON = withText "Ident" $ \case
  ident | ident == unusedIdent -> pure UnusedIdent 
        | otherwise -> pure $ Ident ident 

properNameFromJSON :: Value -> Parser (ProperName a)
properNameFromJSON = fmap ProperName . parseJSON

qualifiedFromJSON :: (Text -> a) -> Value -> Parser (Qualified a)
qualifiedFromJSON f = withObject "Qualified" qualifiedFromObj
  where
  qualifiedFromObj o =
    qualifiedByModuleFromObj o <|> qualifiedBySourcePosFromObj o
  qualifiedByModuleFromObj o = do
    mn <- o .: "moduleName" >>= moduleNameFromJSON
    i  <- o .: "identifier" >>= withText "Ident" (return . f)
    pure $ Qualified (ByModuleName mn) i
  qualifiedBySourcePosFromObj o = do
    ss <- o .: "sourcePos"
    i  <- o .: "identifier" >>= withText "Ident" (return . f)
    pure $ Qualified (BySourcePos ss) i

moduleNameFromJSON :: Value -> Parser ModuleName
moduleNameFromJSON v = ModuleName . T.intercalate "." <$> listParser parseJSON v

moduleFromJSON :: Value -> Parser (Version, Module Ann)
moduleFromJSON = withObject "Module" moduleFromObj
  where
  moduleFromObj o = do
    version <- o .: "builtWith" >>= versionFromJSON
    moduleName <- o .: "moduleName" >>= moduleNameFromJSON
    modulePath <- o .: "modulePath"
    types <- o .:? "typeTable" .!= V.empty
    let context = JSONContext modulePath types
    moduleSourceSpan <- o .: "sourceSpan" >>= sourceSpanFromJSON modulePath
    moduleImports <- o .: "imports" >>= listParser (importFromJSON context)
    moduleExports <- o .: "exports" >>= listParser identFromJSON
    moduleReExports <- o .: "reExports" >>= reExportsFromJSON
    moduleDecls <- o .: "decls" >>= listParser (bindFromJSON context)
    foreignIdents <- o .: "foreign" >>= listParser identFromJSON
    foreignAnnsRaw <- o .:? "foreignAnnotations" .!= M.empty :: Parser (M.Map String Value)
    moduleForeign <- mapM (\ident -> do
          let key = T.unpack (runIdent ident)
          ann <- case M.lookup key foreignAnnsRaw of
                   Just val -> annFromJSON context val
                   Nothing -> return (ssAnn moduleSourceSpan)
          return (ann, ident)
      ) foreignIdents
    moduleComments <- o .: "comments" >>= listParser parseJSON
    let moduleDataDecls = []
    let moduleClassDecls = []
    let coreModule = Module {..}
    validateUsageIdentities coreModule
    return (version, coreModule)

  versionFromJSON :: String -> Parser Version
  versionFromJSON v =
    case parseVersion' v of
      Just r -> return r
      Nothing -> fail "failed parsing purs version"

  importFromJSON :: JSONContext -> Value -> Parser (Ann, ModuleName)
  importFromJSON context = withObject "Import"
    (\o -> do
      ann <- o .: "annotation" >>= annFromJSON context
      mn  <- o .: "moduleName" >>= moduleNameFromJSON
      return (ann, mn))


  reExportsFromJSON = fmap (M.map (map Ident)) . parseJSON

bindFromJSON :: JSONContext -> Value -> Parser (Bind Ann)
bindFromJSON context = withObject "Bind" bindFromObj
  where
  bindFromObj :: Object -> Parser (Bind Ann)
  bindFromObj o = do
    type_ <- o .: "bindType" :: Parser Text
    case type_ of
      "NonRec"  -> (uncurry . uncurry) NonRec <$> bindFromObj' o
      "Rec"     -> Rec <$> (o .: "binds" >>= listParser (withObject "Bind" bindFromObj'))
      _         -> fail ("not recognized bind type \"" ++ T.unpack type_ ++ "\"")
        
  bindFromObj' :: Object -> Parser ((Ann, Ident), Expr Ann)
  bindFromObj' o = do
    a <- o .: "annotation" >>= annFromJSON context
    i <- o .: "identifier" >>= identFromJSON
    e <- o .: "expression" >>= exprFromJSON context
    return ((a, i), e)

recordFromJSON :: (Value -> Parser a) -> Value -> Parser [(PSString, a)]
recordFromJSON p = listParser parsePair
  where
  parsePair v = do
    (l, v') <- parseJSON v :: Parser (PSString, Value)
    a <- p v'
    return (l, a)

exprFromJSON :: JSONContext -> Value -> Parser (Expr Ann)
exprFromJSON context = withObject "Expr" exprFromObj
  where
  exprFromObj o = do
    type_ <- o .: "type"
    case type_ of
      "Var"           -> varFromObj o
      "Literal"       -> literalExprFromObj o
      "Constructor"   -> constructorFromObj o
      "Accessor"      -> accessorFromObj o
      "ObjectUpdate"  -> objectUpdateFromObj o
      "Abs"           -> absFromObj o
      "App"           -> appFromObj o
      "TypeApp"       -> typeAppFromObj o
      "Case"          -> caseFromObj o
      "Let"           -> letFromObj o
      _               -> fail ("not recognized expression type: \"" ++ T.unpack type_ ++ "\"")

  varFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    qi <- o .: "value" >>= qualifiedFromJSON Ident
    return $ Var ann qi

  literalExprFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    lit <- o .: "value" >>= literalFromJSON (exprFromJSON context)
    return $ Literal ann lit

  constructorFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    tyn <- o .: "typeName" >>= properNameFromJSON
    con <- o .: "name" <|> o .: "constructorName" >>= properNameFromJSON
    is  <- o .: "fields" <|> o .: "fieldNames" >>= listParser identFromJSON
    return $ Constructor ann tyn con is

  accessorFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    f   <- o .: "fieldName"
    e <- o .: "expression" >>= exprFromJSON context
    return $ Accessor ann f e

  objectUpdateFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    e   <- o .: "expression" >>= exprFromJSON context
    copy <- o .: "copy" >>= parseJSON
    us  <- o .: "updates" >>= recordFromJSON (exprFromJSON context)
    return $ ObjectUpdate ann e copy us

  absFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    idn <- o .: "argument" >>= identFromJSON
    e   <- o .: "body" >>= exprFromJSON context
    return $ Abs ann idn e

  appFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    e   <- o .: "abstraction" >>= exprFromJSON context
    e'  <- o .: "argument" >>= exprFromJSON context
    return $ App ann e e'

  typeAppFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    e <- o .: "expression" >>= exprFromJSON context
    ty <- o .: "typeArgument" >>= coreFnTypeFromJSON (contextTypes context) >>= \case
      Just t -> pure t
      Nothing -> fail "Expected CoreFnType for typeArgument"
    return $ TypeApp ann e ty

  caseFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    cs  <- o .: "caseExpressions" >>= listParser (exprFromJSON context)
    cas <- o .: "caseAlternatives" >>= listParser (caseAlternativeFromJSON context)
    return $ Case ann cs cas

  letFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    bs  <- o .: "binds" >>= listParser (bindFromJSON context)
    e   <- o .: "expression" >>= exprFromJSON context
    return $ Let ann bs e

caseAlternativeFromJSON :: JSONContext -> Value -> Parser (CaseAlternative Ann)
caseAlternativeFromJSON context = withObject "CaseAlternative" caseAlternativeFromObj
  where
    caseAlternativeFromObj o = do
      bs <- o .: "binders" >>= listParser (binderFromJSON context)
      isGuarded <- o .: "isGuarded"
      if isGuarded
        then do
          es <- o .: "expressions" >>= listParser parseResultWithGuard
          return $ CaseAlternative bs (Left es)
        else do
          e <- o .: "expression" >>= exprFromJSON context
          return $ CaseAlternative bs (Right e)

    parseResultWithGuard :: Value -> Parser (Guard Ann, Expr Ann)
    parseResultWithGuard = withObject "parseCaseWithGuards" $
      \o -> do
        g <- o .: "guard" >>= exprFromJSON context
        e <- o .: "expression" >>= exprFromJSON context
        return (g, e)

binderFromJSON :: JSONContext -> Value -> Parser (Binder Ann)
binderFromJSON context = withObject "Binder" binderFromObj
  where
  binderFromObj o = do
    type_ <- o .: "binderType"
    case type_ of
      "NullBinder"        -> nullBinderFromObj o
      "VarBinder"         -> varBinderFromObj o
      "LiteralBinder"     -> literalBinderFromObj o
      "ConstructorBinder" -> constructorBinderFromObj o
      "NamedBinder"       -> namedBinderFromObj o
      _                   -> fail ("not recognized binder: \"" ++ T.unpack type_ ++ "\"")


  nullBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    return $ NullBinder ann

  varBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    idn <- o .: "identifier" >>= identFromJSON
    return $ VarBinder ann idn

  literalBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    lit <- o .: "literal" >>= literalFromJSON (binderFromJSON context)
    return $ LiteralBinder ann lit

  constructorBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    tyn <- o .: "typeName" >>= qualifiedFromJSON ProperName
    con <- o .: "name" <|> o .: "constructorName" >>= qualifiedFromJSON ProperName
    bs  <- o .: "binders" >>= listParser (binderFromJSON context)
    return $ ConstructorBinder ann tyn con bs

  namedBinderFromObj o = do
    ann <- o .: "annotation" >>= annFromJSON context
    n   <- o .: "identifier" >>= identFromJSON
    b   <- o .: "binder" >>= binderFromJSON context
    return $ NamedBinder ann n b

-- | Reject dangling identities, duplicate declarations, and facts on the wrong
-- kind of node. This validates identity and scope, not the usage proof itself.
-- A binding with missing metadata still shadows an outer binding of that name.
type UsageScope = M.Map Ident (Maybe BindingId)
type UsageValidation = StateT (S.Set BindingId) Parser

validateUsageIdentities :: Module Ann -> Parser ()
validateUsageIdentities coreModule = flip evalStateT S.empty $ do
  traverse_ (validatePlainAnn . fst) (moduleImports coreModule)
  traverse_ (validatePlainAnn . fst) (moduleForeign coreModule)
  traverse_ validateTopLevel (moduleDecls coreModule)
  where
  validateTopLevel (NonRec ann _ expr) = do
    validatePlainAnn ann
    validateUsageExpr M.empty expr
  validateTopLevel (Rec binds) = traverse_ (\((ann, _), expr) -> do
    validatePlainAnn ann
    validateUsageExpr M.empty expr) binds

annUsage :: Ann -> UsageInfo
annUsage (_, _, _, _, info) = maybe emptyUsageInfo id info

validatePlainAnn :: Ann -> UsageValidation ()
validatePlainAnn ann = do
  let info = annUsage ann
  unless (bindingUsage info == Nothing && variableUse info == Nothing) $
    lift $ fail "usage facts are not valid on this annotation"

registerUsageBinding :: UsageScope -> Ann -> Ident -> UsageValidation UsageScope
registerUsageBinding scope ann ident = do
  let info = annUsage ann
      bindingId = bindingUsageId <$> bindingUsage info
  unless (variableUse info == Nothing) $
    lift $ fail "variableUse is not valid on a binding annotation"
  traverse_ (\bid -> do
    declared <- get
    when (S.member bid declared) $ lift $ fail "duplicate usage bindingId"
    modify (S.insert bid)) bindingId
  pure (M.insert ident bindingId scope)

validateUsageBinds :: UsageScope -> [Bind Ann] -> UsageValidation UsageScope
validateUsageBinds = foldM validateBind
  where
  validateBind scope (NonRec ann ident expr) = do
    validateUsageExpr scope expr
    registerUsageBinding scope ann ident
  validateBind scope (Rec binds) = do
    scope' <- foldM (\env ((ann, ident), _) -> registerUsageBinding env ann ident) scope binds
    traverse_ (validateUsageExpr scope' . snd) binds
    pure scope'

literalChildren :: Literal a -> [a]
literalChildren (ArrayLiteral xs) = xs
literalChildren (ObjectLiteral xs) = map snd xs
literalChildren _ = []

validateUsageExpr :: UsageScope -> Expr Ann -> UsageValidation ()
validateUsageExpr scope = \case
  Literal ann lit -> do
    validatePlainAnn ann
    traverse_ (validateUsageExpr scope) (literalChildren lit)
  Constructor ann _ _ _ -> validatePlainAnn ann
  Accessor ann _ expr -> do
    validatePlainAnn ann
    validateUsageExpr scope expr
  ObjectUpdate ann expr _ fields -> do
    validatePlainAnn ann
    validateUsageExpr scope expr
    traverse_ (validateUsageExpr scope . snd) fields
  Abs ann ident body -> do
    scope' <- registerUsageBinding scope ann ident
    validateUsageExpr scope' body
  App ann function argument -> do
    validatePlainAnn ann
    validateUsageExpr scope function
    validateUsageExpr scope argument
  Var ann qualified -> do
    let info = annUsage ann
    unless (bindingUsage info == Nothing) $
      lift $ fail "bindingUsage is not valid on a variable occurrence"
    traverse_ (\usage -> do
      let target = case qualified of
            Qualified (BySourcePos _) ident -> M.lookup ident scope >>= id
            Qualified (ByModuleName _) _ -> Nothing
      unless (target == Just (variableBindingId usage)) $
        lift $ fail "variableUse does not identify its in-scope local binding") (variableUse info)
  Case ann expressions alternatives -> do
    validatePlainAnn ann
    traverse_ (validateUsageExpr scope) expressions
    traverse_ (validateUsageAlternative scope) alternatives
  Let ann binds body -> do
    validatePlainAnn ann
    scope' <- validateUsageBinds scope binds
    validateUsageExpr scope' body
  TypeApp ann expr _ -> do
    validatePlainAnn ann
    validateUsageExpr scope expr

validateUsageAlternative :: UsageScope -> CaseAlternative Ann -> UsageValidation ()
validateUsageAlternative scope (CaseAlternative binders result) = do
  scope' <- foldM validateUsageBinder scope binders
  case result of
    Right expr -> validateUsageExpr scope' expr
    Left guarded -> traverse_ (\(guard, expr) -> do
      validateUsageExpr scope' guard
      validateUsageExpr scope' expr) guarded

validateUsageBinder :: UsageScope -> Binder Ann -> UsageValidation UsageScope
validateUsageBinder scope = \case
  NullBinder ann -> validatePlainAnn ann >> pure scope
  LiteralBinder ann lit -> do
    validatePlainAnn ann
    foldM validateUsageBinder scope (literalChildren lit)
  VarBinder ann ident -> registerUsageBinding scope ann ident
  ConstructorBinder ann _ _ binders -> do
    validatePlainAnn ann
    foldM validateUsageBinder scope binders
  NamedBinder ann ident binder -> do
    scope' <- registerUsageBinding scope ann ident
    validateUsageBinder scope' binder
