{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude

import Control.Arrow ()
import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Aeson (ToJSON(..), Value(..), object)
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.Types (Pair)
import Data.Version (Version, showVersion)
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.State

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan(..))
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Meta(..), Module(..), CoreFnType(..), DataDecl(..), DataConstructor(..), ClassDecl(..))
import Language.PureScript.Names (Ident, ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..), runIdent)
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)


type TypeTableState = State (M.Map CoreFnType Int, Int, [(Int, Value)])

internType :: CoreFnType -> TypeTableState Int
internType ty = do
  (m, nextId, table) <- get
  case M.lookup ty m of
    Just tid -> return tid
    Nothing -> do
      let tid = nextId
      put (M.insert ty tid m, nextId + 1, table)
      val <- coreFnTypeToJSON ty
      (m', nextId', table') <- get
      put (m', nextId', (tid, val) : table')
      return tid

runTypeTable :: TypeTableState a -> (a, [Value])
runTypeTable m = 
  let (res, (_, _, table)) = runState m (M.empty, 0, [])
      -- table is accumulated in reverse order
      sortedTable = map snd $ reverse table
  in (res, sortedTable)


constructorTypeToJSON :: ConstructorType -> Value
constructorTypeToJSON ProductType = toJSON "ProductType"
constructorTypeToJSON SumType = toJSON "SumType"

infixr 8 .=
(.=) :: ToJSON a => String -> a -> Pair
key .= value = Data.Aeson.Key.fromString key Data.Aeson..= value

metaToJSON :: Meta -> Value
metaToJSON (IsConstructor t is)
  = object
    [ "metaType"         .= "IsConstructor"
    , "constructorType"  .= constructorTypeToJSON t
    , "identifiers"      .= identToJSON `map` is
    ]
metaToJSON IsNewtype              = object [ "metaType"  .= "IsNewtype" ]
metaToJSON IsTypeClassConstructor = object [ "metaType"  .= "IsTypeClassConstructor" ]
metaToJSON IsForeign              = object [ "metaType"  .= "IsForeign" ]
metaToJSON IsWhere                = object [ "metaType"  .= "IsWhere" ]
metaToJSON IsSyntheticApp         = object [ "metaType"  .= "IsSyntheticApp" ]

sourceSpanToJSON :: SourceSpan -> Value
sourceSpanToJSON (SourceSpan _ spanStart spanEnd) =
  object [ "start" .= spanStart
         , "end"   .= spanEnd
         ]

coreFnTypeToJSON :: CoreFnType -> TypeTableState Value
coreFnTypeToJSON CFInt = pure $ object [ "type" .= toJSON "Int" ]
coreFnTypeToJSON CFNumber = pure $ object [ "type" .= toJSON "Number" ]
coreFnTypeToJSON CFString = pure $ object [ "type" .= toJSON "String" ]
coreFnTypeToJSON CFBoolean = pure $ object [ "type" .= toJSON "Boolean" ]
coreFnTypeToJSON CFChar = pure $ object [ "type" .= toJSON "Char" ]
coreFnTypeToJSON CFUnit = pure $ object [ "type" .= toJSON "Unit" ]
coreFnTypeToJSON CFAny = pure $ object [ "type" .= toJSON "Any" ]
coreFnTypeToJSON (CFTypeLevelString s) = pure $ object [ "type" .= toJSON "TypeLevelString", "value" .= Language.PureScript.PSString.decodeStringWithReplacement s ]
coreFnTypeToJSON (CFArray ty) = do
  idTy <- internType ty
  pure $ object [ "type" .= toJSON "Array", "element" .= idTy ]
coreFnTypeToJSON (CFTypeVar name) = pure $ object [ "type" .= toJSON "TypeVar", "name" .= toJSON name ]
coreFnTypeToJSON (CFAdt qname args) = do
  let parts = case qname of
                Qualified (ByModuleName (ModuleName mn')) name -> T.splitOn (T.pack ".") mn' ++ [runProperName name]
                Qualified _ name -> [runProperName name]
  args' <- mapM internType args
  pure $ object [ "type" .= toJSON "Adt", "fqn" .= toJSON parts, "args" .= args' ]
coreFnTypeToJSON (CFTypeApp constructor args) = do
  cId <- internType constructor
  aIds <- mapM internType args
  pure $ object [ "type" .= toJSON "TypeApp", "constructor" .= cId, "args" .= aIds ]
coreFnTypeToJSON (CFFunc args ret) = do
  aIds <- mapM internType args
  rId <- internType ret
  pure $ object [ "type" .= toJSON "Func", "args" .= aIds, "ret" .= rId ]
coreFnTypeToJSON (CFRow fields tailTy) = do
  fields' <- mapM (\(k, v) -> do
      vId <- internType v
      pure $ object [ "label" .= Language.PureScript.PSString.decodeStringWithReplacement k, "type" .= vId ]
    ) fields
  tailId <- case tailTy of
    Nothing -> pure Data.Aeson.Null
    Just t -> toJSON <$> internType t
  pure $ object [ "type" .= toJSON "Row", "fields" .= fields', "tail" .= tailId ]
coreFnTypeToJSON (CFRecord row) = do
  rId <- internType row
  pure $ object [ "type" .= toJSON "Record", "row" .= rId ]
coreFnTypeToJSON (CFForAll vars body) = do
  bId <- internType body
  pure $ object [ "type" .= toJSON "ForAll", "vars" .= toJSON vars, "body" .= bId ]
coreFnTypeToJSON (CFConstrainedType constraints body) = do
  let constraintToJSON (qname, args) = do
        let parts = case qname of
                      Qualified (ByModuleName (ModuleName mn')) name -> T.splitOn (T.pack ".") mn' ++ [runProperName name]
                      Qualified _ name -> [runProperName name]
        args' <- mapM internType args
        pure $ object [ "fqn" .= toJSON parts, "args" .= args' ]
  constraints' <- mapM constraintToJSON constraints
  bId <- internType body
  pure $ object [ "type" .= toJSON "ConstrainedType", "constraints" .= constraints', "body" .= bId ]

annToJSON :: Ann -> TypeTableState Value
annToJSON (ss, _, mty, m) = do
  typeVal <- case mty of
    Nothing -> pure Null
    Just ty -> toJSON <$> internType ty
  pure $ object [ "sourceSpan"  .= sourceSpanToJSON ss
                , "type"        .= typeVal
                , "meta"        .= maybe Null metaToJSON m
                ]

literalToJSON :: (a -> TypeTableState Value) -> Literal a -> TypeTableState Value
literalToJSON _ (NumericLiteral (Left n))
  = pure $ object
    [ "literalType" .= "IntLiteral"
    , "value"       .= n
    ]
literalToJSON _ (NumericLiteral (Right n))
  = pure $ object
      [ "literalType"  .= "NumberLiteral"
      , "value"        .= n
      ]
literalToJSON _ (StringLiteral s)
  = pure $ object
    [ "literalType"  .= "StringLiteral"
    , "value"        .= s
    ]
literalToJSON _ (CharLiteral c)
  = pure $ object
    [ "literalType"  .= "CharLiteral"
    , "value"        .= c
    ]
literalToJSON _ (BooleanLiteral b)
  = pure $ object
    [ "literalType"  .= "BooleanLiteral"
    , "value"        .= b
    ]
literalToJSON t (ArrayLiteral xs)
  = do
    xs' <- mapM t xs
    pure $ object
      [ "literalType"  .= "ArrayLiteral"
      , "value"        .= xs'
      ]
literalToJSON t (ObjectLiteral xs)
  = do
    xs' <- recordToJSON t xs
    pure $ object
      [ "literalType"    .= "ObjectLiteral"
      , "value"          .= xs'
      ]

identToJSON :: Ident -> Value
identToJSON = toJSON . runIdent

properNameToJSON :: ProperName a -> Value
properNameToJSON = toJSON . runProperName

qualifiedToJSON :: (a -> Text) -> Qualified a -> Value
qualifiedToJSON f (Qualified qb a) =
  case qb of
    ByModuleName mn -> object
      [ "moduleName" .= moduleNameToJSON mn
      , "identifier" .= toJSON (f a)
      ]
    BySourcePos ss -> object
      [ "sourcePos"  .= toJSON ss
      , "identifier" .= toJSON (f a)
      ]

moduleNameToJSON :: ModuleName -> Value
moduleNameToJSON (ModuleName name) = toJSON (T.splitOn (T.pack ".") name)

moduleToJSON :: Version -> Module Ann -> Value
moduleToJSON v m = 
  let (res, typeTableFinal) = runTypeTable $ do
         decls' <- mapM bindToJSON (moduleDecls m)
         foreignAnns' <- mapM (\(ann, ident) -> do
            annVal <- annToJSON ann
            pure (T.unpack (Language.PureScript.Names.runIdent ident) .= annVal)
          ) (moduleForeign m)
         imports' <- mapM (\(ann, mn) -> do
            annVal <- annToJSON ann
            pure $ object [ "annotation" .= annVal, "moduleName" .= moduleNameToJSON mn ]
          ) (moduleImports m)
         dataDecls' <- mapM dataDeclToJSON (moduleDataDecls m)
         classDecls' <- mapM classDeclToJSON (moduleClassDecls m)
         pure (decls', foreignAnns', imports', dataDecls', classDecls')
         
      (declsValFinal, foreignAnnsFinal, importsFinal, dataDeclsFinal, classDeclsFinal) = res
  in object
  [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
  , "moduleName" .= moduleNameToJSON (moduleName m)
  , "modulePath" .= toJSON (modulePath m)
  , "imports"    .= importsFinal
  , "exports"    .= map identToJSON (moduleExports m)
  , "reExports"  .= reExportsToJSON (moduleReExports m)
  , "foreign"    .= map (identToJSON . snd) (moduleForeign m)
  , "foreignAnnotations" .= object foreignAnnsFinal
  , "decls"      .= declsValFinal
  , "dataDecls"  .= dataDeclsFinal
  , "classDecls" .= classDeclsFinal
  , "builtWith"  .= toJSON (showVersion v)
  , "comments"   .= map toJSON (moduleComments m)
  , "typeTable"  .= toJSON typeTableFinal
  ]

  where
  reExportsToJSON :: M.Map ModuleName [Ident] -> Value
  reExportsToJSON = toJSON . M.map (map runIdent)



bindToJSON :: Bind Ann -> TypeTableState Value
bindToJSON (NonRec ann n e)
  = do
    annVal <- annToJSON ann
    eVal <- exprToJSON e
    pure $ object
      [ "bindType"   .= "NonRec"
      , "annotation" .= annVal
      , "identifier" .= identToJSON n
      , "expression" .= eVal
      ]
bindToJSON (Rec bs)
  = do
    bsVal <- mapM (\((ann, n), e) -> do
                      annVal <- annToJSON ann
                      eVal <- exprToJSON e
                      pure $ object
                        [ "identifier"  .= identToJSON n
                        , "annotation"   .= annVal
                        , "expression"   .= eVal
                        ]) bs
    pure $ object
      [ "bindType"   .= "Rec"
      , "binds"      .= bsVal
      ]

dataDeclToJSON :: DataDecl -> TypeTableState Value
dataDeclToJSON (DataDecl name typeVars ctors) = do
  ctors' <- mapM dataConstructorToJSON ctors
  pure $ object
    [ "name" .= properNameToJSON name
    , "typeName" .= properNameToJSON name
    , "vars" .= toJSON typeVars
    , "typeVars" .= toJSON typeVars
    , "constructors" .= ctors'
    ]

classDeclToJSON :: ClassDecl -> TypeTableState Value
classDeclToJSON (ClassDecl name typeVars superclasses methods) = do
  let superclassToJSON (qname, args) = do
        let parts = case qname of
                      Qualified (ByModuleName (ModuleName mn')) name' -> T.splitOn (T.pack ".") mn' ++ [runProperName name']
                      Qualified _ name' -> [runProperName name']
        args' <- mapM (\t -> toJSON <$> internType t) args
        pure $ object [ "fqn" .= toJSON parts, "args" .= args' ]
  superclasses' <- mapM superclassToJSON superclasses
  let methodToJSON (ident, ty) = do
        ty' <- toJSON <$> internType ty
        pure $ object [ "name" .= runIdent ident, "type" .= ty' ]
  methods' <- mapM methodToJSON methods
  pure $ object [ "name" .= runProperName name
                , "vars" .= toJSON typeVars
                , "superclasses" .= superclasses'
                , "methods" .= methods'
                ]

dataConstructorToJSON :: DataConstructor -> TypeTableState Value
dataConstructorToJSON (DataConstructor name fields) = do
  fields' <- mapM (\t -> toJSON <$> internType t) fields
  pure $ object
    [ "name" .= properNameToJSON name
    , "constructorName" .= properNameToJSON name
    , "fields" .= fields'
    , "fieldTypes" .= fields'
    ]

recordToJSON :: (a -> TypeTableState Value) -> [(PSString, a)] -> TypeTableState Value
recordToJSON f xs = do
  xs' <- mapM (\(k, v) -> do
    v' <- f v
    pure (toJSON (Language.PureScript.PSString.decodeStringWithReplacement k), v')) xs
  -- The original code did: toJSON . map (toJSON *** f)
  -- Data.Aeson's ToJSON for (a,b) emits an array [a,b] if it's not text keys, but for String keys it might emit an object? 
  -- No, recordToJSON emitted an array of tuples in the original.
  -- Wait, original was: recordToJSON f = toJSON . map (toJSON *** f)
  -- toJSON on PSString converts it to Value (probably String).
  -- So `(toJSON k, v')` is a tuple `(Value, Value)`. `toJSON` on a list of tuples `[(Value, Value)]` makes a `[[Value, Value]]`.
  pure $ toJSON xs'


exprToJSON :: Expr Ann -> TypeTableState Value
exprToJSON (Var ann i)              = do
                                        annVal <- annToJSON ann
                                        pure $ object [ "type"        .= toJSON "Var"
                                             , "annotation"  .= annVal
                                             , "value"       .= qualifiedToJSON runIdent i
                                             ]
exprToJSON (Literal ann l)          = do
                                        annVal <- annToJSON ann
                                        lVal <- literalToJSON exprToJSON l
                                        pure $ object [ "type"        .= "Literal"
                                             , "annotation"  .= annVal
                                             , "value"       .= lVal
                                             ]
exprToJSON (Constructor ann d c is) = do
                                        annVal <- annToJSON ann
                                        pure $ object [ "type"        .= "Constructor"
                                             , "annotation"  .= annVal
                                             , "typeName"    .= properNameToJSON d
                                             , "name"        .= properNameToJSON c
                                             , "constructorName" .= properNameToJSON c
                                             , "fields"      .= map identToJSON is
                                             , "fieldNames"  .= map identToJSON is
                                             ]
exprToJSON (Accessor ann f r)       = do
                                        annVal <- annToJSON ann
                                        rVal <- exprToJSON r
                                        pure $ object [ "type"        .= "Accessor"
                                             , "annotation"  .= annVal
                                             , "fieldName"   .= f
                                             , "expression"  .= rVal
                                             ]
exprToJSON (ObjectUpdate ann r copy fs)
                                    = do
                                        annVal <- annToJSON ann
                                        rVal <- exprToJSON r
                                        fsVal <- recordToJSON exprToJSON fs
                                        pure $ object [ "type"        .= "ObjectUpdate"
                                             , "annotation"  .= annVal
                                             , "expression"  .= rVal
                                             , "copy"        .= toJSON copy
                                             , "updates"     .= fsVal
                                             ]
exprToJSON (Abs ann p b)            = do
                                        annVal <- annToJSON ann
                                        bVal <- exprToJSON b
                                        pure $ object [ "type"        .= "Abs"
                                             , "annotation"  .= annVal
                                             , "argument"    .= identToJSON p
                                             , "body"        .= bVal
                                             ]
exprToJSON (App ann f x)            = do
                                        annVal <- annToJSON ann
                                        fVal <- exprToJSON f
                                        xVal <- exprToJSON x
                                        pure $ object [ "type"        .= "App"
                                             , "annotation"  .= annVal
                                             , "abstraction" .= fVal
                                             , "argument"    .= xVal
                                             ]
exprToJSON (TypeApp ann e t)        = do
                                        annVal <- annToJSON ann
                                        eVal <- exprToJSON e
                                        tIdx <- toJSON <$> internType t
                                        pure $ object [ "type"          .= ("TypeApp" :: String)
                                             , "annotation"    .= annVal
                                             , "expression"    .= eVal
                                             , "typeArgument"  .= tIdx
                                             ]
exprToJSON (Case ann ss cs)         = do
                                        annVal <- annToJSON ann
                                        ssVal <- mapM exprToJSON ss
                                        csVal <- mapM caseAlternativeToJSON cs
                                        pure $ object [ "type"        .= "Case"
                                             , "annotation"  .= annVal
                                             , "caseExpressions" .= ssVal
                                             , "caseAlternatives" .= csVal
                                             ]
exprToJSON (Let ann bs e)           = do
                                        annVal <- annToJSON ann
                                        bsVal <- mapM bindToJSON bs
                                        eVal <- exprToJSON e
                                        pure $ object [ "type"        .= "Let" 
                                             , "annotation"  .= annVal
                                             , "binds"       .= bsVal
                                             , "expression"  .= eVal
                                             ]

caseAlternativeToJSON :: CaseAlternative Ann -> TypeTableState Value
caseAlternativeToJSON (CaseAlternative bs r') = do
  bsVal <- mapM binderToJSON bs
  let isGuarded = isLeft r'
  exprsVal <- case r' of
               Left rs -> do
                 rs' <- mapM (\(g, e) -> do
                                gVal <- exprToJSON g
                                eVal <- exprToJSON e
                                pure $ object [ "guard" .= gVal, "expression" .= eVal ]
                             ) rs
                 pure $ toJSON rs'
               Right r -> exprToJSON r
  pure $ object
      [ "binders"     .= bsVal
      , "isGuarded"   .= toJSON isGuarded
      , (if isGuarded then "expressions" else "expression") .= exprsVal
      ]

binderToJSON :: Binder Ann -> TypeTableState Value
binderToJSON (VarBinder ann v)              = do
                                                annVal <- annToJSON ann
                                                pure $ object [ "binderType"  .= "VarBinder"
                                                     , "annotation"  .= annVal
                                                     , "identifier"  .= identToJSON v
                                                     ]
binderToJSON (NullBinder ann)               = do
                                                annVal <- annToJSON ann
                                                pure $ object [ "binderType"  .= "NullBinder"
                                                     , "annotation"  .= annVal
                                                     ]
binderToJSON (LiteralBinder ann l)          = do
                                                annVal <- annToJSON ann
                                                lVal <- literalToJSON binderToJSON l
                                                pure $ object [ "binderType"  .= "LiteralBinder"
                                                     , "annotation"  .= annVal
                                                     , "literal"     .= lVal
                                                     ]
binderToJSON (ConstructorBinder ann d c bs) = do
                                                annVal <- annToJSON ann
                                                bsVal <- mapM binderToJSON bs
                                                pure $ object [ "binderType"  .= "ConstructorBinder"
                                                     , "annotation"  .= annVal
                                                     , "typeName"    .= qualifiedToJSON runProperName d
                                                     , "name"        .= qualifiedToJSON runProperName c
                                                     , "constructorName" .= qualifiedToJSON runProperName c
                                                     , "binders"     .= bsVal
                                                     ]
binderToJSON (NamedBinder ann n b)          = do
                                                annVal <- annToJSON ann
                                                bVal <- binderToJSON b
                                                pure $ object [ "binderType"  .= "NamedBinder"
                                                     , "annotation"  .= annVal
                                                     , "identifier"  .= identToJSON n
                                                     , "binder"      .= bVal
                                                     ]
