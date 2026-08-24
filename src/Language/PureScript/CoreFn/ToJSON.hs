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


type TypeTableState = State (M.Map CoreFnType Int, Int, [(Int, CoreFnType)])

internType :: CoreFnType -> TypeTableState Int
internType ty = do
  (m, nextId, table) <- get
  case M.lookup ty m of
    Just tid -> return tid
    Nothing -> do
      let tid = nextId
      put (M.insert ty tid m, nextId + 1, (tid, ty) : table)
      return tid

runTypeTable :: TypeTableState a -> (a, [Value])
runTypeTable m = 
  let (res, (_, _, table)) = runState m (M.empty, 0, [])
      -- table is accumulated in reverse order
      sortedTable = map snd $ reverse table
  in (res, map coreFnTypeToJSON sortedTable)


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

coreFnTypeToJSON :: CoreFnType -> Value
coreFnTypeToJSON CFInt = object [ "type" .= toJSON "Int" ]
coreFnTypeToJSON CFNumber = object [ "type" .= toJSON "Number" ]
coreFnTypeToJSON CFString = object [ "type" .= toJSON "String" ]
coreFnTypeToJSON CFBoolean = object [ "type" .= toJSON "Boolean" ]
coreFnTypeToJSON CFChar = object [ "type" .= toJSON "Char" ]
coreFnTypeToJSON CFUnit = object [ "type" .= toJSON "Unit" ]
coreFnTypeToJSON CFAny = object [ "type" .= toJSON "Any" ]
coreFnTypeToJSON (CFTypeLevelString s) = object [ "type" .= toJSON "TypeLevelString", "value" .= Language.PureScript.PSString.decodeStringWithReplacement s ]
coreFnTypeToJSON (CFArray ty) = object [ "type" .= toJSON "Array", "element" .= coreFnTypeToJSON ty ]
coreFnTypeToJSON (CFTypeVar name) = object [ "type" .= toJSON "TypeVar", "name" .= toJSON name ]
coreFnTypeToJSON (CFAdt qname args) =
  let parts = case qname of
                Qualified (ByModuleName (ModuleName mn')) name -> T.splitOn (T.pack ".") mn' ++ [runProperName name]
                Qualified _ name -> [runProperName name]
  in object [ "type" .= toJSON "Adt", "fqn" .= toJSON parts, "args" .= toJSON (map coreFnTypeToJSON args) ]
coreFnTypeToJSON (CFTypeApp constructor args) =
  object [ "type" .= toJSON "TypeApp", "constructor" .= coreFnTypeToJSON constructor, "args" .= toJSON (map coreFnTypeToJSON args) ]
coreFnTypeToJSON (CFFunc args ret) = 
  object [ "type" .= toJSON "Func", "args" .= toJSON (map coreFnTypeToJSON args), "ret" .= coreFnTypeToJSON ret ]
coreFnTypeToJSON (CFRow fields tailTy) = 
  let fieldToJSON (k, v) = object [ "label" .= Language.PureScript.PSString.decodeStringWithReplacement k, "type" .= coreFnTypeToJSON v ]
  in object [ "type" .= toJSON "Row", "fields" .= toJSON (map fieldToJSON fields), "tail" .= maybe (toJSON Data.Aeson.Null) coreFnTypeToJSON tailTy ]
coreFnTypeToJSON (CFRecord row) = 
  object [ "type" .= toJSON "Record", "row" .= coreFnTypeToJSON row ]
coreFnTypeToJSON (CFForAll vars body) = 
  object [ "type" .= toJSON "ForAll", "vars" .= toJSON vars, "body" .= coreFnTypeToJSON body ]
coreFnTypeToJSON (CFConstrainedType constraints body) = 
  let constraintToJSON (qname, args) = 
        let parts = case qname of
                      Qualified (ByModuleName (ModuleName mn')) name -> T.splitOn (T.pack ".") mn' ++ [runProperName name]
                      Qualified _ name -> [runProperName name]
        in object [ "fqn" .= toJSON parts, "args" .= toJSON (map coreFnTypeToJSON args) ]
  in object [ "type" .= toJSON "ConstrainedType", "constraints" .= toJSON (map constraintToJSON constraints), "body" .= coreFnTypeToJSON body ]

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
         pure (decls', foreignAnns', imports')
         
      (declsValFinal, foreignAnnsFinal, importsFinal) = res
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
  , "dataDecls"  .= map dataDeclToJSON (moduleDataDecls m)
  , "classDecls" .= map classDeclToJSON (moduleClassDecls m)
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

dataDeclToJSON :: DataDecl -> Value
dataDeclToJSON (DataDecl name typeVars ctors) = object
  [ "name" .= properNameToJSON name
  , "typeName" .= properNameToJSON name
  , "vars" .= toJSON typeVars
  , "typeVars" .= toJSON typeVars
  , "constructors" .= map dataConstructorToJSON ctors
  ]

classDeclToJSON :: ClassDecl -> Value
classDeclToJSON (ClassDecl name typeVars superclasses methods) =
  let superclassToJSON (qname, args) = 
        let parts = case qname of
                      Qualified (ByModuleName (ModuleName mn')) name' -> T.splitOn (T.pack ".") mn' ++ [runProperName name']
                      Qualified _ name' -> [runProperName name']
        in object [ "fqn" .= toJSON parts, "args" .= toJSON (map coreFnTypeToJSON args) ]
      methodToJSON (ident, ty) =
        object [ "name" .= runIdent ident, "type" .= coreFnTypeToJSON ty ]
  in object [ "name" .= runProperName name
            , "vars" .= toJSON typeVars
            , "superclasses" .= toJSON (map superclassToJSON superclasses)
            , "methods" .= toJSON (map methodToJSON methods)
            ]

dataConstructorToJSON :: DataConstructor -> Value
dataConstructorToJSON (DataConstructor name fields) = object
  [ "name" .= properNameToJSON name
  , "constructorName" .= properNameToJSON name
  , "fields" .= map coreFnTypeToJSON fields
  , "fieldTypes" .= map coreFnTypeToJSON fields
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
                                        pure $ object [ "type"          .= ("TypeApp" :: String)
                                             , "annotation"    .= annVal
                                             , "expression"    .= eVal
                                             , "typeArgument"  .= coreFnTypeToJSON t
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
