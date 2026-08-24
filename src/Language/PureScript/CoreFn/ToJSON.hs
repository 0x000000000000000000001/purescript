{-# LANGUAGE NoOverloadedStrings #-}
-- |
-- Dump the core functional representation in JSON format for consumption
-- by third-party code generators
--
module Language.PureScript.CoreFn.ToJSON
  ( moduleToJSON
  ) where

import Prelude

import Control.Arrow ((***))
import Data.Either (isLeft)
import Data.Map.Strict qualified as M
import Data.Aeson (ToJSON(..), Value(..), object)
import Data.Aeson qualified
import Data.Aeson.Key qualified
import Data.Aeson.Types (Pair)
import Data.Version (Version, showVersion)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan(..))
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Meta(..), Module(..), CoreFnType(..), DataDecl(..), DataConstructor(..), ClassDecl(..))
import Language.PureScript.Names (Ident, ModuleName(..), ProperName(..), Qualified(..), QualifiedBy(..), runIdent)
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)

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

annToJSON :: Ann -> Value
annToJSON (ss, _, mty, m) = object [ "sourceSpan"  .= sourceSpanToJSON ss
                                   , "type"        .= maybe Null coreFnTypeToJSON mty
                                   , "meta"        .= maybe Null metaToJSON m
                                   ]

literalToJSON :: (a -> Value) -> Literal a -> Value
literalToJSON _ (NumericLiteral (Left n))
  = object
    [ "literalType" .= "IntLiteral"
    , "value"       .= n
    ]
literalToJSON _ (NumericLiteral (Right n))
  = object
      [ "literalType"  .= "NumberLiteral"
      , "value"        .= n
      ]
literalToJSON _ (StringLiteral s)
  = object
    [ "literalType"  .= "StringLiteral"
    , "value"        .= s
    ]
literalToJSON _ (CharLiteral c)
  = object
    [ "literalType"  .= "CharLiteral"
    , "value"        .= c
    ]
literalToJSON _ (BooleanLiteral b)
  = object
    [ "literalType"  .= "BooleanLiteral"
    , "value"        .= b
    ]
literalToJSON t (ArrayLiteral xs)
  = object
    [ "literalType"  .= "ArrayLiteral"
    , "value"        .= map t xs
    ]
literalToJSON t (ObjectLiteral xs)
  = object
    [ "literalType"    .= "ObjectLiteral"
    , "value"          .= recordToJSON t xs
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
moduleToJSON v m = object
  [ "sourceSpan" .= sourceSpanToJSON (moduleSourceSpan m)
  , "moduleName" .= moduleNameToJSON (moduleName m)
  , "modulePath" .= toJSON (modulePath m)
  , "imports"    .= map importToJSON (moduleImports m)
  , "exports"    .= map identToJSON (moduleExports m)
  , "reExports"  .= reExportsToJSON (moduleReExports m)
  , "foreign"    .= map (identToJSON . snd) (moduleForeign m)
  , "foreignAnnotations" .= object (map (\(ann, ident) -> T.unpack (Language.PureScript.Names.runIdent ident) .= annToJSON ann) (moduleForeign m))
  , "decls"      .= map bindToJSON (moduleDecls m)
  , "dataDecls"  .= map dataDeclToJSON (moduleDataDecls m)
  , "classDecls" .= map classDeclToJSON (moduleClassDecls m)
  , "builtWith"  .= toJSON (showVersion v)
  , "comments"   .= map toJSON (moduleComments m)
  ]

  where
  importToJSON (ann,mn) = object
    [ "annotation" .= annToJSON ann
    , "moduleName" .= moduleNameToJSON mn
    ]

  reExportsToJSON :: M.Map ModuleName [Ident] -> Value
  reExportsToJSON = toJSON . M.map (map runIdent)



bindToJSON :: Bind Ann -> Value
bindToJSON (NonRec ann n e)
  = object
    [ "bindType"   .= "NonRec"
    , "annotation" .= annToJSON ann
    , "identifier" .= identToJSON n
    , "expression" .= exprToJSON e
    ]
bindToJSON (Rec bs)
  = object
    [ "bindType"   .= "Rec"
    , "binds"      .= map (\((ann, n), e)
                                  -> object
                                      [ "identifier"  .= identToJSON n
                                      , "annotation"   .= annToJSON ann
                                      , "expression"   .= exprToJSON e
                                       ]) bs
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

recordToJSON :: (a -> Value) -> [(PSString, a)] -> Value
recordToJSON f = toJSON . map (toJSON *** f)

exprToJSON :: Expr Ann -> Value
exprToJSON (Var ann i)              = object [ "type"        .= toJSON "Var"
                                             , "annotation"  .= annToJSON ann
                                             , "value"       .= qualifiedToJSON runIdent i
                                             ]
exprToJSON (Literal ann l)          = object [ "type"        .= "Literal"
                                             , "annotation"  .= annToJSON ann
                                             , "value"       .=  literalToJSON exprToJSON l
                                             ]
exprToJSON (Constructor ann d c is) = object [ "type"        .= "Constructor"
                                             , "annotation"  .= annToJSON ann
                                             , "typeName"    .= properNameToJSON d
                                             , "name"        .= properNameToJSON c
                                             , "constructorName" .= properNameToJSON c
                                             , "fields"      .= map identToJSON is
                                             , "fieldNames"  .= map identToJSON is
                                             ]
exprToJSON (Accessor ann f r)       = object [ "type"        .= "Accessor"
                                             , "annotation"  .= annToJSON ann
                                             , "fieldName"   .= f
                                             , "expression"  .= exprToJSON r
                                             ]
exprToJSON (ObjectUpdate ann r copy fs)
                                    = object [ "type"        .= "ObjectUpdate"
                                             , "annotation"  .= annToJSON ann
                                             , "expression"  .= exprToJSON r
                                             , "copy"        .= toJSON copy
                                             , "updates"     .= recordToJSON exprToJSON fs
                                             ]
exprToJSON (Abs ann p b)            = object [ "type"        .= "Abs"
                                             , "annotation"  .= annToJSON ann
                                             , "argument"    .= identToJSON p
                                             , "body"        .= exprToJSON b
                                             ]
exprToJSON (App ann f x)            = object [ "type"        .= "App"
                                             , "annotation"  .= annToJSON ann
                                             , "abstraction" .= exprToJSON f
                                             , "argument"    .= exprToJSON x
                                             ]
exprToJSON (TypeApp ann e t)        = object [ "type"          .= ("TypeApp" :: String)
                                             , "annotation"    .= annToJSON ann
                                             , "expression"    .= exprToJSON e
                                             , "typeArgument"  .= coreFnTypeToJSON t
                                             ]
exprToJSON (Case ann ss cs)         = object [ "type"        .= "Case"
                                             , "annotation"  .= annToJSON ann
                                             , "caseExpressions"
                                                                    .= map exprToJSON ss
                                             , "caseAlternatives"
                                                                    .= map caseAlternativeToJSON cs
                                             ]
exprToJSON (Let ann bs e)           = object [ "type"        .= "Let" 
                                             , "annotation"  .= annToJSON ann
                                             , "binds"       .= map bindToJSON bs
                                             , "expression"  .= exprToJSON e
                                             ]

caseAlternativeToJSON :: CaseAlternative Ann -> Value
caseAlternativeToJSON (CaseAlternative bs r') =
  let isGuarded = isLeft r'
  in object
      [ "binders"     .= toJSON (map binderToJSON bs)
      , "isGuarded"   .= toJSON isGuarded
      , (if isGuarded then "expressions" else "expression")
         .= case r' of
             Left rs -> toJSON $ map (\(g, e) -> object [ "guard" .= exprToJSON g, "expression" .= exprToJSON e]) rs
             Right r -> exprToJSON r
      ]

binderToJSON :: Binder Ann -> Value
binderToJSON (VarBinder ann v)              = object [ "binderType"  .= "VarBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "identifier"  .= identToJSON v
                                                     ]
binderToJSON (NullBinder ann)               = object [ "binderType"  .= "NullBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     ]
binderToJSON (LiteralBinder ann l)          = object [ "binderType"  .= "LiteralBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "literal"     .= literalToJSON binderToJSON l
                                                     ]
binderToJSON (ConstructorBinder ann d c bs) = object [ "binderType"  .= "ConstructorBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "typeName"    .= qualifiedToJSON runProperName d
                                                     , "name"        .= qualifiedToJSON runProperName c
                                                     , "constructorName" .= qualifiedToJSON runProperName c
                                                     , "binders"     .= map binderToJSON bs
                                                     ]
binderToJSON (NamedBinder ann n b)          = object [ "binderType"  .= "NamedBinder"
                                                     , "annotation"  .= annToJSON ann
                                                     , "identifier"  .= identToJSON n
                                                     , "binder"      .= binderToJSON b
                                                     ]
