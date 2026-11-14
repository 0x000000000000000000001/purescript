module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)
import Data.Text (Text)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Ann (CoreFnType)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.Names (Ident, ModuleName, ProperName, ProperNameType(..), Qualified)

-- | A data or newtype declaration and the fields of each constructor.
data DataDecl = DataDecl
  { dataDeclName :: ProperName 'TypeName
  , dataDeclTypeVars :: [Text]
  , dataDeclConstructors :: [DataConstructor]
  } deriving (Show)

-- | Constructor fields in declaration order.
data DataConstructor = DataConstructor
  { dataCtorName :: ProperName 'ConstructorName
  , dataCtorFields :: [CoreFnType]
  } deriving (Show)

-- | Type class parameters, superclasses, and member types.
data ClassDecl = ClassDecl
  { classDeclName :: ProperName 'ClassName
  , classDeclTypeVars :: [Text]
  , classDeclSuperclasses :: [(Qualified (ProperName 'ClassName), [CoreFnType])]
  , classDeclMethods :: [(Ident, CoreFnType)]
  } deriving (Show)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleSourceSpan :: SourceSpan
  , moduleComments :: [Comment]
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: [(a, ModuleName)]
  , moduleExports :: [Ident]
  , moduleReExports :: Map ModuleName [Ident]
  , moduleForeign :: [(a, Ident)]
  , moduleDecls :: [Bind a]
  , moduleDataDecls :: [DataDecl]
  , moduleClassDecls :: [ClassDecl]
  } deriving (Functor, Show)
