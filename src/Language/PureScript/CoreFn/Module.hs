module Language.PureScript.CoreFn.Module where

import Prelude

import Data.Map.Strict (Map)

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Expr (Bind)
import Language.PureScript.CoreFn.Ann (CoreFnType)
import Language.PureScript.Names (Ident, ModuleName, ProperName, ProperNameType(..))

data DataDecl = DataDecl
  { dataDeclName :: ProperName 'TypeName
  , dataDeclConstructors :: [DataConstructor]
  } deriving (Show)

data DataConstructor = DataConstructor
  { dataCtorName :: ProperName 'ConstructorName
  , dataCtorFields :: [CoreFnType]
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
  , moduleForeign :: [Ident]
  , moduleDecls :: [Bind a]
  , moduleDataDecls :: [DataDecl]
  } deriving (Functor, Show)
