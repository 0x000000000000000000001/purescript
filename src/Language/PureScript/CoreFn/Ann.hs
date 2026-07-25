module Language.PureScript.CoreFn.Ann where

import Prelude

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Meta (Meta)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Names (Qualified, ProperName, ProperNameType(..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- |
-- Simplified type representation for CoreFn
--
data CoreFnType
  = CFInt
  | CFNumber
  | CFString
  | CFBoolean
  | CFChar
  | CFArray CoreFnType
  | CFFunc [CoreFnType] CoreFnType
  | CFRecord [(PSString, CoreFnType)]
  | CFAdt (Qualified (ProperName 'TypeName))
  | CFAny
  deriving (Show, Eq, Ord, Generic)

instance NFData CoreFnType

-- |
-- Type alias for basic annotations
--
type Ann = (SourceSpan, [Comment], Maybe CoreFnType, Maybe Meta)

-- |
-- An annotation empty of metadata aside from a source span.
--
ssAnn :: SourceSpan -> Ann
ssAnn ss = (ss, [], Nothing, Nothing)

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (ss, _, ty, meta) = (ss, [], ty, meta)
