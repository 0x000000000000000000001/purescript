module Language.PureScript.CoreFn.Ann where

import Prelude

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Meta (Meta)
import Language.PureScript.PSString (PSString)
import Language.PureScript.Names (Qualified, ProperName, ProperNameType(..))
import Data.Text (Text)
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
  | CFUnit
  | CFAny
  | CFTypeLevelString PSString
  | CFArray CoreFnType
  | CFTypeVar Text
  | CFAdt (Qualified (ProperName 'TypeName)) [CoreFnType]
  | CFTypeApp CoreFnType [CoreFnType]
  | CFFunc [CoreFnType] CoreFnType
  | CFRow [(PSString, CoreFnType)] (Maybe CoreFnType)
  | CFRecord CoreFnType
  | CFForAll [Text] CoreFnType
  | CFConstrainedType [(Qualified (ProperName 'ClassName), [CoreFnType])] CoreFnType
  deriving (Show, Eq, Ord, Generic)

instance NFData CoreFnType

-- | Identity of one lexical binding, unique within the exported module.
-- Copies introduced by a later transformation need fresh identities.
newtype BindingId = BindingId Int
  deriving (Show, Eq, Ord, Generic)

instance NFData BindingId

-- | Facts about each dynamic instance of a lexical binding. Unknown facts are
-- represented by Nothing; neither absence nor zero implies object uniqueness.
data BindingUsage = BindingUsage
  { bindingUsageId :: BindingId
  , bindingMaxUses :: Maybe Integer
  , bindingEscapingContext :: Maybe Bool
  } deriving (Show, Eq, Ord, Generic)

instance NFData BindingUsage

-- | A proof that this occurrence has no subsequent direct use on any relevant
-- path, or an unknown result. This does not exclude aliases to the same object.
data LastLocalUse = ProvenLastLocalUse | UnknownLastLocalUse
  deriving (Show, Eq, Ord, Generic)

instance NFData LastLocalUse

data VariableUse = VariableUse
  { variableBindingId :: BindingId
  , variableLastLocalUse :: LastLocalUse
  } deriving (Show, Eq, Ord, Generic)

instance NFData VariableUse

data UsageInfo = UsageInfo
  { bindingUsage :: Maybe BindingUsage
  , variableUse :: Maybe VariableUse
  } deriving (Show, Eq, Ord, Generic)

instance NFData UsageInfo

emptyUsageInfo :: UsageInfo
emptyUsageInfo = UsageInfo Nothing Nothing

-- | Type alias for basic annotations.
type Ann = (SourceSpan, [Comment], Maybe CoreFnType, Maybe Meta, Maybe UsageInfo)

-- |
-- An annotation empty of metadata aside from a source span.
--
ssAnn :: SourceSpan -> Ann
ssAnn ss = (ss, [], Nothing, Nothing, Nothing)

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (ss, _, ty, meta, uc) = (ss, [], ty, meta, uc)
