module Language.PureScript.CoreFn.Usage (computeUsage) where

import Prelude

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Module (Module)
import Language.PureScript.CoreFn.Usage.Analysis qualified as Analysis

-- Recompute facts from this CoreFn and discard annotations from earlier passes.
computeUsage :: Module Ann -> Module Ann
computeUsage = Analysis.annotateUsage . fmap clearUsage
  where
  clearUsage (ss, comments, ty, meta, _) = (ss, comments, ty, meta, Nothing)
