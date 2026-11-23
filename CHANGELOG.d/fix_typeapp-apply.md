* Restore `apply` (`$`) and `applyFlipped` (`#`) optimization when the helper is
  wrapped in typed CoreFn `TypeApp` nodes. This restores downstream JavaScript
  optimizations, including tail-call elimination, while preserving operand
  instantiations and result types in the TAST.
