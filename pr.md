# Add Typed Abstract Syntax Tree (TAST / `tcorefn`) for AOT Backends

## Description
This PR introduces a **Typed Abstract Syntax Tree (TAST)** representation (`tcorefn`) to the PureScript compiler to officially unlock the ecosystem for strictly typed AOT backends (such as Go, Rust, and C++ via `purescript-backend-optimizer`). 

To achieve this, we have integrated `CoreFnType` directly into the `Ann` (Annotation) of the AST, ensuring that deep structural types are preserved through the compilation pipeline and exported alongside the standard JSON output.

### Architectural Decision: Typed CSE (Common Subexpression Elimination)
Previously, the `CSEAnn` used in the optimizer did not consider types during equality checks. This was fine when types were largely absent from the internal AST. However, with our fully populated TAST, a blind CSE causes a severe **monomorphization bug** in AOT backends: if the CSE merges a polymorphic expression with a monomorphic expression (that happen to share the exact same source code shape), the extracted shared variable forces an incorrect type resolution in strict backends.

To fix this, we have deliberately designed the CSE optimizer to **preserve `ExprType` annotations during its equality checks**. The optimizer will now safely refuse to merge two identical expressions if their types differ.

### Impact on JS Generation
As a direct and intended consequence of this safer CSE, the optimizer is slightly more conservative. 
We have updated 4 Golden tests in the `Optimization examples` suite (`2866`, `Monad`, `RecursiveInstances`, `Symbols`). The generated Javascript in these edge cases leaves a few intermediate IIFEs un-merged. 

**Why this is an acceptable trade-off:**
1. The generated JS remains 100% semantically valid.
2. Production minifiers (`esbuild`, `terser`) will easily collapse these intermediate variables anyway, meaning the real-world performance/size impact on the Web ecosystem is exactly zero.
3. In exchange for this microscopic variation in raw JS shape, we provide a perfectly monomorphized, fully typed AST that empowers an entirely new generation of ultra-fast native PureScript backends.

## Related Issues
- Resolves monomorphization crashes in `purescript-backend-optimizer`.

## Checklist
- [x] Added `CoreFnType` to `Ann`.
- [x] CSE engine respects type equality to protect AOT monomorphization.
- [x] Re-generated Golden tests via `HSPEC_ACCEPT=1`.
- [x] `tcorefn` JSON generation implemented and tested.
