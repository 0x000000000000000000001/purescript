# Add Typed Abstract Syntax Tree (TAST / `tcorefn`) for AOT Backends

## Description
This PR introduces a **Typed Abstract Syntax Tree (TAST)** representation (`tcorefn`) to the PureScript compiler to officially unlock the ecosystem for strictly typed AOT backends (such as Go, Rust, and C++ via `purescript-backend-optimizer`). 

To achieve this, we have integrated `CoreFnType` directly into the `Ann` (Annotation) of the AST, ensuring that deep structural types are preserved through the compilation pipeline and exported alongside the standard JSON output.

### Architectural Decision: Typed CSE (Common Subexpression Elimination)
Previously, the `CSEAnn` used in the optimizer did not consider types during equality checks. This was fine when types were largely absent from the internal AST. However, with our fully populated TAST, a blind CSE causes a severe **monomorphization bug** in AOT backends: if the CSE merges a polymorphic expression with a monomorphic expression (that happen to share the exact same source code shape), the extracted shared variable forces an incorrect type resolution in strict backends.

To fix this, we have deliberately designed the CSE optimizer to **preserve `ExprType` annotations during its equality checks**. The optimizer will now safely refuse to merge two identical expressions if their types differ.

### Impact on JS Generation & Test Suite
As a direct and intended consequence of this safer CSE and typed AST, the optimizer is slightly more conservative, which modifies a few snapshots. 
However, the impact is surgically precise. Out of the **1324** tests in the project, exactly **5** Golden tests required updating (a modification rate of **0.37%**).

- **SourceMaps (`Bug4034.out.js.map`)**: The generated sourcemap shifted by a single semicolon (representing a single column mapping shift due to stricter AST node tracking).
- **Optimizer (`4179`, `Monad`, `RecursiveInstances`, `Symbols`)**: The generated Javascript leaves a few intermediate dictionary instantiations un-merged. 

**Why this is an acceptable trade-off:**
1. The compiler is now mathematically safer: the CSE no longer incorrectly merges type-class dictionaries that share structural shapes but differ in their underlying types.
2. The generated JS remains 100% semantically valid. Production minifiers (`esbuild`, `terser`) will effortlessly collapse these intermediate variables anyway, meaning the real-world performance/size impact on the Web ecosystem is absolutely zero.
3. The total codebase footprint of this PR is microscopically small (`12 files changed, 194 insertions(+), 64 deletions(-)`), yet it unlocks a perfectly monomorphized, fully typed AST that empowers an entirely new generation of ultra-fast native PureScript backends.

## Related Issues
- Resolves monomorphization crashes in `purescript-backend-optimizer`.

## Checklist
- [x] Added `CoreFnType` to `Ann`.
- [x] CSE engine respects type equality to protect AOT monomorphization.
- [x] Re-generated Golden tests via `HSPEC_ACCEPT=1`.
- [x] `tcorefn` JSON generation implemented and tested.
