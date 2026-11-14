* Retain type information and local binding usage facts in CoreFn exports.

  `corefn.json` now includes a type table, data and type class declarations,
  foreign import annotations, and explicit `TypeApp` expression nodes.
  External CoreFn consumers need to handle the new expression constructor;
  consumers of type information also need to resolve type-table references.
  The Haskell `Ann`, `Expr`, and `Module` APIs change accordingly. See
  `CORE_FN_TYPES.md` and `CORE_FN_USAGE.md` for the exported contracts.

  Common subexpression elimination preserves distinct type instantiations,
  which can also change the shape of generated JavaScript.
