# Typed CoreFn

The CoreFn export retains type information for backends that need concrete
representations, constructor fields, and polymorphic instantiations. These
extensions are written to `corefn.json` by `--codegen corefn`; there is no
separate code generation target or output file named `tcorefn`.

## Types and annotations

An annotation's optional `type` field refers to a zero-based entry in the
module's `typeTable`. Entries are objects tagged by `type`, and nested types
also use indices into the same table. Indices are local to a module and may
refer forward to later entries. They are not stable between compilations.

For example, this table represents `Int -> Int` at index 0:

```json
[
  { "type": "Func", "args": [1], "ret": 1 },
  { "type": "Int" }
]
```

The type representation includes primitives, arrays, functions, named types,
type applications, type variables, type-level strings, records, rows,
quantifiers, and constraints. `ForAll` retains its ordered `vars` and `body`;
`ConstrainedType` retains its `constraints` and `body`. A fully qualified name
is represented as module components followed by the name, for example
`["Data", "Maybe", "Maybe"]`.

Rows retain field order as an array of `{ "label": ..., "type": ... }` objects.
A closed row has a `null` tail; an open row's tail refers to another type.
Labels and type-level strings use the existing `PSString` encoding, including
UTF-16 code-unit arrays for strings containing unpaired surrogates.

This is a simplified type representation, not the type checker's full type
language. A missing or `null` annotation type is unknown; the `Any` type also
represents information that simplification cannot retain. Neither is evidence
that a value has a particular native representation. Consumers must account
for these cases rather than infer a layout from an absent type.

## Instantiations and foreign imports

A CoreFn `TypeApp` expression records a polymorphic instantiation. Its
`expression` is the function being instantiated and its `typeArgument` is a
type-table index. Its annotation describes the result of that instantiation.
These nodes represent both explicit type applications and instantiations
introduced during type checking. They do not apply a runtime argument;
JavaScript generation erases them.

The existing `foreign` array continues to list imported identifiers.
`foreignAnnotations` maps those identifiers to annotations containing their
declared types, including quantifiers. Foreign contracts must be interpreted
according to the target backend's calling convention; type metadata alone
does not establish purity, ownership, or freedom from aliasing.

## Declarations

`dataDecls` describes local data and newtype declarations. Each declaration
contains its name, type parameters, and constructors with their field types
in declaration order. The exported compatibility aliases are `name`/`typeName`,
`vars`/`typeVars`, constructor `name`/`constructorName`, and
`fields`/`fieldTypes`. Each pair carries the same information.

`classDecls` describes local type classes: `name`, type parameters in `vars`,
`superclasses` with their type arguments, and `methods` with their names and
types. These declarations expose the source-level structure; a backend still
chooses its runtime layout and representation.

## Compatibility

Readers of earlier CoreFn exports must handle the additional `TypeApp`
expression constructor. Consumers which use type information must also
resolve type-table references. The Haskell reader accepts legacy modules
without the additional metadata and inline type objects as well as indexed
types. Malformed type references and unrecognized type tags are rejected.

The Haskell API adds `CoreFnType` and optional usage facts to `Ann`, annotates
`moduleForeign`, and adds data and class declarations to `Module`. Code that
constructs or pattern matches these types needs corresponding updates.

Common subexpression elimination compares type annotations to avoid sharing
expressions whose instantiations differ. This can change the generated
JavaScript even when no CoreFn export is requested.

Local binding usage facts have a separate, conservative contract documented in
[CORE_FN_USAGE.md](CORE_FN_USAGE.md). Transformations must preserve or recompute
those facts before using them for optimization.
