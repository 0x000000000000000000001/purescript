# CoreFn usage facts

These annotations describe direct uses of local bindings in the exported
CoreFn. They complement the TAST types and declaration layouts. They do not prove
ownership, object uniqueness, fresh allocation, or permission to mutate memory.

## JSON representation

Binding and occurrence facts are read directly from separate annotation fields,
without a root marker or schema version. These are fragments; existing
annotation and expression fields are retained:

```json
{
  "annotation": {
    "bindingUsage": {
      "bindingId": 17,
      "maxUses": 1,
      "hasEscapingUseContext": false
    }
  }
}
```

```json
{
  "annotation": {
    "variableUse": { "bindingId": 17, "lastLocalUse": true }
  }
}
```

| Field | Meaning and valid values |
| --- | --- |
| `bindingId` | A nonnegative integer representable by Haskell `Int`. Unique per lexical binding within the module, not an object identity or a stable identifier between compilations. |
| `maxUses` | A nonnegative arbitrary-precision integer (`Integer`), bounding direct uses of each dynamic instance of this binding in the analyzed scope; `null` means unknown. |
| `hasEscapingUseContext` | `true` means a use in a syntactically classified potentially escaping context was found; `false` means none was found; `null` means unknown. This is not a heap escape or alias analysis. |
| `lastLocalUse` | `true` proves no later direct use of this binding instance after this occurrence on any relevant execution path; `null` means unproved. `false` is invalid. |

`bindingUsage` belongs on an `Abs` annotation for its parameter, a local `let`
binding annotation (including members of recursive groups), or a `VarBinder` or
`NamedBinder` annotation for its bound name. Nested pattern binders each have
their own identity. `variableUse` belongs only on a local `Var` occurrence and
identifies the binding resolved by lexical scope. Shadowed names have distinct
identities. The two blocks are not interchangeable.

Top-level declarations, exports, and global or imported references receive no
local binding identity. Their existing qualification is preserved. Absence of
local references cannot establish that an exported value has zero uses.

## Paths, instances, and conservative unknowns

A parameter's count applies to one invocation, even if its function is called
many times. A local binding's instance is created when its binding is evaluated.
Sequential direct uses contribute a sum; mutually exclusive branches permit a
maximum. These are upper bounds, not exact execution counts.

Guards may fail and continue to another guard or alternative. Their uses must
therefore account for that continuation, rather than assuming all alternatives
are mutually exclusive. A use inside a branch is not a last use when another use
can follow the enclosing `case`.

A captured outer binding can be used on repeated closure calls. Its single
textual occurrence does not establish a bound of one or a last use. Captures and
recursive repetition receive unknown facts whenever their multiplicity or
continuation is not established. Function parameters inside those bodies still
have distinct instances and may have established local bounds.

The escaping-context flag records the analysis's syntactic classification of
uses, including contexts such as returned values, arguments, and captures. It
does not summarize what a called function retains or what aliases already exist.
A conservative implementation may leave any unproved fact unknown. Zero uses
requires proof of absence; `maxUses = 1` need not prove a particular occurrence
last, and `lastLocalUse = true` says nothing about other references to the value.

For example, `let y = x in y` creates an alias: a last direct use of `x` does not
make its object exclusive. Likewise, with `data Node a = Node a`:

```purescript
child t = case t of
  Node x -> x
```

The scrutinee `t` can have one direct use outside an escaping context while its
field `x` is returned. Neither that count nor `hasEscapingUseContext = false`
permits mutation of the parent or its fields. An old tree retained by the caller
is another independent reference that this contract does not rule out.

## Compatibility and downstream transformations

Missing optional facts or `null` mean unknown, never zero or a positive proof.
A present binding or occurrence block requires a valid `bindingId`; a malformed
block, negative bound, or invalid last-use value is rejected. The reader also
checks identity uniqueness, lexical scope, and the placement of facts on the
appropriate annotations. Missing blocks provide no usage facts. Historical
`usageCount` and `escapes` fields are neither emitted nor decoded; their presence
in older JSON does not establish any fact.

Existing TAST fields retain their meaning. Consumers that ignore the new fields
may continue without these optimizations. Consumers that use them must preserve
unknowns throughout decoding and analysis. Haskell consumers of `Ann` use
`Maybe UsageInfo` in its fifth component; `UsageInfo` contains only the optional
`bindingUsage` and `variableUse` records.

These facts describe the exported CoreFn, not the result of PBO transformations.
When combining modules, retain module provenance alongside local IDs. Cloning,
inlining, and specialization require fresh IDs and consistent alpha-renaming of
their occurrences. Renaming alone does not preserve counts or last uses. Any
pass that duplicates, removes, moves, or changes evaluation of uses must justify
preservation, or invalidate and recompute the affected facts on its current IR.
Ownership, field sharing, and destructive backend reuse require separate
analyses on the backend IR; these annotations do not establish those properties.

## Validation scope and current status

The contract tests belong in `tests/TestCoreFn.hs`. Required cases cover unused,
single and repeated uses; parameters per invocation; exclusive branches; failed
guards and uses after `case`; reusable captures and recursion; shadowed names and
nested patterns; globals/imports; aliases and returned subtrees; JSON round trips,
historical input, missing facts, and invalid identities or values.

No Haskell build or test was run during this implementation pass. The user will
run `stack build` and the tests; the lot remains pending that validation.
The reader now resolves the writer's `typeTable` references
and reads `TypeApp` expressions, so typed usage round trips can be tested without
discarding those existing TAST facts.

The JSON fragments above illustrate the fields and are not captured output from
a compiler rebuilt after removing the historical fields.
