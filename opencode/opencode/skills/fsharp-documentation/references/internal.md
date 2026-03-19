# Internal Documentation Rules

These rules apply to infrastructure gateways, Core modules, Runtime internals,
and all private members across the codebase.

---

## The `///` vs `//` Boundary

This is the single most important rule in this file:

- **`///` (XML doc comments)** — public symbols only. They feed IntelliSense, generated
  docs, and signature files. Never use on private members.
- **`//` (line comments)** — private functions, internal explanations, SDK behaviour
  notes, and inline reasoning. They are for the maintainer reading the source.

If you find `///` on a `private` function, that is a hard violation. Change it to `//`.

---

## Infrastructure Gateway Functions

Gateway functions sit at the SDK boundary. They are the subtlest code in the library.
Documentation here targets the maintainer who needs to understand *why* a particular
SDK call is wrapped the way it is.

### Public gateway functions

Every public gateway function gets `///` covering four things:

1. **What SDK operation it wraps.** Name the SDK method or behaviour.
2. **Which `CdmError` cases it can produce and when.** Be specific — "Returns
   `Interop(NullReturned(Document))` if the corpus path does not resolve" is correct;
   "may return an error" is useless.
3. **Non-obvious SDK behaviour.** If the SDK does something surprising (returns null
   instead of throwing, silently treats one file type as another, ignores certain
   parameters), document it.
4. **Preconditions the type system does not enforce.** If a `CorpusPath` is accepted
   but must actually point to a document (not a manifest), say so.

```fsharp
/// Loads a document from the corpus by corpus path.
/// The SDK returns null rather than throwing when the path does not resolve,
/// so a null result maps to Interop(NullReturned(Document)).
/// Returns Cancelled if the token is already cancelled.
let loadDocument
    (corpusHandle : CorpusHandle)
    (path : CorpusPath)
    (token : CancellationToken)
    : Task<Result<DocumentHandle, CdmError>>
```

### Private gateway helpers

Use `//` comments. Focus on *why* the helper exists, not restating what the code does:

```fsharp
// Maps a CDM object type enum value to the corresponding DefinitionKind case.
// Returns None for object types that do not correspond to a top-level definition kind.
let private toDefinitionKind (objectType : CdmObjectType) : DefinitionKind option = ...
```

---

## SDK Behaviour Notes

When a gateway function works around a specific SDK quirk, use a `// NOTE:` comment
inline. Include enough context that a future maintainer can decide whether the
workaround is still needed:

```fsharp
// NOTE: The SDK silently treats files ending in .manifest.cdm.json as manifests,
// even when loaded via FetchObjectAsync<CdmDocumentDefinition>. DocumentName.create
// rejects this suffix to prevent silent misinterpretation.
```

```fsharp
// NOTE: CorpusPathToAdapterPath returns "" (not null) for unmounted namespaces.
// This requires a String.IsNullOrEmpty guard, not Interop.ofObj alone.
```

If a link to an SDK issue or source line is available, include it:
```fsharp
// NOTE: CreateRelativeCorpusPath is a no-op when the anchor is null.
// Verified in SDK source: https://github.com/microsoft/CDM/blob/...
```

---

## Core Layer

Core modules (`EffectBoundary`, `Interop`, combinators) are shared infrastructure
used by all gateways.

- **Every public function** gets `///` summarising what it does.
- **`EffectBoundary` functions** additionally document their error-mapping behaviour:
  which exception types map to which `CdmError` cases, and how `OperationCanceledException`
  is handled distinctly from general exceptions.
- **Private helpers** use `//`.

```fsharp
/// Wraps an async SDK call, catching OperationCanceledException as Cancelled
/// and all other exceptions as Interop(Exception).
/// Returns Interop(NullReturned(Task)) if the SDK returns a null Task.
let tryTask
    (op : string)
    (token : CancellationToken)
    (factory : CancellationToken -> Task<'T>)
    : Task<Result<'T, CdmError>>
```

---

## Runtime Layer

The `Cdm<'caps, 'T>` monad, its combinators, and the computation expression builder.

- **Every combinator** (`result`, `bind`, `map`, `apply`, `asks`, etc.) gets `///`
  describing its semantics in consumer-friendly language.
- **Do not use category-theory terminology** in summaries. "Wraps a value in a
  successful effect" is fine. "Returns the unit of the monad" is not.
- **CE builder methods** reference the corresponding combinator: "Calls `Cdm.bind`."
- **Cancellation-checking behaviour** is documented once on `Cdm.run`, not on every
  individual combinator.

---

## Private Functions (All Layers)

Private functions use `//` line comments. Keep to one or two lines. Focus on intent
and constraints, not mechanics:

**Good:**
```fsharp
// Returns Ok if every character satisfies the namespace naming rules.
let private validateCharacters (value : string) = ...
```

**Bad:**
```fsharp
// This function iterates over the characters and checks each one.
let private validateCharacters (value : string) = ...
```

The bad version restates what the code does. The good version tells you what the
function's *contract* is — the reader can see the iteration themselves.

---

## `failwith "not implemented"` Stubs

Do not comment these. The `failwith` message is the comment. When the stub is
replaced with real code, documentation will be written as part of that change.

---

## Maintenance

When modifying a function's behaviour, update its documentation in the same commit.
If you are editing a file that has incorrect or missing documentation on symbols you
are not changing, fix those too — do not leave documentation debt for later.
