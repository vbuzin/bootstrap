# Public API Documentation Rules

These rules apply to every publicly visible symbol in the codebase: types, functions,
modules, DU cases, and record companion modules. "Public" means anything that appears
in IntelliSense tooltips or generated API documentation.

---

## File Headers

Every source file starts with line 1:

```
// Layer: <LayerName> | Responsibility: <one sentence>
```

Layer names: `Domain`, `Core`, `Infrastructure`, `Runtime`, `Public API`, `Tests`.
The layer must match the file's directory. One sentence, no full stop, no line break.

**Good**: `// Layer: Infrastructure | Responsibility: Load, create, save, and inspect CDM document objects through the corpus`

**Bad**: `// Layer: Infrastructure | Responsibility: This file contains the document gateway.`

---

## XML Doc Tags — Allowed Set

Use only these tags. Do not introduce others without updating the documentation
guidelines.

| Tag | When to use |
|-----|------------|
| `///` (plain) | Summary — required on every public symbol |
| `<c>` | Inline code in prose (e.g. `<c>.cdm.json</c>`) |
| `<see cref="..."/>` | Cross-references to other types or members |
| `<param name="...">` | Only when the parameter name alone is insufficient |
| `<returns>` | Only when return semantics are non-trivial |
| `<remarks>` | Extended discussion — use sparingly, prefer the summary |
| `<example>` | Rare; public API entry points only |

**Never use**: `<exception>`, `<permission>`, `<value>`.

The `<param>` rule deserves emphasis: if a function takes `manifestHandle : ManifestHandle`,
the type is self-documenting. A `<param>` tag adds noise. Use `<param>` only when the
parameter has a non-obvious constraint the type does not encode (e.g. "the corpus path
must point to a `.cdm.json` document, not a manifest").

---

## Prose Style

- Complete sentences with full stops.
- British English: behaviour, recognised, serialisation, colour, cancelled.
- Present tense verb phrase: "Creates a…", "Returns the…", "Loads a…".
- Never start with "This function…" or "This type…".
- Refer to error cases by `CdmError` case name: "Returns `Validation` if…",
  "Returns `Interop(NullReturned(Document))` if…".
- Keep summaries under three lines. Use `<remarks>` if genuinely more is needed.

---

## Public Modules

Every public module gets a `///` summary: one or two sentences stating its purpose.
If the module groups operations on a concept, name the concept.

```fsharp
/// Operations for loading, saving, and inspecting CDM definition documents.
[<RequireQualifiedAccess>]
module Document =
```

---

## Public Functions

Every public function requires a `///` summary — one sentence stating what it does.

Additionally, document which `CdmError` cases the function can produce, directly
in the summary or immediately below it:

```fsharp
/// Loads a definition document from the corpus at the given path.
/// Returns Interop(NullReturned(Document)) if the path does not resolve to a document.
/// Returns Validation if the path's final segment is not a valid document name.
let read
    (path : CorpusPath)
    (token : CancellationToken)
    (session : Session)
    : Task<Result<DocumentHandle, CdmError>>
```

Do not use `<exception>` tags. This library communicates errors through `CdmError`,
never through exceptions.

---

## Discriminated Unions

Document both the union type and every case — even single-word cases:

```fsharp
/// Classifies the kind of definition a DefinitionEntry refers to within a CDM document.
[<RequireQualifiedAccess>]
type DefinitionKind =
    /// An entity definition.
    | Entity
    /// A trait definition.
    | Trait
    /// A data type definition.
    | DataType
```

---

## Identifier Types (Opaque Single-Field DUs)

These carry the library's most important documentation because they encode
invariants the type system alone cannot express. Document three things on the type:

1. What value it represents.
2. What invariants the private constructor guarantees.
3. The consequence for downstream code.

```fsharp
/// A validated document name guaranteed to be non-empty, trimmed, at most 256 characters,
/// and free of control characters.
/// The private constructor ensures every instance satisfies these invariants at creation
/// time; downstream code can rely on validity without re-checking.
[<Struct>]
type DocumentName = private | DocumentName of string
```

### `create` functions

Document: accepted input, trimming behaviour, and every rejection reason:

```fsharp
/// Creates a validated document name from a raw string, trimming leading and trailing whitespace.
/// Accepts any trimmed, non-empty string that is at most 256 characters and contains no control characters.
/// Returns Validation if the input is null, empty or whitespace, exceeds 256 characters,
/// or contains a control character.
let create (raw : string) : Result<DocumentName, CdmError> =
```

### `value` accessors

One-line summary, cross-referencing the type:

```fsharp
/// Extracts the raw string value from a <see cref="DocumentName"/>.
let value (DocumentName value) = value
```

---

## Records with Private Fields

Document the type-level summary and the companion module. Individual accessor
functions need only a short `///` — the type-level comment carries the weight.

```fsharp
/// Lightweight reference to a CDM manifest, holding its validated name and corpus path.
/// The private fields ensure both components are always valid domain types — callers
/// cannot construct a declaration from an arbitrary string pair and must go through
/// ManifestDeclaration.create.
type ManifestDeclaration =
    private
        { Name : ManifestName
          Path : CorpusPath }

/// Companion module for constructing and accessing ManifestDeclaration records.
[<RequireQualifiedAccess>]
module ManifestDeclaration =
    /// Creates a manifest declaration from a validated name and corpus path.
    let create (name : ManifestName) (path : CorpusPath) : ManifestDeclaration = ...

    /// Returns the manifest name component of this declaration.
    let name (doc : ManifestDeclaration) : ManifestName = ...
```

---

## What Not to Document (Public Layer)

- Obvious accessor functions — keep to one-line `///`. Do not elaborate beyond
  "Returns the X component of this Y."
- `open` statements — never comment.
- Namespace declarations — the file header is sufficient.
