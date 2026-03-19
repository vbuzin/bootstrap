# Layer Boundaries and External Type Quarantine

## The Layer Model

In a well-structured functional wrapper around an external dependency, code belongs to
exactly one layer. Each layer has a clear responsibility and a clear set of things it
is **forbidden** to do.

```
Domain         Pure F# types. No I/O, no external dependencies, no exceptions.
  ↓
Core           Effect machinery and shared utilities. No external dependencies.
  ↓
Runtime        Lifecycle, effect composition. No external dependencies. Uses Core + Domain.
  ↓
Infrastructure External interop ONLY. Catches exceptions, handles null, wraps external types.
  ↓
Public API     Intent-based surface. No external types visible. Uses Runtime + Domain.
```

Dependencies flow **downward only**. Infrastructure may reference Domain; Domain must
never reference Infrastructure.

---

## What Each Layer Owns

### Domain
- Validated value types (`EmailAddress`, `FilePath`, `ManifestName`)
- Discriminated unions for errors and states
- Pure record types for domain concepts
- `Result<_, DomainError>` constructors for validation

**Forbidden in Domain**: external library types, `Task`, `CancellationToken`, exceptions,
I/O of any kind.

### Core
- Effect boundary helpers (exception-safe Task gateways)
- Cancellation check utilities
- Generic effect plumbing with no knowledge of specific external dependencies

**Forbidden in Core**: external library types, domain-specific logic, any reference to
specific resources.

### Runtime
- The effect monad (e.g. a Reader+Task+Result stack)
- Computation expression builders
- Capability contracts (records of functions that run in the effect)
- Environment threading

**Forbidden in Runtime**: external library types, direct I/O, concrete adapter knowledge.

### Infrastructure
- External object construction and configuration
- Exception capture and null handling at external call sites
- Opaque handle types that wrap external objects
- Gateway modules that translate between external calls and domain types

**Required in Infrastructure**: all exception handling lives here, via guarded wrappers
(`trySync`, `tryTask`, null guards, success-flag translators).
**Forbidden in Infrastructure**: external types escaping into return types visible to
callers above this layer.

### Public API
- Opaque session/context types
- Intent-named operations (`connect`, `readDocument`, `save`)
- Per-operation `CancellationToken` with lifetime linking
- Returns `Task<Result<_, DomainError>>` — never exposes internal effect types or handles

**Forbidden in Public API**: exposing the effect monad type, capability record types,
external library types, or internal handles unless they are opaque wrapper types with
no visible external content.

---

## External Type Quarantine Rules

These rules apply to any external mutable dependency: SDKs, ORMs, HTTP clients,
message brokers, file system libraries.

### Opaque Handle Pattern
External objects must never appear in function signatures outside Infrastructure.
Wrap them in opaque types:

```fsharp
// CORRECT — external type is hidden
[<NoEquality; NoComparison>]
type ConnectionHandle = private ConnectionHandle of ExternalSdkConnection

// WRONG — external type leaks into the caller's world
let loadDocument (conn: ExternalSdkConnection) : Task<ExternalSdkDocument> = ...
```

### Exception Capture at the Boundary
Exceptions must be caught at the point of the external call and reified into typed errors.
They must never propagate into Runtime, Domain, or Public API layers.

```fsharp
// CORRECT — exception captured at interop boundary
let trySync (op: string) (f: unit -> 'T) : Result<'T, DomainError> =
    try Ok (f ())
    with ex -> Error (Interop (Exception (op, ExceptionInfo.ofException ex)))

// WRONG — exception propagates upward
let loadDocument handle path =
    let result = externalLib.Fetch(path)  // can throw — not guarded
    result
```

### Null Handling at the Boundary
External methods that can return null must be checked immediately at the call site.
Null must never propagate into domain code.

```fsharp
// CORRECT — null caught and reified at the boundary
match box externalResult with
| null -> Error (Interop (NullReturned (op, expectedThing)))
| _ -> Ok externalResult

// WRONG — null check deferred to caller
return Ok externalResult  // externalResult might be null
```

---

## SDK-Hostile-Environment Awareness

The external SDK is treated as hostile — it may silently transform, interpret, or
reject input in ways that are not immediately obvious. Domain type validation rules
must account for the SDK's implicit behaviour, not just the wrapper's own invariants.

### Silent format detection
Some SDKs detect file formats based on naming patterns. If the SDK silently treats
files with a certain suffix as a different format, the domain type that represents
filenames must **reject that suffix** with an error message that guides the caller
to the correct API.

```fsharp
// The SDK silently treats *.manifest.cdm.json as a manifest, not a plain document.
// DocumentName must reject this suffix:
let create (raw: string) =
    let normalised = raw.Trim()
    if normalised.EndsWith(".manifest.cdm.json") then
        fail "documentName" "must not use the manifest suffix — use Manifest.create" (Some normalised)
    elif normalised.EndsWith(".cdm.json") then
        Ok (DocumentName normalised)
    else
        fail "documentName" "must end with .cdm.json" (Some normalised)
```

### Silent type coercion
If the SDK accepts a value but silently coerces it to a different type (e.g. treating
a string as a number, or a relative path as absolute), the wrapper should either
validate the input to prevent the coercion, or document the coercion explicitly.

### Implicit state mutation
Some SDK operations mutate in-memory state as a side effect (e.g.
`PopulateManifestRelationshipsAsync` modifies the manifest's `Relationships`
collection). The wrapper must document these mutations clearly, even when wrapping
them in functions that return `Result<unit, _>`.

### What to flag
| Issue | Description |
|---|---|
| Domain type allows a value the SDK will silently reinterpret | Validation rules must account for SDK behaviour |
| SDK mutation not documented in the wrapper function's doc comment | Callers need to know what changes |
| SDK coercion not prevented or documented | Silent type conversion can produce unexpected results |
| Domain type validation designed without checking SDK behaviour | Ask "what will the SDK do with this input?" before finalising rules |

---

## Common Violations to Flag

| Violation | Symptom |
|---|---|
| External type in function signature above Infrastructure | SDK/ORM/client types visible outside Infrastructure |
| Unguarded external call | No `trySync` / `tryTask` wrapper around a call that can throw |
| Exception escaping interop | `try/with` missing at an external call site |
| Null propagating upward | External result used directly without a null guard |
| Domain type referencing Infrastructure | `open MyApp.Infrastructure` in a Domain file |
| Public API exposing the effect monad | Callers forced to understand `Cdm<_, _>` or equivalent |
| Capability type in Public API signature | Internal record-of-functions type visible to consumers |
| Domain type ignoring SDK format detection | SDK silently reinterprets the input (see SDK-Hostile-Environment section) |
