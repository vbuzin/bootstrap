# Error Handling Discipline

## The Single Failure Channel Rule

Every operation that can fail returns `Result<'ok, DomainError>` or
`Task<Result<'ok, DomainError>>`. There is **one** failure channel. Not two.
Not "Result for domain errors and exceptions for unexpected ones". One.

This rule applies at every layer boundary. Exceptions are an implementation detail of
the Infrastructure layer and must never be observable above it.

---

## Error Taxonomy

A well-structured error DU for a project that wraps an external dependency typically
covers these distinct categories:

| Case | Meaning | Detected in |
|---|---|---|
| `Validation` | Invalid caller input (field, path, name) | Domain constructors |
| `Interop(Exception)` | External library threw an exception | Infrastructure, via `trySync`/`tryTask` |
| `Interop(NullReturned)` | External library returned null where a value was expected | Infrastructure, via null guard |
| `Interop(FalseReturned)` | External library returned a false success flag | Infrastructure, via `expectTrue` |
| `NotFound` | Expected resource is absent | Infrastructure translation or runtime lookup |
| `Lifecycle` | Operation invalid for current state | Runtime workflow checks |
| `Conflict` | Deterministic resource conflict (duplicate, overwrite denied) | Runtime or interop pre-check |
| `Cancelled` | Cancellation was observed | Core boundary or runtime |

Adapt the taxonomy to the domain — but every case should carry enough metadata for the
caller to diagnose what went wrong and where.

### What to flag
- `raise` used for a domain problem (should be `Error (Validation ...)`)
- A catch-all `| ex -> Error (Interop ...)` in domain or runtime code (only valid in Infrastructure)
- `try/with` in the public API layer (exceptions must already be caught before reaching here)
- An error case that carries no diagnostic metadata
- An error case that cannot be acted on by the caller ("something went wrong" with no specifics)

---

## Interop Boundary Patterns

These are the standard vocabulary for translating external library behaviour into typed errors.

### Exception capture
```fsharp
// For synchronous calls that can throw
let trySync (op: string) (f: unit -> 'T) : Result<'T, DomainError> =
    try Ok (f ())
    with ex -> Error (Interop (Exception (op, ExceptionInfo.ofException ex)))

// For async calls returning Task<'T>
let tryTask (op: string) (token: CancellationToken) (f: CancellationToken -> Task<'T>)
    : Task<Result<'T, DomainError>> = ...
```

### Null guard
```fsharp
// Immediately after any call that can return null
let ofObj (op: string) (expected: ExpectedThing) (x: 'T) : Result<'T, DomainError> =
    match box x with
    | null -> Error (Interop (NullReturned (op, expected)))
    | _ -> Ok x
```

### Success flag
```fsharp
// For library methods that return bool to signal success
let expectTrue (op: string) (expected: ExpectedThing) (value: bool) : Result<unit, DomainError> =
    if value then Ok ()
    else Error (Interop (FalseReturned (op, expected)))
```

**The op name convention**: always `"ModuleName.methodName"` matching the actual F# module
and function. This name appears in error metadata and must uniquely identify the call site.

---

## Cancellation Discipline

Cancellation is a first-class error case, not an exception to be caught and swallowed.

- Check for cancellation **before** crossing async boundaries, not just after.
- `OperationCanceledException` maps to `Error (Cancelled op)`, not to
  `Error (Interop (Exception ...))`. Distinguish them.
- Per-operation tokens should be **linked** with session/lifetime tokens:
  `CancellationTokenSource.CreateLinkedTokenSource(sessionToken, operationToken)`.
- The linked source must be `use`d (disposed) at the call site, not just created.

---

## Result Composition Patterns

### Railway-oriented (the standard)
```fsharp
// Preferred: bind chains naturally, failure short-circuits
DomainType.create rawInput
|> Result.bind (fun value ->
    OtherType.create otherInput
    |> Result.map (ComposedType.create value))
```

### Effect pipeline (for async operations in capabilities)
```fsharp
// Standard wiring for capabilities backed by gateways
CdmEffect.asks _.Token
|> CdmEffect.bind (SomeGateway.load handle input >> CdmEffect.ofTaskResult)
```

### What to flag
- Nested `match result with` blocks where `Result.bind` / `Result.map` would compose cleanly
- Missing `Result.mapError` where an error needs contextual enrichment at a boundary
- `Result.bind` where an intermediate step ignores its result (should be `Result.iter` or a `let!`)
