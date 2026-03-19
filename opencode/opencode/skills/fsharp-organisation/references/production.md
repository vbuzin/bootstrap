# Production Code: Pattern Extraction and Reuse

## Effect Combinator Patterns

The most common source of unextracted duplication in F# effect-based code is
**wiring patterns** — the repeated plumbing between effect combinators and gateway
functions.

### The token-bind-lift pattern
When capability wiring repeats this structure across three or more implementations:

```fsharp
MyEffect.asks _.Token
|> MyEffect.bind (SomeGateway.methodA handle arg >> MyEffect.ofTaskResult)

MyEffect.asks _.Token
|> MyEffect.bind (SomeGateway.methodB handle >> MyEffect.ofTaskResult)

MyEffect.asks _.Token
|> MyEffect.bind (OtherGateway.list handle >> MyEffect.ofTaskResult)
```

This is a candidate for a named combinator:
```fsharp
// In the effect module — pure effect plumbing, no gateway-specific knowledge
let ofTokenTask (f: CancellationToken -> Task<Result<'T, DomainError>>) : MyEffect<'caps, 'T> =
    asks _.Token |> bind (f >> ofTaskResult)
```

Capability wiring then becomes:
```fsharp
ReadDocument = fun path ->
    MyEffect.ofTokenTask (DocumentGateway.load handle path)

SaveDocument = fun handle ->
    MyEffect.ofTokenTask (DocumentGateway.save handle)
```

**When to extract**: after the third distinct gateway call uses this shape.
**Where it lives**: the effect module (Runtime layer) — it is pure effect plumbing
with no gateway-specific knowledge.

### The asks-capabilities-bind pattern
Public API operations often repeat the same two-step dispatch:

```fsharp
MyEffect.asks _.Capabilities |> MyEffect.bind _.ReadDocument(path)
MyEffect.asks _.Capabilities |> MyEffect.bind _.ListEntities(handle)
MyEffect.asks _.Capabilities |> MyEffect.bind _.Save(handle)
```

Consider whether the effect module should expose:
```fsharp
let withCaps (f: 'caps -> MyEffect<'caps, 'T>) : MyEffect<'caps, 'T> =
    asks _.Capabilities |> bind f
```

**Caution**: Only extract if the pattern genuinely appears three or more times in
production wiring — not if it appears once in tests and twice in the public API.

---

## Capability Constructor Proliferation

When fake or real capability constructors grow unbounded, it is a sign that capability
configuration is not composable:

```fsharp
// Constructors that enumerate every combination of success/failure:
createSuccessWithHandle handle
createSuccessWithHandleAndEntities handle entities
createWithDocumentError handle error
createWithEntitiesError handle error
createWithSaveError handle error
// ...
```

When constructors outnumber capabilities, consider a record-based configuration approach:

```fsharp
type FakeCapabilityConfig = {
    Handle: DocumentHandle
    Entities: EntityDeclaration list
    DocumentError: DomainError option
    EntitiesError: DomainError option
    SaveError: DomainError option
}
```

**When to refactor**: when the number of constructors exceeds the number of capabilities,
or when adding a new capability requires adding two or more new constructors.

---

## Gateway Traversal Logic

If two or more gateways need to traverse a list and short-circuit on the first failure,
the traversal helper does not belong in either gateway — it belongs in a shared utility:

```fsharp
// Defined as `private` in one gateway module — should be promoted if used elsewhere
let private traverseResult (f: 'a -> Result<'b, DomainError>) (items: 'a list)
    : Result<'b list, DomainError> =
    (items, Ok [])
    ||> List.foldBack (fun item acc ->
        match f item, acc with
        | Ok v, Ok rest -> Ok (v :: rest)
        | Error e, _ | _, Error e -> Error e)
```

**Where it lives**: a `ResultExtensions` or `Traversal` module in Core or Domain —
it has no external dependency knowledge, only `Result` knowledge.

---

## Repeated Pre-condition Pattern in Gateways

If every gateway function opens with the same check:

```fsharp
match EffectBoundary.cancelIfRequested op token with
| Error cancelled -> return Error cancelled
| Ok () ->
    let sdkObj = Handle.unwrap handle
    ...
```

Once this appears in three or more gateway functions, consider a helper that encapsulates
the pre-condition:

```fsharp
let withCancellationCheck
    (op: string) (token: CancellationToken) (handle: 'handle)
    (unwrap: 'handle -> 'sdkObj)
    (f: 'sdkObj -> Task<Result<'T, DomainError>>)
    : Task<Result<'T, DomainError>> =
    task {
        match EffectBoundary.cancelIfRequested op token with
        | Error e -> return Error e
        | Ok () -> return! f (unwrap handle)
    }
```

---

## Module Cohesion

### When a file has grown too large
Signs that a module should be split:
- Multiple `module private` sections with genuinely different concerns
- Helper functions used by only one of the types defined in the file
- The file name no longer accurately describes the file's contents

### `internal` vs `private` vs public
- `private`: used only within this module — not shared even with tests
- `internal` (via `InternalsVisibleTo`): shared with the test project but not public
- No modifier: part of the public surface

Audit: are things `internal` that should be `private`? Are things `private` that are
duplicated in test helpers and should be `internal` instead?
