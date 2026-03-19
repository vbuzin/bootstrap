# Parameter Ordering and Pipeline Composition

## The Core Rule

Parameters are ordered for **partial application at the call site** — the most stable,
least varying argument comes first; the most varying "data" argument comes last.

This is not a style preference. It is what makes `|>` pipelines work naturally and what
makes partial application produce useful intermediate functions.

---

## The Standard Ordering Hierarchy

From first (most stable) to last (most varying):

1. **Capability / handle** (connection, context object, external resource handle)
2. **Configuration** (policies, options, flags)
3. **Identity / address** (path, name, key, namespace)
4. **Cancellation token** (lifetime concern, not business logic)
5. **Data / payload** (the thing being processed)

### Example
```fsharp
// CORRECT — stable dependencies first, token before payload
let loadDocument (handle: ConnectionHandle) (path: ResourcePath) (token: CancellationToken)
    : Task<Result<DocumentHandle, DomainError>>

// WRONG — token before path breaks partial application in capability wiring
let loadDocument (handle: ConnectionHandle) (token: CancellationToken) (path: ResourcePath)
```

The correct ordering allows clean pipeline composition in capability wiring:
```fsharp
// `SomeGateway.load handle path` produces `CancellationToken -> Task<Result<_, _>>`
// The pipeline supplies the token naturally as the final argument
CdmEffect.asks _.Token
|> CdmEffect.bind (SomeGateway.load handle path >> CdmEffect.ofTaskResult)
```

If token came before path, the `>>` composition would require an eta expansion,
breaking the pipeline's readability.

---

## Public API Ordering Convention

Public API functions follow a slightly different convention: the most intent-bearing
arguments come first, and the **session/context comes last** to enable `|>` from the
session object.

```fsharp
// Public API: what, how, then session last
let readDocument (path: ResourcePath) (token: CancellationToken) (session: MySession)
    : Task<Result<DocumentHandle, DomainError>>

// Enables natural pipeline usage
let! handle = session |> MySession.readDocument path operationToken
let! content = session |> MySession.readContent handle operationToken
```

---

## Configuration Builder Pattern

When constructing options objects, each combinator should take `opts` as its **last**
parameter so the pipeline reads naturally:

```fsharp
// CORRECT — opts last enables |> composition
let withLocalAdapter (ns: NamespaceName) (rootPath: string) (opts: ConnectionOptions)
    : ConnectionOptions

// Usage reads naturally as a pipeline
let opts =
    ConnectionOptions.empty
    |> ConnectionOptions.withLocalAdapter ns rootPath
    |> ConnectionOptions.withDefaultNamespace ns
```

---

## What to Flag

| Issue | Example | Problem |
|---|---|---|
| Token before address/identity | `f handle token path` | Breaks `>> ofTaskResult` composition |
| Data before configuration | `f payload policy handle` | Partial application produces odd-shaped closures |
| Options not last in builder combinators | `withAdapter rootPath ns opts` | Forces `fun opts -> ...` at every call site |
| Session/context not last in public API | `readDocument session path token` | Cannot `session \|> readDocument path token` |
| Inconsistent ordering across sibling functions | `load handle path token` / `save token handle` | Callers must check the signature every time |

---

## Reviewing Ordering in Practice

When you see a function signature, ask:

1. Can I partially apply this to get a useful intermediate function?
2. Does the ordering support `|>` pipelines at the actual call sites?
3. Are all sibling functions in this module consistent with each other?

Ordering problems are most visible at the **point of use**, not in the declaration.
If you have access to the call sites, look at them before concluding the ordering is correct.
