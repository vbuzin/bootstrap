# Test Organisation: Helper Extraction and Setup Reuse

## The Core Problem

Test code accumulates duplication faster than production code because each new test file
tends to borrow from the nearest similar test rather than from a shared library. The
result is setup code that is 80% identical across files, subtle inconsistencies when one
copy is updated but others are not, and test functions that are hard to read because half
their length is infrastructure rather than intent.

---

## Anatomy of a Well-Organised Test Suite

### Layer 1: Shared infrastructure helpers
Lives in a dedicated file (e.g. `TestHandles.fs`, `TestResources.fs`).
Contains: external resource construction, minimal SDK setup, shared object builders.
**Rule**: if you need to construct an external object in more than one test file, it belongs here.

### Layer 2: Fake/stub capability builders
Lives in a dedicated file (e.g. `FakeCapabilities.fs`, `Stubs.fs`).
Contains: named constructors for deterministic capability/dependency implementations.
**Rule**: no test file should inline a capability record literal — always use a named constructor.

### Layer 3: Workflow runners
Typically private helpers within a test file, or promoted to shared if used across files.
Contains: the "run X through the effect system" pattern that mirrors what the public API
does internally.

### Layer 4: Test cases
The actual `[<Fact>]` functions. Should be short: arrange (1–3 lines), act (1 line),
assert (1–3 lines). If a fact function exceeds 15 lines, setup is leaking in.

---

## Common Extraction Targets

### The `connectWith` / `setupWith` pattern
When the same "connect and return connected capabilities" sequence appears privately in
three or more test files:

```fsharp
// Defined independently in WorkflowTests, ReadTests, WriteTests — same function
let private connectWith (startup: StartupCapabilities) : Task<ConnectedCapabilities> =
    task {
        match!
            MyEffect.run
                { Capabilities = startup; Token = CancellationToken.None }
                (MyEffect.asks _.Capabilities |> MyEffect.bind _.Connect())
        with
        | Ok caps -> return caps
        | Error err -> return failwith $"Connect failed: {err}"
    }
```

**When to extract**: immediately — if it appears in three files it is already past threshold.
**Where**: a `TestWorkflows.fs` shared helper, or inside `FakeCapabilities.fs` as a
public function.

### The `setupResource` pattern
When the same external resource construction (e.g. creating a temp directory, spinning
up an in-memory store, mounting an adapter) appears privately in multiple test files:

```fsharp
// Private in FileTests, IndexTests, QueryTests — same shape
let private setupResource (root: string) : Task<ConnectionHandle> =
    task {
        let opts = ConnectionOptions.empty |> ConnectionOptions.withLocal ns root
        match! Gateway.connect CancellationToken.None opts with
        | Error err -> return failwith $"Setup failed: {err}"
        | Ok handle -> return handle
    }
```

**Where it lives**: a shared `TestHandles.fs` or `TestInfrastructure.fs`.

### The `failUnexpected` helper
```fsharp
// Defined privately in every test file, often with slightly different messages:
let private failUnexpected result =
    Assert.True(false, $"Unexpected: {result}")
```

This belongs in a single shared test utility. Variations in the message format confirm
the copies were written independently rather than shared.

### Temporary resource lifecycle
```fsharp
// Repeated in every integration test that needs temporary storage:
let root = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
Directory.CreateDirectory(root) |> ignore
try
    ...
finally
    Directory.Delete(root, recursive = true)
```

Once this appears in three or more tests, extract a `withTempDirectory` helper:

```fsharp
let withTempDirectory (f: string -> Task<unit>) : Task<unit> =
    task {
        let root = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory(root) |> ignore
        try
            do! f root
        finally
            Directory.Delete(root, recursive = true)
    }
```

### Repeated resource file creation
When tests write the same minimal resource file (JSON, XML, config) across multiple files,
extract a named helper:

```fsharp
// Instead of duplicating the JSON template:
let writeMinimalDocument (root: string) (name: string) =
    File.WriteAllText(
        Path.Combine(root, $"{name}.doc.json"),
        minimalDocumentJson name)
```

---

## Fake Capability Constructor Discipline

### Name constructors by what they configure, not what they return
```fsharp
// POOR — describes the return type, not the configuration
createSuccessWithHandle handle

// BETTER — describes what is distinctive about this configuration
createWithConnectedHandle handle         // emphasises the connected state
createFailingAtDocumentRead handle err   // emphasises which operation fails
```

### Cover exactly the scenarios you have
Constructors should match the test scenarios that need them. If a constructor exists
but no test uses it, delete it — it is speculative API surface.

### Document what each fake ignores
When a constructor accepts an argument it does not use (needed for API consistency but
irrelevant to the test logic), say so:

```fsharp
/// Creates capabilities that fail at ReadDocument.
/// The handle parameter is accepted for API consistency but is not used —
/// ReadDocument returns the error before the handle would be accessed.
let createWithDocumentError (_handle: DocumentHandle) (error: DomainError) : StartupCapabilities =
    ...
```

---

## Compilation Order

F# compilation is order-dependent. Test helpers must appear before the test files that
use them in the `.fsproj` file.

Recommended ordering:
1. Shared test utilities (`TestHandles.fs`, `TestWorkflows.fs`)
2. Fake/stub builders (`FakeCapabilities.fs`)
3. Unit tests (no I/O, no external dependencies)
4. Integration tests (in dependency order)

When a new shared helper is extracted, **update the `.fsproj` immediately**. A helper
in the wrong position produces confusing `FS0039` errors that obscure the real issue.

---

## What Healthy Test Files Look Like

A test file for a gateway module should contain:
- A call to the shared `TestHandles.setupResource` helper (not inline setup)
- One fact per distinct scenario: success, each distinct failure mode, cancellation
- Facts that are 8–15 lines each, mostly Arrange / Act / Assert
- No inline external object construction (belongs in `TestHandles.fs`)
- No inline capability record literals (belongs in `FakeCapabilities.fs`)
- Comments that explain *why* a test is structured a certain way, not *what* it does
  (the code should make the *what* obvious)
