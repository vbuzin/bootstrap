# Test Documentation Rules

These rules apply to all test files, test helper modules, and shared test
infrastructure.

---

## File Headers

Same rule as production code — every test file starts with line 1:

```
// Layer: Tests | Responsibility: <one sentence>
```

The responsibility clause describes what the file tests, not what it contains:

**Good**: `// Layer: Tests | Responsibility: ManifestGateway integration tests for load, extract, save, and cancellation behaviour.`

**Bad**: `// Layer: Tests | Responsibility: Tests for the manifest gateway.`

---

## Test Names

Use backtick syntax. Follow the pattern:

```
`<function under test> <expected outcome> <condition>`
```

### Examples

```fsharp
[<Fact>]
let ``loadManifest returns ManifestHandle when manifest file exists`` () = ...

[<Fact>]
let ``DocumentName returns Validation error when suffix is .manifest.cdm.json`` () = ...

[<Fact>]
let ``bind returns error unchanged when first computation fails`` () = ...

[<Fact>]
let ``connect returns Cancelled when token is already cancelled`` () = ...
```

### Rules

- The name must be specific enough that a CI failure identifies the problem without
  opening the file.
- Use the function's actual name, not a paraphrase: `loadManifest`, not "loading a
  manifest".
- State the outcome positively when possible: "returns ManifestHandle when…" rather
  than "does not fail when…".
- For error-path tests, name the specific error case: "returns Validation error when…",
  "returns Cancelled when…", "returns NullReturned when…".

---

## Comments Inside Test Bodies

Use `//` line comments inside test bodies **only** when:

1. **The test exercises a specific SDK behaviour** that is not obvious from the code.
   The comment explains the *SDK* behaviour, not the *test* behaviour.

   ```fsharp
   [<Fact>]
   let ``loadManifest returns NullReturned when manifest file does not exist`` () =
       // The SDK returns null rather than throwing when a manifest path does not resolve,
       // so this test exercises the Interop.ofObj null-handling path specifically.
       withTempCorpus (fun root handle -> ...
   ```

2. **The arrange step involves non-obvious setup** — a specific file layout, a
   deliberate ordering, a workaround for test infrastructure constraints.

   ```fsharp
   // Uses a sentinel path because saveConfigAsync validates the path only after
   // checking the cancellation token, so the path value is irrelevant to this test.
   ```

### What NOT to comment

- Obvious assertions: `Assert.Equal("documentName", error.Field)` is self-documenting.
- Standard match/assert patterns.
- `open` statements.
- The act step when the function name and test name already make it clear.

---

## Test Helper Functions (Within Test Files)

Private helper functions that are **setup** (not the subject under test) use `//`
comments explaining:

1. **What they set up and tear down.**
2. **When to use them** (and when to use an alternative).
3. **Assumptions they make** about the filesystem, SDK state, or preconditions.

```fsharp
// Writes a manifest file, acquires a corpus handle, loads the manifest handle,
// and loans both to 'f'. Cleanup runs unconditionally when 'f' completes.
// Used for tests where loadManifest is setup rather than the subject under test.
let private withTempManifest (name : string) (f : CorpusHandle -> ManifestHandle -> Task<unit>) = ...
```

```fsharp
// Acquires a corpus handle and loans it to 'f', passing the temp root path as well.
// Uses setupLocalCorpusWithDefaultNamespace because save tests require the SDK to resolve
// a write target, which needs a default namespace. Load-only tests tolerate the extra setup.
let private withTempCorpus (f : string -> CorpusHandle -> Task<unit>) = ...
```

### When NOT to comment helpers

- Trivial one-line helpers that delegate to another function: the name is the comment.
- Helpers whose purpose is obvious from their signature and usage.

---

## Shared Test Infrastructure (`TestHelpers.fs`)

Every **public** function in `TestHelpers.fs` gets `///` (XML doc comments) — they
are effectively a shared API consumed by multiple test files.

Document:
1. What it sets up and tears down.
2. When to use it vs. alternatives.
3. Assumptions about the filesystem or SDK state.

```fsharp
/// Creates a CorpusHandle with a local adapter mounted at root.
/// Fails the test immediately if corpus connection or adapter mounting fails.
let setupLocalCorpus (root : string) : Task<CorpusHandle> = ...

/// Like setupLocalCorpus, but also sets the default namespace.
/// Required for SaveManifest tests — the SDK needs a default namespace to resolve the write target.
let setupLocalCorpusWithDefaultNamespace (root : string) : Task<CorpusHandle> = ...
```

**Private** helpers within `TestHelpers.fs` use `//`, same as everywhere else.

---

## No `///` on Test Functions

Test functions (`[<Fact>]` and `[<Theory>]` decorated `let` bindings) do **not**
get `///` comments. The backtick test name is the documentation. Adding `///` above
a test function is redundant and creates maintenance burden.

---

## Parameterised Tests

For `[<Theory>]` tests with `[<InlineData>]` or `[<MemberData>]`, document the
data source function with a `//` comment if the test data encodes non-obvious
cases:

```fsharp
// Edge cases: empty-after-trim, single control character, exactly at max length,
// one character over max length. Each pair is (input, expected field in error).
let invalidInputs : obj[] list = [ ... ]
```

If the data is self-evident (e.g. a list of valid names), skip the comment.
