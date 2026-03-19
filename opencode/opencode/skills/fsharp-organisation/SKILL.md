---
name: fsharp-organisation
description: >
  Proactively audits F# codebases — both production code and tests — for structural
  drift, missed reuse opportunities, and premature duplication. Trigger whenever the
  user adds a new module, helper, or test file; when patterns appear more than once
  without a shared abstraction; or when you are about to write code that resembles
  something already in the codebase.
  Designed to catch what goes unnoticed: repeated effect wiring patterns, inline test
  setup that should be shared helpers, gateway traversal logic duplicated across modules,
  and combinator patterns copy-pasted before being named.
  Do not wait to be asked — if duplication exists or is about to be introduced, raise it.
  Covers: production pattern extraction, test helper organisation, effect combinator
  reuse, capability wiring patterns, and the three-strikes refactoring trigger.
---

# F# Code Organisation and Reuse Reviewer

You are auditing F# code for **structural drift and missed reuse opportunities** — the
kind of problems that accumulate silently across iterations and require deliberate
attention to surface.

This skill exists because these problems are easy to miss in the moment: each instance
looks reasonable in isolation, and the cost only becomes visible when you step back and
look at the whole.

**Your job is to step back and look at the whole.**

---

## The Three-Strikes Rule

When you see the same pattern, structure, or setup sequence **for the second time**,
note it mentally. When you see it **for the third time**, it must be named and extracted.

"Same pattern" means: structurally identical with different variable bindings.
Not just conceptually similar — actually the same shape.

Applies equally to:
- Production code (wiring, pipelines, gateway calls, traversal logic)
- Test setup (resource construction, capability wiring, fake builders)
- Error assertion patterns
- Helper functions defined inline in multiple test files

---

## What to Look For

Read `references/production.md` for production code patterns.
Read `references/tests.md` for test organisation patterns.

In both contexts, these general signals indicate a reuse opportunity:

### Signal 1: Inline setup repeated across files
The same 3–8 lines of setup code appear near the top of multiple functions or modules.
It has a name informally ("the connect setup", "the gateway pattern") but no formal home.

### Signal 2: A private helper duplicated across modules
A `private` function that does the same thing appears in two or more modules. `private`
means "not yet promoted", not "inherently local".

### Signal 3: A comment that describes repeated intent
If you see `// mirrors what Session.readDocument does internally` in two test files,
there is a shared concept that should be a shared function.

### Signal 4: Inconsistent evolution
Two similar modules diverged — one was updated, one was not. The inconsistency is a sign
the shared logic was never extracted, so each update had to be replicated manually.

### Signal 5: Effect patterns copy-pasted before being named
```fsharp
// Appears in three different capability wiring implementations:
CdmEffect.asks _.Token
|> CdmEffect.bind (DocumentGateway.load handle path >> CdmEffect.ofTaskResult)

CdmEffect.asks _.Token
|> CdmEffect.bind (DocumentGateway.save handle >> CdmEffect.ofTaskResult)

CdmEffect.asks _.Token
|> CdmEffect.bind (EntityGateway.list handle >> CdmEffect.ofTaskResult)
```
Three repetitions of `asks _.Token |> bind (f >> ofTaskResult)`. This pattern has a name
and should become a combinator once the third instance appears.

---

## How to Raise a Finding

1. **Quote the two or more instances** — show the actual repeated code, not a description.
2. **Name the pattern** — give the shared concept a name, even if the user will rename it.
3. **Propose an extraction** — show what the extracted form would look like and where it lives.
4. **Assess the urgency** — is this blocking, near-term debt, or a future-phase note?

Do not just say "there is duplication here". Show it, name it, and propose the fix.

---

## When to Raise vs When to Note

**Raise immediately** if:
- The pattern has three or more instances
- The duplication has already caused inconsistency (one copy updated, others not)
- A future phase will likely add a fourth instance

**Note for later** if:
- The pattern has appeared exactly twice and both copies are identical
- The correct home for the extraction is not yet clear
- The extraction would require changes across multiple layers simultaneously

When noting for later, be explicit: "This pattern has appeared twice. If it appears
again, extract it into [proposed location]."

---

## Reference Files

| Concern | File |
|---|---|
| Production code patterns, effect combinators, capability wiring | `references/production.md` |
| Test organisation, helper extraction, setup reuse | `references/tests.md` |
