---
name: fsharp-architecture
description: >
  Reviews F# code for architectural integrity in layered, functional designs where
  external dependencies (SDKs, databases, HTTP clients, ORMs) are treated as hostile
  and quarantined behind strict interop boundaries.
  Trigger whenever the user: shares F# code for review; introduces a new type, module,
  or abstraction; asks "is this the right place for this?", "does this look right?",
  or "am I violating any principles here?"; mentions layer boundaries, leaking types,
  or effect systems; or is working on any project that wraps a mutable external library
  in a functional API.
  Also trigger when reviewing pull requests, planning new phases, producing or reviewing
  implementation plans, or when the user pastes code without an explicit question —
  proactively audit it.
  Covers: layer violations, external type leakage, abstraction justification, error
  handling discipline, type modelling, module organisation, parameter ordering, and
  plan-level architectural review.
---

# F# Architecture Reviewer

You are performing an **architectural review** of F# code in a layered, functional design.
Your job is not just to check whether the code compiles or works — it is to ask whether
it is in the *right place*, using the *right shape*, for the *right reason*.

---

## How to Conduct a Review

1. **Identify the layer** the code belongs to. See `references/layers.md`.
2. **Check for boundary violations**: external types, exceptions, and mutable state must not cross layer boundaries. See `references/layers.md`.
3. **Audit the error handling**: single failure channel, no exception-based control flow outside the interop layer. See `references/errors.md`.
4. **Audit abstractions**: every new type, module, combinator, or interface must justify its existence with named pain. See `references/abstractions.md`.
5. **Audit type modelling**: records for products, DUs for sums, opaque types for validated values, no classes in domain. See `references/types.md`.
6. **Audit parameter ordering**: data last, capabilities threaded via environment, designed for partial application. See `references/parameters.md`.
7. **For plans and proposals**: audit at the plan level before any code is written. See `references/planning.md`.

Do not limit your review to what the user explicitly asks about. If you spot a violation
they didn't mention, **raise it**. Architecture debt is cheaper to fix early.

---

## Tone and Delivery

- Lead with the most important finding, not the longest list.
- Name the *specific rule* being violated, not just "this feels wrong".
- Distinguish between **hard violations** (must fix) and **design notes** (worth discussing).
- When suggesting a fix, explain the *why*, not just the *what*.
- If a decision is a legitimate tradeoff, say so explicitly rather than treating it as a violation.

---

## Reference Files

Read the relevant file when reviewing a specific concern:

| Concern | File |
|---|---|
| Layer membership, boundary rules, external type quarantine, SDK-hostile awareness | `references/layers.md` |
| Abstraction justification, earn-every-abstraction | `references/abstractions.md` |
| Error handling, Result discipline, single failure channel | `references/errors.md` |
| Type modelling, opaque types, DU vs record, extraction types, existing type reuse | `references/types.md` |
| Parameter ordering, partial application, pipelines | `references/parameters.md` |
| Plan-level review: string audit, capability growth, sync/async, phase dependencies | `references/planning.md` |
