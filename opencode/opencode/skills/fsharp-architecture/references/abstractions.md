# Abstraction Justification: Earn Every Abstraction

## The Core Principle

An abstraction is justified when it **prevents a specific, nameable hazard** or
**unlocks a specific, demonstrable usability benefit** that already exists in real code.

The question to ask before introducing any new type, module, combinator, CE, or interface:

> "What specific pain does this prevent, and where in the existing code does that pain live?"

If you cannot point to existing code where the pain is visible, the abstraction is premature.

---

## The Earn-Every-Abstraction Test

Ask these questions in order. Stop at the first "no" — the abstraction is not yet earned.

1. **Does the pain already exist?** Can you point to two or more real call sites in the
   existing codebase that suffer from the problem this abstraction claims to solve?

2. **Is the pain recurring?** A one-off awkwardness does not justify an abstraction.
   The pattern should appear at least twice, ideally three or more times.

3. **Does the abstraction make the call sites simpler or safer?** Not just different —
   demonstrably better. Measure against the actual existing code, not against a
   hypothetical.

4. **Would a future maintainer thank you or curse you?** Abstractions that obscure
   rather than illuminate are worse than the duplication they replaced.

---

## Common Premature Abstractions in F# Functional Design

### Modelling intent before you understand the shape
```fsharp
// ConnectRequest introduced before connect took any meaningful input.
// The type existed to be principled, not to solve a real problem.
type ConnectRequest = { Namespace: NamespaceName; RootPath: string }
```
**Fix**: Wait until the function actually takes caller-supplied validated input.
The right shape of the type only becomes visible when real parameters exist.

### Wrapping an external type hierarchy that needs no wrapping
If the external library exposes `AdapterBase` with many subtypes, wrapping each in a DU
case produces a closed list that must be updated every time the library adds a subtype,
with no safety benefit — the validation happens on construction, not on the wrapper type.

**Fix**: Expose the external base type directly when wrapping adds no safety or usability.
Document the decision explicitly. The external dependency is already a transitive
requirement; making callers take it directly is not a new burden.

### Capability contracts created before implementations exist
Adding a new function to a record of capabilities before there is a real implementation
to wire in means the contract was designed speculatively. A capability without an
implementation is a promise with no evidence it is the right promise.

### Effect combinators created before the repetition is visible
```fsharp
// bindTask created before the pattern appeared more than once
let bindTask f expr = bind (f >> ofTaskResult) expr
```
**Fix**: Let the pattern appear naturally in two or three real call sites. Then name it.
The name will be more accurate because it comes from the pattern, not from anticipation.

---

## Signs That an Abstraction IS Earned

- You find yourself copying and modifying the same 3–5 line pattern in multiple modules.
- A bug fix in one copy needs to be replicated manually to other copies.
- A new reader of the code must understand the same mechanism multiple times to follow
  a single workflow.
- A name for the concept exists naturally in the domain ("gateway", "handle", "boundary",
  "capability") and the current code has no explicit home for it.

---

## The "Layers Before Abstractions" Rule

Before introducing a new abstraction, check whether the problem is actually a
**misplaced responsibility**. Often what looks like a need for a new type is actually
code in the wrong layer.

Ask: "Would this pain disappear if the code were in the right layer?" If yes, move it
rather than abstracting it.

---

## Reviewing Abstractions in Practice

When you encounter a new type, module, or combinator, ask:

1. What does this replace? Show me the code it simplifies.
2. How many times does the simplified form appear?
3. What breaks if I delete this abstraction today?
4. Is there a simpler shape that solves the same problem?

If the answers are "nothing specific", "once", "not much", and "yes" — the abstraction
should not exist yet.
