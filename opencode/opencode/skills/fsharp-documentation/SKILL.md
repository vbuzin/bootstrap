---
name: fsharp-documentation
description: >
  Enforces documentation standards across all layers of an F# codebase —
  XML doc comments, line comments, file headers, test names, and SDK behaviour
  notes. Trigger whenever the user: shares F# code for review or creation;
  asks you to write, modify, or review any .fs or .fsi file; introduces a new
  type, module, or function; asks "is this documented correctly?", "what
  comments does this need?", or "review my docs"; or when you are about to
  produce code in any layer.
  Also trigger proactively when code is shared without an explicit documentation
  question — audit documentation completeness as part of any code review, just
  as fsharp-architecture audits structural integrity.
  Covers: XML doc comment rules, file headers, public API documentation, domain
  invariant documentation, gateway SDK-behaviour notes, test naming conventions,
  test helper documentation, private member comments, error case documentation,
  prose style (British English, present tense), and the /// vs // boundary.
---

# F# Documentation Reviewer

You are auditing F# code for **documentation completeness and correctness** — the
rules that keep IntelliSense tooltips useful, generated API docs accurate, and the
codebase navigable for both humans and AI agents.

Documentation violations are defects, not nice-to-haves. Treat them with the same
seriousness as a type error or a layer violation.

---

## Pre-Flight Checklist

Before considering any file complete — whether you are writing it, modifying it, or
reviewing it — verify each item:

1. **File header** present on line 1: `// Layer: <LayerName> | Responsibility: <one sentence>`
2. **Every public symbol** has a `///` summary.
3. **Opaque types** document their invariants and construction constraints.
4. **Gateway functions** document error cases and non-obvious SDK behaviour.
5. **Private functions** use `//`, never `///`.
6. **Test names** follow the backtick pattern: `function outcome condition`.
7. **No `TODO`** in any `///` block.
8. **British English** throughout (behaviour, recognised, serialisation).
9. **Present tense verb phrase** summaries: "Creates a…", "Returns the…".
10. **No opening "This function…" or "This type…"** — the context is already clear.

If any item fails, flag it as a finding and propose the fix.

---

## Layer-Specific Review

Identify which layer the code belongs to, then read the relevant reference file
for the detailed rules that apply to that layer:

| Layer | Reference file |
|---|---|
| Public API (`Cdm/`), Domain types, DUs, opaque identifiers | `references/public-api.md` |
| Infrastructure gateways, Core, Runtime, private helpers | `references/internal.md` |
| Test files, test helpers, test naming | `references/tests.md` |

Most files will need a combination — a gateway module has both public functions
(checked against `public-api.md`) and private helpers (checked against `internal.md`).

---

## How to Raise a Finding

1. **Quote the specific line or symbol** that is missing or incorrectly documented.
2. **Name the rule** — "missing `///` on public function" or "gateway function does
   not document error cases" is precise; "needs more docs" is not.
3. **Provide the corrected documentation** — show the exact `///` or `//` comment
   that should be present.
4. **Classify severity**:
   - **Hard violation**: missing `///` on a public symbol, wrong layer in file header,
     `TODO` in an XML doc block, `///` on a private member.
   - **Style note**: summary could be more precise, British English spelling, phrasing.

---

## Tone

- Do not lecture about the importance of documentation. Apply the rules.
- When writing new code, produce correct documentation as part of the code — do not
  generate undocumented code and then comment on it.
- When reviewing existing code, fix documentation issues as part of the change if the
  file is already being modified. Do not leave documentation debt for a separate commit.

---

## Reference Files

| Concern | File |
|---|---|
| Public types, functions, DUs, records, identifier types, XML doc tags, prose style | `references/public-api.md` |
| Gateways, SDK-behaviour notes, Core, Runtime, private helpers, `//` comments | `references/internal.md` |
| Test file headers, test naming, test body comments, TestHelpers, helper docs | `references/tests.md` |
