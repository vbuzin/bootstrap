---
name: idiomatic-rust
description: >
  Idiomatic Rust style guide for readable, maintainable code. Distills refactoring
  patterns: extract shared helpers over nested ifs, pure shape predicates, collapse
  abandon/Option ladders, prefer existing APIs over new ones, emit/leaf helpers,
  exact contracts over fuzzy contains, lean tests that assert structure not plumbing,
  and a consistent module/docs/comment style (emits tree, progress contract, section
  dividers, sparse why-not-what inline notes). Use when writing or reviewing Rust,
  refactoring for readability, cleaning up parser/grammar code, documenting modules,
  or when the user runs /idiomatic-rust. Triggers: idiomatic Rust, rust style,
  clean up this rust, make this more readable, rust refactor, ergonomic rust,
  rust docs, comment style, module docs.
---

# Idiomatic Rust

Apply these patterns when writing or refactoring Rust. Goal: code that feels
inevitable - fewer branches, clear ownership of concepts, exact contracts,
minimal ceremony.

Project-specific invariants (e.g. AGENTS.md, ADRs) always win over this skill.

## Core stance

1. **Prefer deleting complexity** over rearranging it.
2. **One concept, one helper** - if two call sites share a sequence, extract it.
3. **Decide, then act** - shape checks and classification before mutation.
4. **Exact contracts** - full-string matches, not fuzzy `contains` / prefix-only parses.
5. **Reuse existing APIs** - do not invent platform/parser helpers until forced;
   if tempted to add a new public/core API, stop and ask.
6. **Readable > clever** - boring early-returns beat nested boolean soup.
7. **Docs that orient** - module docs state what, emits, and progress contract;
   inline comments only where non-obvious.

## Module layout and documentation

Apply this shape to non-trivial modules (especially parsers / multi-path code).

### Module doc (`//!`) skeleton

1. **One-line title** - what the module is.
2. **Purpose / fidelity** - what it implements (spec, counterpart, scope).
3. **Emits tree** (when the module produces structure) - indented kind outline.
4. **Rules / notes** - only load-bearing shape rules, bullets.
5. **Progress contract** - guard / parse / failure policy (uniform wording across
   sibling modules when they share a dispatcher).

Example (grammar object):

```rust
//! Timestamp object grammar.
//!
//! Implements ... per org-syntax section 5.16 ..., adapted to the token-stream
//! / event-emitting CST model.
//!
//! Emits:
//!   TIMESTAMP
//!     DATE
//!     [TIME | TIME_RANGE]
//!     ROD*
//!
//! Progress contract (uniform with markup/link):
//! - `at_timestamp` is the cheap guard.
//! - `parse_timestamp` is only called when the guard returns true.
//! - On structural failure, the opener is left bare; no rewind to the dispatcher.
```

### Section dividers

Group the file with consistent ASCII banners (not ad-hoc `// -- Foo ---` mixtures):

```rust
// ---------------------------------------------------------------------------
// Public surface for the inlines dispatcher
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Diary: <%%(SEXP) [TIME | TIME_RANGE]>
// ---------------------------------------------------------------------------
```

Typical order:

1. Shape constants / fixed lead sequences
2. Public surface (`at_*` / `parse_*`)
3. One section per major path or domain concept
4. Shared helpers (pure scans, emit helpers)
5. Tests

### Item docs (`///`)

- **First line**: what it does (imperative or "Returns ...").
- **Blank line**, then only what a caller needs: precondition, success/failure,
  fidelity note, non-obvious parameter meaning.
- Do **not** restate the function body or narrate every step.
- Public / `pub(crate)` entry points get docs; private helpers only when the
  name alone is not enough or fidelity is non-obvious.

```rust
/// Parse a regular bracket link (precondition: `at_link(parser, 0)` is true).
///
/// On success, emits LINK (and optional LINK_DESCRIPTION) and advances past it.
/// On failure, abandons the marker; any tokens already eaten remain bare.
pub(crate) fn parse_link(parser: &mut Parser) { ... }
```

### Inline comments (`//`)

Sparse and high-value only:

| Use | Example |
|-----|---------|
| Cursor / phase state | `// Cursor is on the first '%' of '%%(`.` |
| Domain rule not in the name | `// Diary is active-only: '<%%(...)>', never '[%%(...)'.` |
| Token identity after eat | `parser.eat_any(); // --` |
| Why a choice exists | `// Peek before consume so form choice is pure.` |
| Fidelity pointer | `// Matches the byte parser: find_any(..., b">\n") then rfind ')'.` |

Avoid:

- Narrating the next line (`// increment i`, `// return true`)
- Restating type names or obvious control flow
- Large block comments that should be a helper name or `///` on a function
- Stale/wrong copy-paste (`// check at_block` on a heading parse)

### Naming alignment with docs

Prefer names that make comments unnecessary (`diary_bounds`, `parse_one_stamp`,
`habit_deadline_len`). If a comment only renames the call, delete the comment
or rename the function.

### Tests section

- Banner: `// ---------------------------------------------------------------------------` / `// Tests` / same closer, or short `// --- Basic forms ---`.
- Test helpers: short, no essay docs unless the harness is non-obvious.
- Prefer assert messages / good test names over paragraph comments in tests.

## Control flow

### Flatten nests with early returns

Bad: 4-level `if { if { if { true } else { false } } }`.

Good:

```rust
fn try_parse_range_tail(parser: &mut Parser, open: Kind, close: Kind) -> bool {
    if parser.current() != Kind::TEXT || parser.text_of(0) != "--" {
        return false;
    }
    parser.eat_any();
    if parser.current() != open {
        return false;
    }
    parser.eat_any();
    parse_one_stamp(parser, close)
}
```

### Collapse repeated failure ladders

When several steps each do `let Some(x) = ... else { abandon; return }`, extract
a pure/setup function that returns `Option<...>` and abandon once:

```rust
let Some((outer_abs, sexp_end_abs)) = diary_bounds(parser) else {
    sexp_marker.abandon(parser);
    marker.abandon(parser);
    return;
};
```

### Share the happy path

If two flows both do "find closer -> interior under limit -> eat closer", make
one function (`parse_one_stamp`) and call it from both. Duplication of the
sequence is a design smell even when each copy is short.

### Checkpoint/rewind for optional tails

Probe optional continuations behind a checkpoint; rewind on failure. Keep the
probe function pure-ish (`-> bool`) and let the caller own checkpoint policy.

## Helpers and layering

### Extract for naming, not for lines

A helper is justified when it:

- names a domain step (`parse_rods`, `habit_deadline_len`, `stamp_brackets`), or
- removes a repeated multi-step sequence, or
- isolates lexer impedance / weirdness in one place.

Reject thin wrappers that only rename a single call.

### Prefer pure observers where possible

If a scan does not need to advance the cursor or mutate limit state, take `&T`
not `&mut T`. Avoid borrowing mutably "just in case".

```rust
// Good: pure find
fn find_last_kind_before(parser: &Parser, before_abs: usize, kind: Kind) -> Option<isize>

// Avoid: mut + with_limit only to read
fn find_last_kind_before(parser: &mut Parser, ...) // unless mutation is required
```

### Compose existing primitives

Build local helpers from what the type already exposes (`find_kind_fwd`,
`at_prefix`, `eat_run`, ...). Example: same-line closer = find kind, then reject
if a newline appears earlier - not a bespoke peek loop next to the canonical
finder.

### Keep constants and shape limits at the top

Magic lengths, max digit runs, fixed lead sequences (`DIARY_LEAD`) belong near
the top of the module, not buried mid-file.

## Shape and contracts

### Exact match for structured tokens

If the grammar is `VALUE UNIT`, require the whole string - not a valid prefix
with trailing junk:

```rust
// Good
digits + unit_char && digits + 1 == s.len() && value > 0

// Bad
parse prefix value+unit and ignore rest  // accepts "2dfoo"
```

Optional tails (e.g. habit `/Nunit`) are **extra tokens** folded into the same
node length - not accidental acceptance of garbage in the value TEXT.

### Classify before consume

Decide node kind / length while only peeking; then emit in one go. Avoid
negative relative peeks after partial eats (`text_of(-1)`).

```rust
let as_range = is_glued_range_middle(mid) && /* peeks 3..=4 */;
let n = if as_range { 5 } else { 3 };
emit_leaf(parser, kind, n);
```

### Isolate impedance mismatch

Lexer quirks (delimiter splits, glued hyphens in TEXT) live in one documented
helper. Do not leak `contains('-')` or special-case token surgery into weekday,
date, and ROD logic.

### Small pure string/byte predicates

Reuse tiny predicates instead of repeating digit loops:

```rust
fn is_digits(s: &str) -> bool { !s.is_empty() && s.bytes().all(|c| c.is_ascii_digit()) }
fn starts_with_digit(s: &str) -> bool { s.as_bytes().first().is_some_and(u8::is_ascii_digit) }
```

Prefer `is_ok_and`, `is_some_and`, `then_some`, `split_once` over verbose match
trees for simple predicates.

## Emission and mutation

### One leaf emitter

If several sites do start + eat N + complete, use one helper:

```rust
fn emit_leaf(parser: &mut Parser, kind: Kind, n: usize) {
    let m = parser.start();
    for _ in 0..n {
        parser.eat_any();
    }
    m.complete(parser, kind);
}
```

### Optional parse + trailing whitespace

Bundle "try parse; if ok, eat whitespace" when that pair repeats:

```rust
fn try_parse_time_then_ws(parser: &mut Parser) {
    if try_parse_time_sequence(parser) {
        parser.eat_run(Kind::WHITESPACE);
    }
}
```

### Pair related delimiters

```rust
fn stamp_brackets(active: bool) -> (Kind, Kind) {
    if active { (L_ANGLE, R_ANGLE) } else { (L_BRACKET, R_BRACKET) }
}
```

Avoid scattering open/close conditionals.

### Infallible conversions when the invariant is local

If `rel` always comes from a successful in-bounds find, return `usize` (with
`debug_assert`) instead of forcing every caller through `Option` noise. Reserve
`Option` for truly fallible domain steps.

## Types and APIs

- Prefer `&str` / slices and exact shape checks over owning intermediate strings.
- Do not return structured parse results you immediately throw away - if only a
  bool is needed, return a bool (or keep structure only when callers use fields).
- Avoid new public API on shared types for a single call site. Local module
  helpers first; promote only when a second domain needs the same primitive.
- No `unwrap`/`expect` in library code except tests (with a reason). Use
  `debug_assert` for internal invariants.

## Tests

### Assert the contract that can break

- Prefer structural asserts (node kind text, counts) over "whole outer string
  equals source" alone - the latter masks under-carved children.
- Negative cases for exactness (`+2dfoo` is not a ROD).
- Optional tails that must fold in (habit `/4d` inside ROD text).

### Do not test other modules' plumbing here

Drop tests of shared infrastructure (`eat_run`, generic finders) from feature
modules. Keep isolated tests only for helpers this module owns.

### Lean fixtures

Small helpers are fine:

```rust
fn assert_full_ts(src: &str) { ... }
fn text_of_kind(src: &str, kind: Kind) -> Option<String> { ... }
fn count_kind(src: &str, kind: Kind) -> usize { ... }
```

Merge near-duplicate cases; keep one sharp test per behavior.

## Refactor checklist

When cleaning a Rust module, work in this order:

1. **Contracts** - exact shapes, under/over-carve bugs, weak tests that mask them.
2. **Shared sequences** - extract the real repeated path (not cosmetic splits).
3. **Flatten control flow** - early returns, single abandon sites, checkpoint tails.
4. **Pure vs mutating** - shrink `&mut` and side effects on observers.
5. **Local helpers from existing APIs** - no new core API without asking.
6. **Trim tests** - structure assertions in; plumbing tests out.
7. **Constants and docs** - top-level shape constants; module `//!` with emits +
   progress contract; section banners; short why-not-what comments only.

## Anti-patterns (push back)

- Nested `if/else` returning bool flags instead of early `return false`
- `contains` / prefix parse for fixed grammars that should be exact
- Negative-index peeks after partial consume
- Copy-pasted open/close or closer+interior+eat sequences
- `Option` chains that only exist because a conversion was made fallible for no reason
- Feature logic scattered into shared modules
- Tests that re-validate framework APIs or only check the outer lossless span
- New public helpers "for cleanliness" used once
- Module docs that only say "parses X" with no contract / emits / failure policy
- Wall of inline comments that should be structure or better names
- Mismatched or copy-pasted guard comments (`at_block` on heading code)
- Inconsistent section divider styles within one crate

## Tone when applying

Be direct. Prefer a smaller, clearer design over a polished version of the same
messy idea. Preserve behavior unless the user asked to change it; if a cleaner
structure needs a tiny behavior fix for correctness (e.g. exact ROD value), do
it and say so.
