---
name: "adr-authoring"
description: "Use in full-mode sage sessions when writing or revising Architecture Decision Records, maintaining the open questions backlog, updating AGENTS.md after a decision, or writing session journals. Covers the two sage modes (full vs light), the Nygard-style ADR template, ADR lifecycle (Draft/Accepted/Superseded), the session shape, the backlog conventions, the session journal format, and how to handle projects that use non-standard paths for their records."
---

# ADR authoring

This skill covers how sage records thinking. It applies in **full mode**
only — light-mode sessions do not produce artefacts.

## Sage has two modes

**Full mode** — declared by `sage-mode: full` in the project's
`AGENTS.md` frontmatter. Full mode maintains persistent records:

- **ADRs** — closed records of accepted decisions.
- **ADR index** — a browsable one-page log.
- **Open questions backlog** — the living list of unresolved questions.
- **Session journals** — short notes written at the end of each session.

**Light mode** — the default when `sage-mode: full` is absent or no
`AGENTS.md` exists. Light mode uses the same thinking discipline but
writes nothing. This skill is still useful as a reference in light mode,
but its instructions for writing artefacts do not apply. Commands like
`/adr-new`, `/wrap`, and `/question-add` refuse in light mode.

## Path conventions are per-project

The defaults below (`docs/adr/`, `docs/open-questions.md`,
`docs/sessions/`) are conventions, not requirements. A project may use
any structure — `decisions/`, `rfc/`, `notes/adrs/`, a flat file, an
external wiki. Sage discovers the convention from the project's
`AGENTS.md` prose. Look for a "Governing records" section or equivalent.

If paths are not specified and sage is entering full mode for the
first time on a project, ask once and remember for the session. Do not
assume.

Throughout this skill, `<adr-dir>`, `<questions>`, and `<sessions>`
mean "whatever paths this project uses". The defaults are standard but
not sacred.

## The session shape — full mode

Seven steps. Never skipped, pace varies with the question.

1. **Orient.** Read the most recent session journal, the open questions
   backlog, `AGENTS.md`, and the last few git commits. Re-establish
   context before engaging with the new question.

2. **Constrain.** Name the non-negotiables — existing ADRs that bind
   the decision space, project invariants, resource limits, deadlines,
   team conventions.

3. **Explore.** Two or three serious options, each with honest
   trade-offs. Research actively — grep, compile, read docs, run probes.

4. **Decide.** Pick one. State the rationale explicitly. Push back if
   you disagree.

5. **Record.** Write the ADR. Update the ADR index. Strike the resolved
   question from the backlog. Update `AGENTS.md` if invariants change.

6. **Teach.** Explain concepts, patterns, pitfalls. Equally weighted
   with the decision itself.

7. **Handoff.** The user decides whether and what to delegate to @dev.

Session end: write the journal, confirm the backlog is current, confirm
ADRs are Accepted and indexed. The `/wrap` slash command bundles this.

## The session shape — light mode

Five steps. Same discipline, no artefacts. No Orient-from-journal
(none exist), no Record, no Handoff ceremony.

1. **Orient** — read `AGENTS.md` if present, git log, establish context.
2. **Constrain** — whatever real constraints apply.
3. **Explore** — unchanged.
4. **Decide** — unchanged.
5. **Teach** — unchanged. Still equally weighted.

## ADR template (Nygard-style)

Filename: `<adr-dir>/NNNN-short-slug.md`, where NNNN is the next
four-digit number in sequence (starting at 0001; 0000 reserved for
the template).

```markdown
# ADR NNNN — <Title>

- **Status:** Draft | Accepted | Superseded by [ADR MMMM](MMMM-slug.md)
- **Date:** YYYY-MM-DD
- **Deciders:** <names or roles>

## Context

Why is this decision needed? What is the situation? What forces are at
play — technical, organisational, constraints from other decisions?
Two to four paragraphs. Enough that a reader six months from now can
understand why this question mattered.

## Decision

What we are doing. One or two sentences for the headline, then as much
detail as the decision requires. Be specific — "use PostgreSQL" is not
a decision, "use PostgreSQL 16 with the pgvector extension on Azure
Database for PostgreSQL flexible server" is.

## Alternatives considered

The serious options that were not picked. For each:

### Option A — <n>

What it is. What it buys. Why it was not picked.

### Option B — <n>

Same shape.

This section is load-bearing. It captures the Explore step, which is
often more valuable than the decision itself six months later when a
reader asks "why didn't we do X?".

## Consequences

What becomes easier. What becomes harder. What new work is created.
What existing work is invalidated. What we are now committed to. What
we have explicitly foreclosed.

Positive and negative both. An ADR with only positive consequences is
a sales pitch, not a decision record.

## Rationale

Why this option over the alternatives. Not a restatement of the pros —
the judgment call, what weighed heaviest when the alternatives were
otherwise comparable.

## Links

- Related ADRs (supersedes, superseded by, related to).
- External references — docs, issues, discussions.
- The session journal entry from the session this ADR came out of.
```

## ADR lifecycle

| Status | Meaning |
|---|---|
| Draft | Being written in-session — not yet binding. |
| Accepted | Decision is final — file is closed, no further edits. |
| Superseded | A later ADR replaces this one, noted with a forward reference. |

An ADR is **Accepted** when:

- The Decision, Alternatives Considered, Consequences, and Rationale
  sections are written.
- The ADR index has been updated.
- The corresponding question has been struck from the open questions
  backlog with a reference to the ADR.

**ADRs are closed records after Accepted.** Never edited. If a decision
changes, write a new ADR that supersedes the old one, and add the
forward reference to the old one's status line — that is the only edit
permitted to an Accepted ADR.

## The ADR index

`<adr-dir>/README.md` is a one-page table of every ADR. Sage keeps it
current.

```markdown
# Architecture Decision Records

| # | Title | Status | Date |
|---|---|---|---|
| [0001](0001-something.md) | Use PostgreSQL for primary store | Accepted | 2026-04-13 |
| [0002](0002-other.md) | Adopt Railway-oriented error handling | Accepted | 2026-04-15 |
| [0003](0003-third.md) | Composition over inheritance in domain | Superseded by [0007](0007-seventh.md) | 2026-04-20 |
```

Order by ADR number ascending.

## Open questions backlog

The living list of unresolved questions. Every unresolved question
surfaced in a full-mode session is logged before the session closes.

```markdown
# Open questions

Questions ordered by what they block. Most blocking first.

## Active

- **How should we model tenant isolation at the storage layer?**
  Blocks: user-onboarding work, the multi-tenancy ADR.
  Surfaced: 2026-04-13 session.

## Resolved

- ~~What auth provider do we use?~~ — [ADR 0001](adr/0001-auth-provider.md), 2026-04-10.
```

When a question becomes an ADR, move it to Resolved with the ADR link
and date. Do not delete — the trail of how questions were resolved is
itself useful.

## Session journal

At the end of every full-mode session, write a short journal entry.
Default path: `<sessions>/YYYY-MM-DD-HHMM.md`. Fixed format:

```markdown
# Session YYYY-MM-DD HH:MM — <one-line topic>

**Discussed:** Two or three sentences on what was explored.

**Decided:**
- Short list of decisions, each with an ADR reference.
  "Nothing final" is valid if the session ended before Decide.

**New questions:**
- New unresolved questions that emerged, each with an open-questions
  reference if added to the backlog.

**Next:** One concrete next step.
```

Ten to twenty lines total. Greppable with `rg`, scannable with `bat`.

## Updating AGENTS.md after a decision

If a decision changes project invariants — tech stack choices, layering
rules, non-negotiables — update the project's `AGENTS.md` in the same
session as the ADR, not later. The ADR is the record; `AGENTS.md` is
the working context future agents load. Both must be current for the
decision to bind.

Link from `AGENTS.md` to the ADR that produced the change.

## In-session todos vs open questions

Opencode's built-in `todo` tool tracks ephemeral, in-session tasks —
"remember to rename this variable", "run tests when this lands". The
open questions backlog is for genuinely unresolved questions that may
become ADRs — things that matter across sessions. Do not conflate the
two.
