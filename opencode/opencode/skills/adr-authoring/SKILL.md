---
name: "adr-authoring"
description: "Use when writing or revising Architecture Decision Records, maintaining docs/open-questions.md, updating AGENTS.md after a decision, or writing session journals. Covers the Nygard-style ADR template, ADR lifecycle (Draft/Accepted/Superseded), the session shape (Orient/Constrain/Explore/Decide/Record/Teach/Handoff), the open-questions backlog conventions, and the session journal format. Load when starting a sage session, when recording a decision, or when scaffolding ADR infrastructure for a new project."
---

# ADR authoring

This skill covers how sage records thinking. Three artefacts are in scope:

1. **ADRs** (`docs/adr/NNNN-slug.md`) — closed records of accepted decisions.
2. **The ADR index** (`docs/adr/README.md`) — a one-page browsable log of
   every ADR and its status.
3. **The open questions backlog** (`docs/open-questions.md`) — the living
   list of unresolved questions that may become ADRs.
4. **Session journals** (`docs/sessions/YYYY-MM-DD-HHMM.md`) — short notes
   written at the end of each sage session.

## The session shape

Every sage session follows this loop. Steps can move quickly or slowly
depending on the question, but none is skipped.

1. **Orient.** Read the most recent session journal, the current
   open-questions backlog, the project's AGENTS.md, and the last few git
   commits. Establish context before engaging with the new question. The
   `/orient` slash command bundles this into one call.
2. **Constrain.** Name the non-negotiables. Existing ADRs that bind the
   decision space. Project invariants. Resource and time limits. Team
   conventions. A great option that violates a constraint is wasted work —
   surface constraints before exploring options.
3. **Explore.** Two or three serious options, each with honest trade-offs.
   Not three where one is a straw man; three you would genuinely consider.
   For each: what it buys, what it costs, what it blocks, what it
   forecloses. Research actively — grep, compile, read docs, run probes.
4. **Decide.** Pick one. State the rationale explicitly. If you and the
   user disagree, say so; do not capitulate to reach closure faster.
5. **Record.** Write the ADR. Update the ADR index. Strike the resolved
   question from `docs/open-questions.md`. If the decision changes project
   invariants, update `AGENTS.md`.
6. **Teach.** Explain the underlying concepts, the relevant patterns, and
   what to watch out for during implementation. The user should leave with
   enough depth to write the code confidently, not just a spec to follow.
7. **Handoff.** The user decides whether and what to delegate to @dev (or
   @fixer). Sage does not dispatch execution.

Session end: write the journal, confirm open-questions is current, confirm
any ADRs written this session are marked Accepted and indexed. The `/wrap`
slash command bundles this.

## ADR template (Michael Nygard style)

Filename: `docs/adr/NNNN-short-slug.md`, where NNNN is the next four-digit
number in sequence (starting at 0001; 0000 is reserved for the template).

```markdown
# ADR NNNN — <Title>

- **Status:** Draft | Accepted | Superseded by [ADR MMMM](MMMM-slug.md)
- **Date:** YYYY-MM-DD
- **Deciders:** <names or roles>

## Context

Why is this decision needed? What is the situation? What forces are at
play — technical, organisational, constraints from other decisions?
Two to four paragraphs. Enough that a reader six months from now, with no
memory of the conversation, can understand why this question mattered.

## Decision

What we are doing. One or two sentences at most for the headline, then as
much detail as the decision itself requires. Be specific — "use PostgreSQL"
is not a decision, "use PostgreSQL 16 with the pgvector extension for
embedding storage, hosted on Azure Database for PostgreSQL flexible server"
is.

## Alternatives considered

The serious options that were not picked. For each:

### Option A — <name>

What it is. What it buys. Why it was not picked.

### Option B — <name>

Same shape.

This section is load-bearing. It captures the Explore step, which is often
more valuable than the decision itself when a reader asks "why didn't we
do X?" six months later.

## Consequences

What becomes easier because of this decision. What becomes harder. What
new work is created. What existing work is invalidated. What we are now
committed to. What we have explicitly foreclosed.

Positive and negative consequences both go here. An ADR with only positive
consequences is a sales pitch, not a decision record.

## Rationale

Why this option over the alternatives. Not a restatement of the pros, but
the judgment call — what weighed heaviest when the alternatives were
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
| Superseded | A later ADR replaces this one. Noted with a forward reference in the header. |

An ADR is **Accepted** when:

- The Decision, Alternatives Considered, Consequences, and Rationale
  sections are all written.
- The ADR index (`docs/adr/README.md`) has been updated with the new
  entry.
- The corresponding question (if any) has been struck from
  `docs/open-questions.md` with a reference to the ADR.

**ADRs are closed records after Accepted.** Never edited. If a decision
changes, write a new ADR that supersedes the old one, and add the forward
reference to the old one's status line — that is the only edit permitted
to an Accepted ADR.

## The ADR index

`docs/adr/README.md` is a one-page table of every ADR. Sage keeps it
current.

```markdown
# Architecture Decision Records

| # | Title | Status | Date |
|---|---|---|---|
| [0001](0001-something.md) | Use PostgreSQL for primary store | Accepted | 2026-04-13 |
| [0002](0002-other.md) | Adopt Railway-oriented error handling | Accepted | 2026-04-15 |
| [0003](0003-third.md) | Prefer composition over inheritance in domain layer | Superseded by [0007](0007-seventh.md) | 2026-04-20 |
```

Order by ADR number ascending. Keep the table simple — the ADR files
themselves carry the detail.

## Open questions backlog

`docs/open-questions.md` is the ADR backlog — not a notes dump. Every
unresolved question surfaced in a session is logged before the session
closes.

```markdown
# Open questions

Questions ordered by what they block. Most blocking first.

## Active

- **How should we model tenant isolation at the storage layer?**
  Blocks: user-onboarding work, the multi-tenancy ADR.
  Surfaced: 2026-04-13 session.

- **Do we need our own retry policy, or is the SDK's enough?**
  Blocks: resilience ADR.
  Surfaced: 2026-04-15 session.

## Resolved

- ~~What auth provider do we use?~~ — [ADR 0001](adr/0001-auth-provider.md), 2026-04-10.
- ~~Should domain types know about the database?~~ — [ADR 0003](adr/0003-layering.md), 2026-04-12.
```

When a question becomes an ADR, move it to Resolved with the ADR link and
date. Do not delete — the trail of how questions were resolved is itself
useful.

## Session journal

At the end of every sage session, write a short journal entry:
`docs/sessions/YYYY-MM-DD-HHMM.md`. Fixed format:

```markdown
# Session YYYY-MM-DD HH:MM — <one-line topic>

**Discussed:** Two or three sentences describing what we explored. Not a
transcript — the highlights.

**Decided:**
- Short list of decisions, each with an ADR reference where applicable.
  "Nothing final" is a valid entry when a session ended before Decide.

**New questions:**
- New unresolved questions that emerged, each with an open-questions.md
  reference if added to the backlog.

**Next:** One concrete next step. "Write the auth provider ADR" or
"implement the retry wrapper" or "return to this when we know the tenant
count".
```

Ten to twenty lines total. The goal is that a `rg` across
`docs/sessions/` surfaces past thinking quickly, and that reading the
most recent journal re-orients you at the start of the next session.

## Updating AGENTS.md after a decision

If a decision changes project invariants — tech stack choices, layering
rules, coding conventions, non-negotiables — update the project's
`AGENTS.md` in the same session as the ADR, not later. The ADR is the
record; `AGENTS.md` is the working context future agents load on every
session. Both need to be current for the decision to actually bind.

When editing `AGENTS.md`, link to the ADR that produced the change. That
way, a future agent reading the constraint can find the rationale without
asking.

## In-session todos vs open questions

Opencode has a built-in `todo` tool for in-session task tracking. That is
the right place for ephemeral "remember to rename this variable" or "run
the tests once this is done" items — things that matter within the
session and do not survive it.

`docs/open-questions.md` is for genuinely unresolved questions that may
become ADRs — things that matter across sessions. Do not conflate the
two. The backlog is not a todo list.
