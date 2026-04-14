---
description: "Session-start: detect sage mode (full vs light), load context, and orient."
agent: "sage"
---

You are starting a new sage session. Detect the mode, then establish
context.

## Mode detection

Check for `AGENTS.md` and read its frontmatter if any:
!test -f AGENTS.md && head -20 AGENTS.md || echo "(no AGENTS.md)"

Rule:
- `AGENTS.md` exists AND contains `sage-mode: full` in frontmatter →
  **full mode**.
- Otherwise → **light mode**.

State the mode clearly in your opening line, and offer the one-word
override. Examples:

> "Working in **full** mode (sage-mode: full in AGENTS.md). Say `light`
> to switch for this session."

> "Working in **light** mode (no sage-mode declared). Say `full` to
> switch — I'll need to know where ADRs, open questions, and session
> journals should live before I can record anything."

## Context loading

The project's AGENTS.md (if present):
@AGENTS.md

Recent git activity:
!git log --oneline -10 2>/dev/null || echo "(not a git repository)"

**Only if in full mode**, additionally load:

Most recent session journal (discover path from AGENTS.md if it
specifies; otherwise try the default):
!fd --max-depth 3 --type f --extension md . docs/sessions/ 2>/dev/null | sort | tail -n 1 | xargs -I {} cat {} 2>/dev/null || echo "(no session journal found at default path — check AGENTS.md for custom location)"

Open questions backlog (default path):
!cat docs/open-questions.md 2>/dev/null || echo "(no open questions file at default path — check AGENTS.md for custom location)"

ADR index (default path):
!cat docs/adr/README.md 2>/dev/null || echo "(no ADR index at default path)"

## Orientation

Based on the above, give a short orientation:

- Which mode you're in and why.
- (Full mode) Where we left off, what's blocking, what seems worth
  tackling today.
- (Light mode) What the project appears to be about from AGENTS.md
  and git history, and what you're ready to think through.

Then wait for the user to direct the session.
