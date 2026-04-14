---
description: "Session-end (full mode only): write journal, update open questions, confirm ADRs indexed."
agent: "sage"
---

First, verify this is a **full-mode** session.

Check AGENTS.md frontmatter:
!test -f AGENTS.md && head -10 AGENTS.md || echo "(no AGENTS.md)"

If `sage-mode: full` is not declared, this is a light-mode session.
Respond:

> "This is a light-mode session — nothing to record. If you'd like to
> promote this project to full mode, add `sage-mode: full` to the
> AGENTS.md frontmatter and we can set up the scaffolding."

Then stop.

## Full-mode wrap

If this is full mode, proceed:

1. **Review what we discussed.** Summarise the session in two or three
   sentences. Highlights only, not a transcript.

2. **List decisions reached.** Each with an ADR reference if recorded,
   or "nothing final" if the session ended before Decide.

3. **List new open questions.** Anything surfaced but not resolved,
   each already added (or about to be added) to the backlog.

4. **Name the next concrete step.** One step, not a plan.

5. **Write the session journal** using the fixed format from the
   adr-authoring skill. Use today's date and current time for the
   filename. Discover the session journal path from AGENTS.md if
   specified; otherwise use `docs/sessions/YYYY-MM-DD-HHMM.md`.
   Create the directory if it does not exist.

6. **Update the open questions backlog** if new questions emerged or
   any were resolved this session. Discover its path from AGENTS.md
   if specified; otherwise use `docs/open-questions.md`.

7. **Verify ADRs written this session** are marked Accepted (not
   Draft), listed in the ADR index, and that any resolved questions
   point to them.

8. **Confirm** what you wrote and where. Then stop.

If any step reveals missing infrastructure (no ADR directory, no
open-questions file), say so rather than silently skipping. Ask
the user where these should live and update AGENTS.md accordingly.
