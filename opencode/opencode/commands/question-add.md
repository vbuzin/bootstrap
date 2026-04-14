---
description: "Add an open question to the project backlog (full mode only)."
agent: "sage"
argument-hint: "<the question, as a short sentence>"
---

First, verify this is a **full-mode** session.

Check AGENTS.md frontmatter:
!test -f AGENTS.md && head -10 AGENTS.md || echo "(no AGENTS.md)"

If `sage-mode: full` is not declared, respond:

> "This is a light-mode session — there is no persistent backlog to
> add to. If this question matters beyond this session, promote the
> project to full mode by adding `sage-mode: full` to AGENTS.md
> frontmatter."

Then stop.

## Full-mode question add

Add this open question to the backlog: $ARGUMENTS

Discover the open-questions file path from AGENTS.md if specified;
otherwise default to `docs/open-questions.md`.

Current backlog (default path):
@docs/open-questions.md

Do the following:

1. **Check for duplicates.** If a similar question exists in Active,
   say so and ask whether to merge or keep separate.

2. **Add the question to Active** with:
   - The question itself as the bold headline.
   - What it blocks (other work, other ADRs, upcoming decisions).
   - Date surfaced (today).

3. **Place it in priority order.** Most blocking at the top of Active.
   Ask where it fits if not obvious.

4. **Show me the updated file** before writing.

If the open-questions file does not exist, ask where it should live
and offer to create it plus update AGENTS.md to point at it.
