---
description: "Session-end: write dated session journal, update open-questions, confirm ADRs indexed."
agent: "sage"
---

You are wrapping up this sage session. Do the following, in order, and
show your work:

1. **Review what we discussed.** Summarise the session in two or three
   sentences. Not a transcript — the highlights only.

2. **List decisions reached.** Each one with an ADR reference if
   recorded, or "nothing final" if the session ended before Decide.

3. **List new open questions.** Anything we surfaced but did not
   resolve. Each should already be (or be about to be) added to
   `docs/open-questions.md`.

4. **Name the next concrete step.** One thing. Not a plan, a step.

5. **Write the session journal** to
   `docs/sessions/YYYY-MM-DD-HHMM.md` using the fixed format from the
   adr-authoring skill. Use today's date and current time. If the
   `docs/sessions/` directory does not exist, create it.

6. **Update `docs/open-questions.md`** if new questions emerged or if
   any were resolved this session.

7. **Verify ADRs written this session** are marked Accepted (not
   Draft), listed in `docs/adr/README.md`, and that any resolved
   questions point to them.

8. **Confirm** what you wrote and where. Then stop.

If any step reveals missing infrastructure (no `docs/adr/` directory,
no open-questions file), say so rather than silently skipping. Suggest
`/init-sage` to scaffold.
