---
description: "Session-start: load recent session journal, open questions, AGENTS.md, and recent git activity."
agent: "sage"
---

You are starting a new sage session. Before engaging with any new
question, re-establish context from the previous sessions.

Read the most recent session journal (if any):
!fd --max-depth 2 --type f --extension md . docs/sessions/ 2>/dev/null | sort | tail -n 1 | xargs -I {} cat {} 2>/dev/null || echo "(no session journal found)"

The current open questions backlog:
@docs/open-questions.md

The project's AGENTS.md:
@AGENTS.md

Recent git activity:
!git log --oneline -10 2>/dev/null || echo "(not a git repository)"

Current ADR index (if present):
!cat docs/adr/README.md 2>/dev/null || echo "(no ADR index yet — run /init-sage to scaffold)"

Based on the above, give me a short orientation: where we left off, what
is currently blocking, and what seems most worth tackling today. Then
wait for my direction on which question to work on.
