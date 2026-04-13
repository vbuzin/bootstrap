---
description: "List all ADRs in the current project with status and date."
agent: "sage"
---

Show a summary of all ADRs in this project.

Read the ADR index if present:
@docs/adr/README.md

If the index is missing or stale, enumerate the ADR files directly:
!fd --type f --extension md . docs/adr/ 2>/dev/null | sort || echo "(no ADRs yet)"

Present the list grouped by status (Accepted, Draft, Superseded). For
each ADR, one line: `NNNN — title — status — date`. If the index is
out of sync with the files, say so and offer to update it.
