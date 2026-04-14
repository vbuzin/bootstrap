---
description: "Draft a new ADR for a decision reached in the current session (full mode only)."
agent: "sage"
argument-hint: "<short title for the decision>"
---

First, verify this is a **full-mode** session.

Check AGENTS.md frontmatter:
!test -f AGENTS.md && head -10 AGENTS.md || echo "(no AGENTS.md)"

If `sage-mode: full` is not declared, respond:

> "This is a light-mode session — we don't record ADRs here. If this
> decision is worth capturing, promote the project to full mode by
> adding `sage-mode: full` to AGENTS.md frontmatter, then re-run this
> command."

Then stop.

## Full-mode ADR drafting

Draft a new ADR titled: $ARGUMENTS

Follow the Nygard-style template from the adr-authoring skill.

1. **Discover the ADR directory** from AGENTS.md if specified;
   otherwise default to `docs/adr/`.
   !test -f AGENTS.md && grep -A2 -i "governing records\|adr" AGENTS.md | head -10 || true

2. **Pick the next ADR number.** Look at the ADR directory and use
   the next unused four-digit number. 0000 is reserved for templates —
   start at 0001.
   !fd --type f --extension md . docs/adr/ 2>/dev/null | sort | tail -n 3 || echo "(no ADRs yet at default path)"

3. **Choose a filename slug** from the title — short, lowercase,
   hyphen-separated. Filename: `<adr-dir>/NNNN-slug.md`.

4. **Start the ADR in Draft status.** Fill in Title, Status (Draft),
   Date (today), Deciders, Context, Decision, Alternatives Considered,
   Consequences, Rationale, Links.

5. **Show me the draft.** Before writing to disk, show the full ADR
   in chat. I will approve or redirect. Do not mark Accepted in the
   draft — that happens in `/wrap`.

6. **Do not update the index yet.** The ADR stays Draft until the
   session wraps. `/wrap` will update the ADR index and strike the
   resolved question from the open questions backlog.

If we have not actually reached a decision yet, say so and refuse to
draft. Writing an ADR before Decide is premature.
