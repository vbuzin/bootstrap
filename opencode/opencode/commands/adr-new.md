---
description: "Draft a new ADR for a decision reached in the current session. Pass the ADR title as argument."
agent: "sage"
argument-hint: "<short title for the decision>"
---

Draft a new ADR titled: $ARGUMENTS

Follow the Nygard-style template from the adr-authoring skill. Do the
following:

1. **Pick the next ADR number.** Look at `docs/adr/` and use the next
   unused four-digit number. 0000 is reserved for templates — start at
   0001.
   !fd --type f --extension md . docs/adr/ 2>/dev/null | sort | tail -n 3 || echo "(no ADRs yet)"

2. **Choose a filename slug** from the title — short, lowercase,
   hyphen-separated. Filename: `docs/adr/NNNN-slug.md`.

3. **Start the ADR in Draft status.** Fill in:
   - Title, Status (Draft), Date (today), Deciders.
   - Context — based on the current session's discussion.
   - Decision — what we just decided, stated precisely.
   - Alternatives considered — the other options we explored in this
     session, each with what it buys and why it was not picked.
   - Consequences — positive and negative both.
   - Rationale — why this option over the alternatives.
   - Links — related ADRs, external references, session journal entry.

4. **Show me the draft.** Before writing to disk, show the full ADR
   in chat. I will check it and either approve or redirect. Do not
   mark it Accepted in the draft — that happens in `/wrap`.

5. **Do not update the index yet.** The ADR stays Draft until the
   session wraps. `/wrap` will update `docs/adr/README.md` and strike
   the resolved question from `docs/open-questions.md`.

If we have not actually reached a decision yet, say so and refuse to
draft. Writing an ADR before Decide is premature.
