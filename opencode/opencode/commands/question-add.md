---
description: "Add an open question to docs/open-questions.md. Pass the question as argument."
agent: "sage"
argument-hint: "<the question, as a short sentence>"
---

Add this open question to the project backlog: $ARGUMENTS

Current backlog:
@docs/open-questions.md

Do the following:

1. **Check for duplicates.** If a similar question already exists in
   Active, say so and ask whether to merge or keep separate.

2. **Add the question to the Active section** with:
   - The question itself as the bold headline.
   - What it blocks (other work, other ADRs, upcoming decisions).
   - Date surfaced (today).

3. **Place it in priority order.** The most blocking question goes at
   the top of Active. Ask me where this one fits if it is not obvious.

4. **Show me the updated file** before writing.

If `docs/open-questions.md` does not exist, say so and suggest
`/init-sage` to scaffold the project first.
