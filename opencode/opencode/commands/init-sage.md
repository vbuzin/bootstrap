---
description: "Scaffold the current project for the sage workflow: docs/adr, open-questions, session journal, AGENTS.md."
agent: "sage"
---

Scaffold this project for the sage workflow. Check what exists first and
only create what is missing — never overwrite existing files without
asking.

Check current state:
!eza -la docs/ 2>/dev/null || echo "(no docs/ directory)"
!test -f AGENTS.md && echo "AGENTS.md exists" || echo "AGENTS.md missing"

Then, for each item below, create it only if it does not already exist:

1. **`docs/adr/` directory** containing:
   - `0000-template.md` — the ADR template from the adr-authoring skill,
     marked clearly as a template (not a real ADR).
   - `README.md` — the ADR index, initially empty (just the header and
     an empty table).

2. **`docs/open-questions.md`** — initially containing the two-section
   structure (Active, Resolved) with a short note explaining the
   convention.

3. **`docs/sessions/`** directory with a `.gitkeep` file (so git tracks
   the empty directory).

4. **`AGENTS.md`** at the project root — only if missing. Use
   opencode's built-in `/init` if this repo has enough content to
   analyse; otherwise create a minimal skeleton with:
   - Project name and one-line description (ask me if unclear).
   - Tech stack.
   - Non-negotiables placeholder.
   - Link to `docs/adr/README.md`.
   - Link to `docs/open-questions.md`.

5. **`.opencode/` directory** — only if the project needs local agent
   or command overrides (rare; skip unless I ask).

After scaffolding, show me what you created. Do not start a real sage
session until I confirm the scaffold looks right.
