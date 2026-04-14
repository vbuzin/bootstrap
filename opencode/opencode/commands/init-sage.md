---
description: "Promote the current project to sage full mode: scaffold ADRs, open questions, sessions, and add sage-mode: full to AGENTS.md."
agent: "sage"
---

You are promoting this project to **full mode**. This means:

- Adding `sage-mode: full` to the project's `AGENTS.md` frontmatter.
- Scaffolding the infrastructure for persistent records.
- Confirming with the user where each artefact should live, because
  not every project uses the default `docs/adr/` etc.

Check current state:
!eza -la docs/ 2>/dev/null || echo "(no docs/ directory)"
!test -f AGENTS.md && echo "AGENTS.md exists" || echo "AGENTS.md missing"

## Step 1 — Confirm paths with the user

Before writing anything, ask the user (if unclear) where these three
things should live:

- **ADR directory** (default: `docs/adr/`)
- **Open questions file** (default: `docs/open-questions.md`)
- **Session journals directory** (default: `docs/sessions/`)

If the user is ambivalent, use the defaults. If they have a different
convention (e.g. `decisions/`, `rfc/`), use theirs. Record the choice
for this session.

## Step 2 — Scaffold the paths chosen

For each item, create it only if it does not already exist:

1. **ADR directory** containing:
   - `0000-template.md` — the Nygard template from the adr-authoring
     skill, clearly marked as a template.
   - `README.md` — the ADR index, initially empty (header plus empty
     table).

2. **Open questions file** — initially containing the Active/Resolved
   two-section structure with a short note explaining the convention.

3. **Session journals directory** — with a `.gitkeep` file so git
   tracks the empty directory.

## Step 3 — Update or create AGENTS.md

- **If AGENTS.md exists**: add `sage-mode: full` to the frontmatter.
  If the file has no frontmatter, add a frontmatter block at the
  top. If AGENTS.md specifies custom paths, ensure they match what
  was scaffolded. Add or update a "Governing records" section
  linking to the ADR index, open questions, and sessions.

- **If AGENTS.md does not exist**: create a minimal skeleton:

  ```markdown
  ---
  sage-mode: full
  ---

  # <Project name>

  <one-line description — ask the user if unclear>

  ## Tech stack

  <ask the user, or leave a placeholder>

  ## Non-negotiables

  <empty for now; populated as invariants are decided>

  ## Governing records

  - [Architecture Decision Records](<adr-dir>/README.md)
  - [Open questions](<questions-path>)
  - [Session journals](<sessions-dir>/)
  ```

## Step 4 — Show and confirm

Show the user everything you created. List the paths, show the
AGENTS.md diff or full file, confirm `sage-mode: full` is in place.

Do not start a real full-mode session until the user confirms the
scaffold looks right.

## Notes

- `/init-sage` is the **only** way a project gets promoted to full
  mode. Merely creating `docs/adr/` is not enough — sage looks for
  `sage-mode: full` in AGENTS.md frontmatter as the authoritative
  signal.
- To demote a project back to light mode, remove `sage-mode: full`
  from AGENTS.md. The existing ADRs and backlog stay where they
  are; sage just stops writing new ones.
