# sage-setup — an opencode configuration for disciplined thinking

This is a personal opencode configuration built around the idea that
**architectural thinking and everyday coding are different activities
that deserve different tools**. It introduces a dedicated thinking
partner agent called **sage** alongside a streamlined coding agent
called **dev**, plus the supporting infrastructure — skills, slash
commands, and a memory model — that make both modes work well.

It is designed to be read end-to-end by anyone who wants to understand
why it is shaped the way it is, not just how to install it. If you are
here to install, skip to [Installation](#installation). Everything
else is rationale and reference.

---

## Table of contents

1. [The idea](#the-idea)
2. [The two modes](#the-two-modes)
3. [The agents](#the-agents)
4. [The skills](#the-skills)
5. [The slash commands](#the-slash-commands)
6. [Memory model](#memory-model)
7. [Permissions and data flow](#permissions-and-data-flow)
8. [File layout](#file-layout)
9. [Installation](#installation)
10. [Daily workflow](#daily-workflow)
11. [Customisation](#customisation)
12. [Changing models](#changing-models)
13. [Adding your own agents, skills, commands](#adding-your-own-agents-skills-commands)
14. [What's deliberately excluded](#whats-deliberately-excluded)
15. [Troubleshooting](#troubleshooting)

---

## The idea

Most coding agent configurations treat the agent as a single entity
that does everything: brainstorms, plans, codes, debugs, tests, reviews.
That works for quick tasks. It stops working when the task involves a
load-bearing decision — something the user wants to think through
properly, explore alternatives for, and record so their future self can
understand why a particular path was chosen.

The pain is familiar to anyone who has built something non-trivial with
an LLM coding assistant: you and the model talk through three
approaches, you pick one, the model writes the code, and six months
later you look at the code and think "why did we do it this way?" and
neither you nor the next agent session has any memory of the reasoning.
The decision lives in a chat log that has been compacted into
non-existence.

This configuration is built on a different premise: **thinking should
produce artefacts**, and those artefacts should be plain, editable,
greppable files in the project, not chat history or a vector database.
The thinking agent (sage) is structurally different from the coding
agent (dev) — different model posture, different permissions, different
tools — because conflating them produces mediocre versions of both.

At the same time, not every project deserves the ceremony. A scratch
repo, a quick exploration, a one-off prototype — these need a thought
partner, not a record-keeping bureaucracy. So sage has two modes, and
the boundary is explicit and trivially overridable.

### Origins

This setup is a generalised version of a personal working agreement
originally written for a Rust learning project. The original was
tightly coupled to Rust, to the specific project, and to "Cowork" (a
Claude-based thinking partner). The generalisation strips those
couplings and ports the shape to opencode, where sage plays the
thinking-partner role and dev plays the implementation role.

The seven-step session shape (Orient, Constrain, Explore, Decide,
Record, Teach, Handoff) comes directly from the original agreement.
The ADR format is Michael Nygard's. The rest — the permission model,
the two-mode split, the skill-and-command layer — is opencode-native.

---

## The two modes

Sage operates in one of two modes, detected at session start and
stated explicitly in its opening message.

### Full mode

Full mode maintains persistent records. You are in full mode when the
project's `AGENTS.md` frontmatter declares it:

```yaml
---
sage-mode: full
---
```

In full mode, sage writes ADRs, maintains an open questions backlog,
and produces session journals. The seven-step loop applies. The user
is treating architectural decisions as load-bearing, and the project
has the infrastructure (paths, conventions) to hold them.

Full mode is the right default for:

- Projects with multiple contributors or a future you that will return
  to the code months later.
- Anything with non-trivial architecture decisions (data model,
  layering, external integration boundaries).
- Learning projects where the point is to understand *why*, not just
  *what*.
- Production code you want to be able to reason about in six months
  without having to re-derive the reasoning.

### Light mode

Light mode uses the same thinking discipline with no artefacts. You
are in light mode by default — when `sage-mode: full` is absent from
`AGENTS.md`, or when there is no `AGENTS.md` at all.

In light mode, sage still explores options, still pushes back, still
teaches, still refuses to write code files. It just does not write
anything down afterwards. The five-step loop applies: Orient,
Constrain, Explore, Decide, Teach. No Record. No Handoff ceremony.
You tab to dev when you want to write code, same as always.

Light mode is the right default for:

- Scratch projects, quick ideation, throwaway repos.
- Projects where you are not the primary author and do not want to
  impose structure.
- Any session where the decision is not worth a written record.
- Projects that have not yet earned the ceremony.

### Mode detection and override

At `/orient`, sage detects the mode and states it:

> "Working in **full** mode (sage-mode: full in AGENTS.md). Say
> `light` to switch for this session."

> "Working in **light** mode (no sage-mode declared). Say `full` to
> switch — I'll need to know where ADRs should live."

The override is one word and sticks for the session. If you say
`full` in a project without scaffolding, sage asks where ADRs, open
questions, and session journals should live before proceeding — it
does not assume `docs/adr/`, because not every project uses that
convention.

### Promoting a project from light to full

Run `/init-sage` once in the project. It:

1. Asks where you want ADRs, open questions, and session journals to
   live.
2. Scaffolds those paths with a template ADR (`0000-template.md`),
   an empty ADR index (`README.md`), and an empty open-questions file.
3. Adds `sage-mode: full` to the project's `AGENTS.md` frontmatter,
   creating `AGENTS.md` if it does not exist.
4. Confirms with you before writing anything.

After that, every new sage session in that project starts in full
mode automatically.

### Demoting from full back to light

Remove `sage-mode: full` from `AGENTS.md`. The existing ADRs and
backlog stay where they are — sage just stops writing new ones.
Decommissioning is cheap because nothing is locked in.

---

## The agents

Five custom agents plus opencode's hidden system agents (compaction,
title, summary). The built-in `build`, `plan`, `general`, and
`explore` agents are disabled — their roles are covered by the
custom set.

### sage (primary)

**Role**: disciplined thinking partner. Brainstorming, architecture,
exploring options, reaching decisions, writing ADRs (in full mode),
teaching concepts. Never writes code.

**Model**: `xai/grok-4.20-0309-reasoning` — the reasoning tier is
where sage earns its keep. Temperature `0.75`, high enough for honest
exploration of alternatives, tight enough to stay coherent when
writing an ADR.

**Permissions**: path-scoped. Sage can edit anything under `docs/`
and `AGENTS.md` freely. Global opencode config edits (global skills,
agents, commands) require permission prompts. Code files (`.rs`,
`.py`, `.ts`, `.fs`, anything that isn't markdown) are physically
denied — sage *cannot* write code even if asked. Bash is allow-by-
default with deny rules for the big guns (`rm -rf /`, `sudo`, force
push, hard reset).

**Why it can't write code**: the discipline is the point. If sage
could write code, the pressure to "just do it" would gradually erode
the boundary between thinking and implementing. Permissions enforce
the boundary structurally so behavioural discipline doesn't have to.

**Hard rules** (baked into the prompt):

- Never write code files.
- ADRs are append-only once Accepted. Supersede, never edit.
- When editing `~/.config/opencode/AGENTS.md`, always show the diff
  before saving. That file affects sage's own behaviour across every
  future session — silent edits to load-bearing global config is how
  agents drift.
- Push back on decisions you disagree with. Agreement is cheap.
- If context is getting long, wrap proactively rather than losing a
  mid-decision to compaction.

### dev (primary)

**Role**: everyday driver. Reads code, writes code, refactors, runs
builds, investigates failures. Full tool access with minimal
friction — dev is the agent you spend most of your time with.

**Model**: `xai/grok-4.20-0309-reasoning`, temperature `0.55`.
Reasoning tier because dev often needs to trace through real code
paths, and fast models hallucinate details. Lower temperature than
sage because dev is executing, not exploring.

**Permissions**: full edit/write/bash with deny rules only for the
big guns. Trusted with everything.

**When dev hands back to sage**: the prompt instructs dev to suggest
tabbing to sage for anything load-bearing — "should we...", "what's
the right way to model...", "how do we want to handle..." — so that
architectural decisions don't get improvised mid-implementation.

**When dev dispatches to subagents**:

- `@scout` for read-only navigation — cheap, fast, keeps reasoning
  tokens for reasoning.
- `@fixer` for deep root-cause work or a refactor large enough to
  deserve its own context window.
- `@reviewer` for a pre-commit review pass.

### fixer (subagent)

**Role**: root-cause debugging and ruthless refactoring.

**Model**: `xai/grok-4.20-0309-reasoning`, temperature `0.4`.
Reasoning tier because debugging is where fast models confidently
invent fixes that don't work. Low-ish temperature for tighter
convergence on the real cause.

**Discipline**: find why before deciding how. Reproduce, trace, read,
then fix. Minimal correct change over clever rewrite. If the fix
needs more scope than requested, say so rather than silently
expanding. Verify with the reproduction case before and after.

### reviewer (subagent)

**Role**: read-only code review for correctness, security,
performance, maintainability, idiomaticity.

**Model**: `xai/grok-4.20-0309-reasoning`, temperature `0.3`. Very
low temperature because reviews should be deterministic in what they
flag — the same code should get the same review.

**Permissions**: hard deny on edit and write. Safe bash only (grep,
find, read, git log/show/diff/blame). Reviewer returns concerns
grouped by severity (blocking, should-fix, nit); it never edits.

### scout (subagent)

**Role**: fast, cheap, read-only codebase navigation. Grep, find,
read, safe git queries. Answers "where", "what calls", "find",
"list" questions — nothing else.

**Model**: `xai/grok-code-fast-1`, temperature `0.2`. Explicitly
pinned to the fast model. Without this, scout would inherit the
invoking primary's model (reasoning tier), defeating the point.

**Permissions**: write/edit/webfetch all deny. Bash restricted to an
explicit allow list of read-only commands.

**What it doesn't do**: analysis, opinions, reasoning chains. Scout
returns raw findings — paths, line numbers, minimal context — and
lets the caller reason. Saving reasoning-model tokens on navigation
work is the entire point.

### Disabled built-ins

- **build** — dispatcher role collapsed into dev.
- **plan** — thinking role collapsed into sage.
- **general** — replaced by dev for multi-step work; subagent
  dispatch pattern for parallel work isn't used.
- **explore** — replaced by scout (same role, custom prompt,
  explicit model pinning).

The hidden system agents (`compaction`, `title`, `summary`) are
left alone. Opencode uses them internally.

---

## The skills

Skills are loaded on demand based on their descriptions. Two are
included globally.

### adr-authoring

Covers the full-mode artefacts: session shape, Nygard ADR template,
lifecycle states (Draft/Accepted/Superseded), ADR index conventions,
open questions backlog format, session journal format. Handles
non-standard paths — sage discovers ADR/questions/sessions locations
from project `AGENTS.md` rather than assuming `docs/adr/`.

Loaded automatically in full-mode sessions; light mode may skip it.

### skill-authoring

The meta-skill. Covers the distinction between AGENTS.md rules
(always loaded), skills (on-demand), and slash commands
(user-triggered). Explains how to write descriptions that actually
trigger retrieval. States that sage owns skill authoring — deciding
*what belongs in a skill* is a structural judgment, which is exactly
what sage exists for.

---

## The slash commands

Seven global commands, all in `~/.config/opencode/commands/`.

| Command | Purpose | Mode |
|---|---|---|
| `/orient` | Session start — detect mode, load context | both |
| `/wrap` | Session end — write journal, update backlog | full only |
| `/adr-new <title>` | Draft a new ADR | full only |
| `/adr-list` | Show ADR index | full only |
| `/question-add <question>` | Add to open questions | full only |
| `/init-sage` | Promote project to full mode | either |
| `/skill-new <name> [global\|local]` | Create a new skill with structural review | both |

Commands that are full-mode only check AGENTS.md frontmatter first
and refuse with a clear message if the project is in light mode,
offering to promote via `/init-sage`.

---

## Memory model

Memory is layered and entirely file-based. No vector database, no
plugin, no LLM-powered memory extraction.

### Long-term, decision-grade

- `<adr-dir>/**` — ADRs, immutable once Accepted. The load-bearing
  record of why things are the way they are.
- `AGENTS.md` — current project invariants, tech stack, governing
  records. The context future agents load on every session.

Sage keeps both current.

### Mid-term, rolling

- `<sessions>/YYYY-MM-DD-HHMM.md` — one per full-mode session,
  written by `/wrap`. Ten to twenty lines: what was discussed, what
  was decided, what new questions emerged, what to tackle next.

This is the "what were we doing last week" layer. ADRs capture
conclusions; session journals capture the thinking around them.
Scannable with `rg`, readable with `bat`.

### In-session working memory

- Opencode's native compaction handles context-window management.
- Sage's discipline of recording decisions as they happen (rather
  than holding them in conversation state) is belt-and-braces: even
  if compaction drops mid-session context, the ADR draft is already
  on disk.
- Opencode's built-in `todo` tool handles ephemeral task tracking
  within a session — separate from the cross-session open questions
  backlog.

### Why files, not a vector DB

Files are greppable with `rg`, editable by hand, diffable in git,
and survive tool churn. A vector DB is opaque, needs a separate
service, and — when the agent captures something wrong, which it
will — fixing it means a web UI round-trip instead of editing a
line. The Obsidian-style tooling preference (ripgrep, fd, eza,
markdown-native) pointed directly at the file-based approach.

Cross-project semantic recall (*"I know I solved this in a
different repo once"*) is the one thing files cannot do cheaply. If
that specific need appears, `opencode-mem` is a reasonable addition
— installed only for that use case, not as the primary memory.

---

## Permissions and data flow

### Data flow

This configuration uses xAI as the only LLM provider. All model
calls go to xAI — sage, dev, fixer, reviewer, and scout. Nothing
is routed to Anthropic, OpenAI, opencode's servers, or any other
third party.

The only paths where data leaves your machine:

1. **The xAI API**, when any agent is invoked.
2. **Webfetch**, when an agent reads a URL (transparent — you see
   the fetch happen).
3. **Opencode's update notification**, `autoupdate: notify` —
   checks for a new version on startup. This is metadata only;
   your conversation is never involved.
4. **Manual `/share`**, which is off by default and requires
   explicit invocation.

No conversations are logged remotely. No code is uploaded.

### Permissions model

Opencode permissions are keyed by tool (`edit`, `write`, `bash`,
`webfetch`) and scoped by path or command pattern. Rules are
evaluated by specificity; more specific wins. Typical shape for
sage:

```yaml
edit:
  "*": "deny"           # default: deny everything
  "*.md": "ask"          # markdown at root: ask
  "docs/**": "allow"     # docs: allow
  "AGENTS.md": "allow"   # project AGENTS.md: allow
  ".opencode/agents/**": "allow"  # project-local: allow
  "~/.config/opencode/agents/**": "ask"  # global: ask
```

This means sage, asked to write a `.py` file, is refused by the
filesystem before the prompt discipline even kicks in. The
structural boundary is enforced at the permission layer.

Bash uses the same pattern but matches command prefixes:

```yaml
bash:
  "*": "allow"              # default: allow
  "rm -rf /*": "deny"        # deny recursive delete
  "rm -rf ~*": "deny"
  "sudo *": "deny"
  "git push*": "ask"         # ask for push
  "git reset --hard*": "ask"
  "git clean -fd*": "ask"
```

Sage has full command-line freedom for compile, test, grep, LSP
queries, etc. — necessary because sage researches actively during
the Explore step.

---

## File layout

```
~/.config/opencode/
├── opencode.jsonc              # provider, default_agent, compaction, autoupdate
├── tui.jsonc                   # theme
├── AGENTS.md                   # global personal rules (voice, conventions)
├── agents/
│   ├── sage.md                 # primary — disciplined thinking
│   ├── dev.md                  # primary — everyday driver
│   ├── fixer.md                # subagent — debug + refactor
│   ├── reviewer.md             # subagent — read-only review
│   └── scout.md                # subagent — read-only navigation
├── skills/
│   ├── adr-authoring/
│   │   └── SKILL.md
│   └── skill-authoring/
│       └── SKILL.md
└── commands/
    ├── orient.md
    ├── wrap.md
    ├── adr-new.md
    ├── adr-list.md
    ├── question-add.md
    ├── init-sage.md
    └── skill-new.md
```

Per-project (created by `/init-sage` when promoting to full mode):

```
<project>/
├── AGENTS.md                   # sage-mode: full in frontmatter
├── docs/                       # or wherever you choose
│   ├── adr/
│   │   ├── 0000-template.md
│   │   └── README.md           # ADR index
│   ├── open-questions.md
│   └── sessions/
│       └── .gitkeep
└── .opencode/                  # optional — for project-local overrides
```

Everything in `docs/` is committed to git. The ADRs, the index, the
open questions, and the session journals are all part of the
project's version history.

---

## Installation

### Prerequisites

- Opencode installed (`curl -fsSL https://opencode.ai/install | bash`
  or Homebrew).
- An xAI API key exported as `XAI_API_KEY` in your shell profile.
- Ripgrep, fd, eza, bat — optional but assumed by the global AGENTS.md
  tooling defaults.

### First install

```bash
# Back up any existing config
cp -r ~/.config/opencode ~/.config/opencode.bak.$(date +%Y%m%d) 2>/dev/null || true

# Copy everything in
cp -r global/opencode/* ~/.config/opencode/

# Verify
opencode agent list
```

You should see `sage` and `dev` as primaries, plus `fixer`,
`reviewer`, `scout` as subagents. The built-ins (`build`, `plan`,
`general`, `explore`) should be marked disabled.

### Updating from a previous install

If you already have a previous version of this setup, only the
files in the update package need to be copied — the rest is
unchanged. Overwrite the updated files in place:

```bash
cp -r agents/* ~/.config/opencode/agents/
cp -r skills/adr-authoring/* ~/.config/opencode/skills/adr-authoring/
cp -r commands/* ~/.config/opencode/commands/
cp project-template/AGENTS.md <wherever-you-keep-the-template>/
```

No restart needed — opencode re-reads config on each session start.

### Smoke test

```bash
cd /tmp && mkdir sage-test && cd sage-test && opencode
```

You should land in sage by default. Type `/orient`. It should report
"Working in **light** mode (no sage-mode declared)" because `/tmp/sage-test`
has no AGENTS.md. Say `full` and it should ask where records should
live. Say `light` again to switch back. Exit with `/exit` or Ctrl-D.

---

## Daily workflow

### Quick ideation (light mode)

```
$ cd ~/scratch/new-idea
$ opencode
> /orient
sage: Working in light mode (no sage-mode declared). Say `full` to
switch — I'll need to know where ADRs should live.
(Short orientation based on git log, if any.)

> I'm thinking about how to model the config layer for this. Options?
sage: (explores two or three approaches, honest trade-offs, pushes
back if something doesn't make sense, teaches the relevant concepts)

> OK, let's go with the first one.
sage: Right. Tab to @dev when you're ready to write it.
```

Nothing is recorded. The conversation is the artefact.

### Deliberate architecture session (full mode)

```
$ cd ~/projects/real-project
$ opencode
> /orient
sage: Working in full mode (sage-mode: full in AGENTS.md). Last
session was 2026-04-12 on the retry policy question. Three open
questions active; the most blocking is "how should tenant isolation
work at the storage layer". Git log shows work on the auth module.
What's on today?

> The tenant isolation question. Let's think it through.
sage: (Orient → Constrain → Explore with three serious options,
grepping the codebase for how tenancy is currently handled, fetching
docs on the SDK's row-level security support, running a probe query
→ Decide → Teach)

> Right, let's record that.
> /adr-new tenant-isolation-at-storage-layer
sage: (Drafts ADR 0007 in Nygard format based on the session,
shows it in chat for approval)

> Looks good.
sage: (Writes to docs/adr/0007-tenant-isolation-at-storage-layer.md,
still as Draft.)

> /wrap
sage: (Summarises the session, writes the journal to
docs/sessions/2026-04-13-1430.md, marks the ADR as Accepted, updates
docs/adr/README.md index, strikes the resolved question from
docs/open-questions.md pointing at ADR 0007.)

> Tab
(switches to @dev)
dev: Based on what sage just decided, I'll start with the row-level
security setup in the storage layer. Let me grep for where the
connection pool is configured first.
```

### Promoting a scratch project to full mode

```
$ cd ~/scratch/interesting-idea
$ opencode
> /orient
sage: Working in light mode...

> This is turning out to be worth thinking about properly.
> /init-sage
sage: I'll promote this to full mode. Where do you want records to
live? Defaults are docs/adr/, docs/open-questions.md, docs/sessions/
— hit enter to accept, or name alternatives.

> defaults are fine
sage: (Scaffolds docs/adr/0000-template.md, docs/adr/README.md,
docs/open-questions.md, docs/sessions/.gitkeep, adds sage-mode: full
to AGENTS.md frontmatter — creating AGENTS.md if missing.)

Confirmed. Next sage session will start in full mode.
```

---

## Customisation

### Global AGENTS.md (~/.config/opencode/AGENTS.md)

This is where voice, tone, conventions, and standing instructions
live. Applied to every session, every agent, every project. Current
contents include:

- Grok personality block (truth-seeking, witty, no corporate fluff)
- Preferred tooling (rg, fd, eza, bat, jq, yq)
- Engineering defaults (composable functions, quarantine externals,
  verify before claiming, explicit over clever)
- Session hygiene (don't silently deviate, show commands, diagnose
  before fixing)

Edit freely. Sage can edit this file with `allow` permission, but
its prompt requires showing the diff in chat before saving — so
silent drift is protected against.

### Project AGENTS.md

This is where project-specific rules live. At minimum:

```yaml
---
sage-mode: full   # or omit for light mode
---

# Project name
...
```

Recommended sections: Tech stack, Non-negotiables, Current focus,
Governing records (links to ADR index, open questions, sessions).
Sage reads this on every `/orient` and uses it to understand both
the project and the discipline level.

### Per-project overrides

Place agent files, skills, or commands under `.opencode/` in a
project root to override globals for that project only. Sage has
`allow` permission on `.opencode/` paths, so it can write these
without prompts. Global equivalents require `ask`.

---

## Changing models

All five custom agents specify their model explicitly in the
frontmatter. To change one, edit the file.

Current defaults:

| Agent | Model | Temperature | Rationale |
|---|---|---|---|
| sage | `xai/grok-4.20-0309-reasoning` | 0.75 | Reasoning for exploration; temp balances honest exploration against coherent ADR writing |
| dev | `xai/grok-4.20-0309-reasoning` | 0.55 | Reasoning because dev traces real code paths; temp low because dev executes |
| fixer | `xai/grok-4.20-0309-reasoning` | 0.4 | Reasoning because debugging is where fast models invent fixes |
| reviewer | `xai/grok-4.20-0309-reasoning` | 0.3 | Low temp for deterministic review findings |
| scout | `xai/grok-code-fast-1` | 0.2 | Fast model — scout doesn't reason, it reports |

To swap a model, edit the `model:` line in the agent's markdown file:

```yaml
---
model: "xai/whatever-the-new-reasoning-model-is-called"
---
```

The model ID format is `provider/model-id`. Run `opencode models` to
see available models for the providers you have configured.

### Using a different provider

Add the provider to `opencode.jsonc`:

```jsonc
{
  "provider": {
    "xai": { ... },
    "anthropic": {
      "options": { "apiKey": "{env:ANTHROPIC_API_KEY}" }
    }
  }
}
```

Then edit agent files to reference the new provider: e.g.
`model: "anthropic/claude-opus-4-6"`. Data flow changes accordingly
— if you add Anthropic, calls to Anthropic models route there.

### Reasoning effort knobs

For providers that expose reasoning effort as a parameter (OpenAI
reasoning models, some xAI tiers), opencode passes unknown
frontmatter fields straight through to the provider. Add e.g.
`reasoningEffort: "high"` to an agent's frontmatter to control it.

---

## Adding your own agents, skills, commands

### A new agent

```bash
opencode agent create
```

...or create a markdown file directly at
`~/.config/opencode/agents/<n>.md` (global) or
`.opencode/agents/<n>.md` (project). The file needs frontmatter
with `description`, `mode` (primary, subagent, or all), `model`, and
`permission`. See existing agents for the shape.

Keep the role narrow — "agents that do many things" trigger
unpredictably and dispatch inefficiently.

### A new skill

Use the `/skill-new <n> [global|local]` slash command. Sage will
have the structural conversation with you first:

1. Is a skill the right shape, or does this belong in AGENTS.md or
   a slash command?
2. What are the concrete triggers that should load it?
3. Is it one topic or several? If several, split.

Then it scaffolds `skills/<n>/SKILL.md` with quoted YAML
frontmatter. See the skill-authoring skill for detailed guidance.

### A new slash command

Create a markdown file at `~/.config/opencode/commands/<n>.md`
(global) or `.opencode/commands/<n>.md` (project). Frontmatter
supports `description`, `agent` (to pin the command to a specific
agent), `model` (to override), and `argument-hint` (shown in the
TUI when autocompleting).

The body is the prompt template. Use `$ARGUMENTS` for the full
argument string, `!command` to inject bash output, `@path/to/file`
to include file contents. **Always quote YAML values** — opencode's
frontmatter parser has crashed on unquoted values containing colons.

---

## What's deliberately excluded

- **opencode-mem** (vector DB memory). Skipped because files are
  greppable, editable, diffable, and fit the Obsidian-style
  tooling preference. If cross-project semantic recall becomes a
  real need, add it then.
- **MCP servers**. Opencode supports the protocol. Useful for
  Azure DevOps, Gmail, Obsidian vault, etc. — but nothing is
  installed by default. Add specific ones when a specific pain
  point appears.
- **Custom TypeScript tools** (`~/.config/opencode/tools/`).
  Overkill until bash and webfetch can't cover a workflow.
- **Plugins** (`~/.config/opencode/plugins/`). Opencode supports
  pre/post tool hooks in TypeScript. Useful for automatic formatter
  runs, session journal auto-append, etc. Not needed yet.
- **A separate tester agent**. Tests are written by dev or fixer
  inline. A fast-model tester is the wrong optimisation target —
  tests should catch real bugs, not just look plausible.
- **A separate coder agent**. Dev writes code directly. Dispatching
  to a dedicated coder for greenfield work was a holdover from
  coarser permission models; with path-scoped permissions it
  adds a Tab hop that doesn't pay off.
- **A dispatcher "build" agent**. Same reason.

---

## Troubleshooting

**Sage refuses to write a file I asked it to.** Almost certainly
the correct behaviour — sage's permissions deny everything outside
`docs/`, `AGENTS.md`, and `.opencode/`. Tab to dev if you want
code written.

**Opencode crashes on startup after editing a command.** Check for
unquoted colons in slash command frontmatter. The parser has a
known bug. Every value in this package is quoted; if you edit a
command, quote any new values you add.

**`@scout` is slow.** Verify it's actually running on
`grok-code-fast-1`. Subagents inherit the invoking agent's model
unless they specify their own. Scout specifies it explicitly; check
with `head -10 ~/.config/opencode/agents/scout.md`.

**ADR index is out of sync with the ADR files.** Sage should update
it on `/wrap`, but if a session ended without wrap, run `/adr-list`
— it will detect drift and offer to fix.

**Sage is in light mode but I want full mode in this project.**
Two options: (1) say `full` at `/orient` for a one-off override,
or (2) run `/init-sage` to promote the project permanently by
adding `sage-mode: full` to AGENTS.md.

**Sage is in full mode but I want light for this one session.**
Say `light` at `/orient`. The override sticks for the session.

**I want to move ADRs to a different folder.** Sage discovers ADR
locations from the project's `AGENTS.md`. Edit the "Governing
records" section to point at the new location, move the files,
and sage will use the new path from the next session onward.

**I want to reset and start fresh.** Your backup from the initial
install is at `~/.config/opencode.bak.YYYYMMDD`. Restore with
`rm -rf ~/.config/opencode && mv ~/.config/opencode.bak.YYYYMMDD ~/.config/opencode`.

**Reasoning-model costs are higher than expected.** Normal for
sage sessions — exploration is token-heavy. Mitigations:
(1) dispatch more navigation work to `@scout`, (2) keep sessions
focused on one question at a time and `/wrap` between, (3)
consider dropping sage's temperature slightly if exploration is
wandering, (4) use light mode for anything that doesn't earn the
ceremony. (5) switch to a cheaper model.
