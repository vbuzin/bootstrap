---
name: "skill-authoring"
description: "Use when creating, revising, or reviewing opencode skills (SKILL.md files under ~/.config/opencode/skills/ or .opencode/skills/). Covers the distinction between skills, AGENTS.md rules, and slash commands; frontmatter format; description quality as load-time relevance; single-topic scoping; supporting files; and the evolution discipline for keeping skills current rather than abandoned."
---

# Skill authoring

Opencode has three places to put guidance for agents. Choosing the right
one is the first and most important decision when writing a skill.

## The three-way distinction

**AGENTS.md** is always loaded. Every agent, every session, every project
(for project-level AGENTS.md) or every session full stop (for global
`~/.config/opencode/AGENTS.md`). Use it for:

- Voice and tone (British English, no fluff).
- Persistent conventions that apply everywhere (preferred tooling, naming
  conventions, FP lean).
- Standing instructions the user wants enforced by default.
- Project-specific invariants (tech stack, non-negotiables, links to
  governing ADRs).

**Skills** are loaded on demand. Opencode reads every SKILL.md at startup
and exposes them to agents via the skill tool's description. An agent
pulls a skill into context *when its description matches the current
task*. Use skills for:

- Specialised knowledge that is not needed every session.
- Workflows with their own shape (ADR authoring, skill authoring,
  release processes).
- Templates, checklists, and reference material an agent should consult
  when a specific type of work arises.
- Knowledge the user wants the agent to apply consistently when a topic
  comes up, but does not want taking up context space the rest of the
  time.

**Slash commands** are user-triggered. The user types `/something` and a
command template runs. Use commands for:

- Repeatable workflows the user wants to launch deliberately (`/orient`,
  `/wrap`, `/adr-new`).
- Actions that bundle a shell command's output into a prompt
  (`!git log -10` injection).
- Scaffolding operations (`/init-sage`, `/skill-new`).

**Rule of thumb**: if it's needed every session, AGENTS.md. If it's
needed when a topic arises, skill. If it's triggered by an explicit user
action, command.

When unsure, default to a skill. Skills are cheap — if the description is
good, they only load when relevant, so the cost of a skill that is
rarely needed is near zero. A rule stuffed into AGENTS.md that is only
rarely relevant wastes context on every session.

## Frontmatter

Opencode recognises only a small set of frontmatter fields for skills.
Unknown fields are silently ignored — do not invent new ones and expect
them to work.

Recognised fields:

- `name` — the skill identifier. Should match the directory name.
- `description` — what drives retrieval. See below.

Always quote values. Opencode has crashed on unquoted YAML frontmatter
containing colons — quoting everything is cheap insurance against the
parser.

```yaml
---
name: "skill-authoring"
description: "Use when creating or revising opencode skills..."
---
```

## Description quality is load-time relevance

The description is the entire mechanism by which a skill gets loaded. A
vague description — "helps with Rust" — will not trigger when it should.
A good description states the *triggers*: what user intents, question
shapes, or topics should pull this skill in.

Structure a description as:

1. **When to use it** — concrete triggers, not vague capability.
2. **What it covers** — the specific sub-topics inside.
3. **When it applies** — if there are clear cases ("load when starting a
   sage session", "load when recording a decision").

Compare:

- ❌ "Helps with architecture decisions."
- ✅ "Use when writing or revising Architecture Decision Records,
  maintaining docs/open-questions.md, updating AGENTS.md after a
  decision, or writing session journals. Covers the Nygard-style
  template, ADR lifecycle, session shape, and backlog conventions."

The second one names the artefacts and the operations. An agent reading
a list of skill descriptions can match "I am about to write a decision
record" to the second version immediately; the first version is
indistinguishable from a dozen other possible skills.

## One topic per skill

If a skill's description uses "and" repeatedly, split it. Omnibus skills
have two failure modes:

1. **They miss triggering.** A description too broad to match any
   specific query does not get loaded when it should.
2. **They over-trigger.** When they do load, they pull in a lot of
   irrelevant content because the skill covers five topics and only one
   is relevant.

Better: two focused skills that each cover one thing well. If two topics
are genuinely inseparable (same context, same workflow, same reference
material) they can share a skill — but the bar is "genuinely
inseparable", not "thematically related".

## Supporting files

A skill is a directory. `SKILL.md` is the entry point, but the directory
can contain supporting files — templates, examples, prompts, reference
tables — that `SKILL.md` references by path.

```
~/.config/opencode/skills/adr-authoring/
├── SKILL.md
├── template.md        (referenced from SKILL.md)
└── examples/
    └── 0001-example.md
```

Reference supporting files from SKILL.md using relative paths. Do not
duplicate their content inside SKILL.md — point to them.

## Global vs project-local skills

- **Global** — `~/.config/opencode/skills/NAME/SKILL.md`. Applies across
  every project. Reserved for reusable discipline (ADR authoring, skill
  authoring, testing conventions that apply to any codebase).
- **Project-local** — `.opencode/skills/NAME/SKILL.md` (committed to
  git). Applies only in that repo. Use for project-specific knowledge
  the whole team benefits from (the domain model, the auth flow, the
  deployment pipeline).

If a skill starts as project-local and proves useful in a second project,
promote it to global. Do not pre-emptively generalise — let two real
use cases pull the abstraction out.

## Evolution discipline

Skills should be revised when they are wrong, not abandoned. An abandoned
skill that still triggers is worse than no skill: it ships stale guidance
that the agent will follow because the agent trusts loaded skills.

Treat skills like ADRs' quieter cousin:

- Every edit to a skill is deliberate.
- Every edit is noted in the session journal with what changed and why.
- If a skill is no longer relevant, delete it. Do not leave it.
- Periodically (at least quarterly), read each skill and ask: is this
  still accurate? Is the description still well-tuned? Has the topic
  shifted?

## Review flow

When asked to review an existing skill, check:

1. **Triggering** — would the description actually match the cases where
   this skill should load? Are the triggers specific and concrete?
2. **Scope** — is it one topic, or has it grown into an omnibus?
3. **Accuracy** — does the content still reflect current practice?
4. **Structure** — is the SKILL.md readable top-to-bottom? Is the most
   important content up front?
5. **References** — do the supporting files and external links still
   exist and still say what the skill claims they say?

Report findings the same way @reviewer reports code: where, what, why
it matters, suggested fix. Offer to make the edits; let the user
confirm.

## Who writes skills

Sage owns skill authoring. Deciding what belongs in a skill versus
AGENTS.md versus a slash command is a structural judgment — the kind of
thing the disciplined thinking loop exists for. Dev can write a skill if
asked, but the question "is this the right shape for a skill?" is sage's
call.

Global skill edits prompt for confirmation (`ask` permission) because
they affect every future session across every project. Project-local
skill edits are `allow` — scoped to one repo, no different from editing
`docs/`.
