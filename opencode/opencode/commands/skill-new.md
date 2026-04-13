---
description: "Scaffold a new skill directory with SKILL.md. Pass the skill name as argument."
agent: "sage"
argument-hint: "<skill-name> [global|local]"
---

Scaffold a new skill. Argument: $ARGUMENTS

Parse the argument as `<skill-name> [scope]` where scope is `global`
(default) or `local`. The skill name must be kebab-case — no spaces,
no underscores, no capitals.

Before creating anything, have the structural conversation from the
skill-authoring skill:

1. **Is a skill the right shape for this?** Check the three-way
   distinction: AGENTS.md (always loaded), skill (on-demand), slash
   command (user-triggered). Ask me to describe what the skill should
   do, then tell me honestly whether a skill is the right home for it
   or whether it belongs somewhere else.

2. **What are the triggers?** A skill is loaded when its description
   matches the current task. Help me draft a description that names
   the triggers concretely — specific question shapes, specific
   artefacts, specific operations. Vague descriptions do not load.

3. **Is this one topic or several?** If the description needs "and"
   more than once, push back and propose splitting.

Once we have agreement on shape, triggers, and scope, create:

- **Global scope** — `~/.config/opencode/skills/<name>/SKILL.md`.
  This is a global write that will prompt for permission; explain
  what the skill does before answering the prompt.
- **Local scope** — `.opencode/skills/<name>/SKILL.md`. No prompt —
  project-scoped, safe to write.

Start the SKILL.md with quoted frontmatter:

```yaml
---
name: "<name>"
description: "<the description we drafted>"
---
```

Then add the body — the skill's actual content, structured so the
most important guidance is up front and reference material is
further down.

Show me the finished file before writing.
