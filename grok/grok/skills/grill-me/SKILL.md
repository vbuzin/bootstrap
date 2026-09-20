---
name: grill-me
description: >
  Relentless interview about a plan, decision, or idea until every branch of
  the design tree is resolved. User-invoked only. Use when the user runs
  /grill-me, says "grill me", or wants to stress-test a plan or design before
  building.
disable-model-invocation: true
argument-hint: plan or design to grill
license: MIT
metadata:
  author: Matt Pocock (adapted for Grok)
---

Interview the user relentlessly until you reach a shared understanding. Map this as a **design tree**: every decision branches into the decisions that hang off it. Do not implement, edit, or commit anything until they confirm the tree is settled.

Work the tree in **rounds**. The **frontier** is every decision whose prerequisites are already settled: the questions you can ask *now* without guessing at answers you haven't heard yet. Ask the whole frontier in one round. Then wait.

Use `ask_user_question` for the round (`multi_select` only when several options can be true together). One question object per frontier decision. Put your recommended option first and append `(Recommended)` to its label. The user always has Other.

Finding *facts* is your job, never the user's. When a frontier question needs a fact from the repo, spawn an `explore` subagent and keep going: a running look-up is an unsettled prerequisite, so only questions downstream of it wait; ask the rest of the frontier now. The *decisions* are the user's.

Each round of answers reshapes the tree: settled decisions push the frontier outward. Recompute and ask the next round. A question that depends on another still open in this round belongs to a later round.

The session is done when the frontier is empty: every branch visited, nothing left silently assumed. Summarise the decisions, then stop. Do not act on them until the user confirms shared understanding.
