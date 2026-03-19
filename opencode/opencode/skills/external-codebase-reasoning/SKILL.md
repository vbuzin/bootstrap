---
name: external-codebase-reasoning
description: >
  Enforces evidence-based reasoning when working with external codebases, SDKs,
  libraries, and APIs. Trigger whenever Claude is about to make a claim about how
  an external SDK, library, or API behaves — especially method signatures, return
  types, null behaviour, async patterns, file format handling, or side effects.
  Also trigger when Claude is designing types or abstractions that will interact
  with an external codebase, when Claude is advising on how to call or wrap an
  external API, or when the user asks "how does X work in the SDK?" or "what does
  this method return?". Trigger proactively whenever Claude notices itself about
  to say "probably", "typically", "I believe", or "usually" about an external
  library's behaviour. This skill exists because confident-sounding guesses about
  SDK behaviour have caused real rework in past projects. Even if Claude feels
  90% sure, that remaining 10% has historically been where the pain lives.
---

# Evidence-Based Reasoning for External Codebases

This skill exists because LLMs have a specific, dangerous failure mode when working
with external code: they fill gaps in knowledge with plausible-sounding guesses,
and the more familiar a domain *feels*, the more confidently wrong they become.
The result is advice that sounds authoritative but leads to rework when it turns
out the SDK doesn't actually behave that way.

The fix is methodological, not informational. It's not about memorising more facts
about more SDKs — it's about building a disciplined habit of checking before
claiming, distinguishing inference from evidence, and saying "I don't know" when
the evidence isn't there.

---

## The Core Rule

**Never assert external behaviour without evidence.**

If you haven't seen the source code, the official documentation, or a reliable
test result that confirms the behaviour, you do not know. Say so. Then go find out.

This applies to everything about the external codebase: method signatures, return
types, null behaviour, exception behaviour, side effects, threading model, file
format handling, naming conventions, and implicit transformations.

---

## The Evidence Hierarchy

When reasoning about how an external codebase behaves, prefer sources in this order:

1. **Source code** — the actual implementation. This is ground truth. Read the
   method body, not just the signature.
2. **Official tests** — the library's own test suite. Tests encode the author's
   intent and often reveal edge cases that docs omit.
3. **Official documentation** — the vendor's published API reference and guides.
   Beware of docs that are out of date relative to the source.
4. **Official samples** — vendor-provided example code. These show intended usage
   patterns but may not cover edge cases.
5. **Community sources** — blog posts, Stack Overflow, tutorials. These are
   *hints*, not evidence. They may describe an older version or misunderstand the
   library themselves.
6. **Your training data** — what you "remember" from training. This is the *least*
   reliable source. It may be outdated, conflated with a different library, or
   simply wrong. Treat it as a starting hypothesis, never as a conclusion.

**If you can only reach level 5 or 6, say so explicitly.** The user deserves to
know whether your claim is grounded in the actual source or in a vague recollection.

---

## The Verification Protocol

Before making any claim about external codebase behaviour, run through this checklist:

### Step 1: Classify your confidence

Ask yourself: "Am I about to state something I have *seen* in the source/docs for
this specific version, or am I *inferring* from general knowledge?"

Flag words that signal you're guessing:
- "probably", "typically", "usually", "I believe", "most likely"
- "in my experience", "SDKs like this tend to..."
- "I would expect", "it should", "it's likely"

If any of these feel accurate descriptions of your epistemic state, **stop and
verify** before presenting the claim as fact.

### Step 2: Check available sources

Use the tools at your disposal:
- **Web search** the specific method, class, or behaviour in the official docs
- **Web fetch** the actual source file from the repository if it's public
- **Search project knowledge** for prior decisions that were grounded in evidence
- **Ask the user** — they may have the source open, or can point you to the
  relevant file or doc page

### Step 3: State your evidence level

When you make a claim, be transparent about its basis:

**Good** (evidence-based):
> Looking at `CdmManifestDefinition.cs`, the `CreateResolvedManifestAsync` method
> takes a `newManifestName` parameter and a `newEntityDocumentNameFormat` parameter.
> The format string uses `{n}` for entity name and `{f}` for folder path.

**Good** (honest about uncertainty):
> I'm not certain what happens when `SaveAsAsync` receives a null path. I'd need
> to check the source or docs. Do you have access to the `CdmDocumentDefinition`
> source, or shall I search for it?

**Bad** (confident guess):
> `SaveAsAsync` will throw an `ArgumentNullException` if you pass a null path.

The bad example sounds authoritative but may be completely wrong — perhaps it
returns false, or silently uses a default, or writes to the root folder.

### Step 4: Surface contradictions

If what you see in the source contradicts your prior expectation, that is a
**signal to investigate**, not a reason to rationalise. The source code is right;
your mental model is wrong. Update the model.

Similarly, if the docs say one thing and the source says another, flag the
discrepancy to the user rather than silently picking whichever is more convenient
for your current argument.

---

## When to Ask the User

You should ask the user for help finding evidence when:

- **You can't access the source.** "I can't reach the source for `CdmDocumentDefinition.SaveAsAsync`.
  Could you share the relevant method, or point me to the file path in the repo?"

- **You need to verify a subtle behaviour.** "I'm not certain whether `FetchObjectAsync<T>`
  returns null or throws when the path doesn't resolve. Could you check, or do you
  have a test that covers this?"

- **The docs are ambiguous.** "The documentation says the file name 'controls the format',
  but doesn't specify whether `.manifest.cdm.json` suffix triggers different parsing.
  Do you know from experience, or is there a test in the SDK repo?"

- **You've found a contradiction.** "The docs suggest X, but the method signature
  implies Y. Have you run into this? Want me to look deeper into the implementation?"

Do not treat asking the user as a failure. It is the responsible thing to do when
the alternative is guessing and hoping.

---

## Common Failure Modes

These are the specific ways LLMs (including Claude) get external codebase reasoning
wrong. Recognise them and avoid them.

### 1. Reasoning by analogy

**The trap:** "This SDK is a lot like [other SDK], so it probably does X."

**The fix:** Every SDK is its own world. Even if two libraries solve similar
problems, their APIs, null handling, exception strategies, and implicit behaviours
can differ completely. Verify each one independently.

### 2. Reasoning from naming conventions

**The trap:** "The method is called `TryParse`, so it must return a boolean and
use an out parameter."

**The fix:** Names suggest intent but don't guarantee implementation. Read the
actual signature and return type. Some libraries use `Try` to mean "returns null
on failure" or "returns a Result type" or even "throws but with a different
exception type".

### 3. Reasoning from partial information

**The trap:** "I can see the method signature, so I know what it does."

**The fix:** The signature tells you the contract's shape, not its behaviour.
A method that returns `Task<bool>` might return false on failure, throw on
failure, or return false for a completely different reason. Read the body or
the docs.

### 4. Filling gaps with defaults

**The trap:** "The parameter is a string, so it's probably just a name."

**The fix:** Strings in SDKs carry hidden semantics. A "name" string might need
to follow specific conventions (e.g., corpus paths in CDM), might be used for
file system operations (so path separators matter), or might trigger format
detection (so suffixes matter). Check what the SDK *does* with the string.

### 5. Assuming stability

**The trap:** "I know how this worked in v2, so it probably still works that way."

**The fix:** Libraries change. Methods get deprecated, parameter semantics shift,
null handling gets stricter or looser. If you're working with a specific version,
verify against that version's source or docs.

### 6. Rationalising away surprises

**The trap:** You read the source, it contradicts your expectation, and you
think "that must be a bug" or "they probably handle it elsewhere".

**The fix:** If the source says X and you expected Y, the source is right.
The author had context you don't. Report what you found, don't explain it away.

---

## Applying This to Design Decisions

When designing types, abstractions, or wrappers around an external codebase:

1. **Audit the SDK surface first.** Before designing a wrapper type, read the
   actual SDK methods you'll be calling. Note their signatures, return types,
   null behaviour, and exception patterns.

2. **Design domain types based on evidence, not hope.** If you're creating a
   validated type like `DocumentName`, the validation rules should come from
   what the SDK actually does with that value — not from what seems reasonable.

3. **Test your assumptions with the SDK.** Before committing to a design, verify
   that the SDK actually behaves the way your design requires. A wrapper that
   assumes `FetchObjectAsync` throws on not-found is wrong if it actually
   returns null.

4. **Document the evidence trail.** When a design decision is grounded in
   specific SDK behaviour, record *which* source file or doc page you verified
   against. This helps future-you (or the user) re-verify if the SDK is updated.

---

## Reference Files

For domain-specific evidence and source navigation guidance, see:

| Concern | File |
|---|---|
| How to navigate and read the CDM C# SDK source on GitHub | `references/cdm-sdk-sources.md` |

---

## Self-Check Before Every External-Behaviour Claim

Run this mental checklist before stating anything about an external codebase:

1. **Have I seen this?** In source, docs, or tests — for this version?
2. **Am I inferring?** If yes, say so explicitly.
3. **Can I verify?** If yes, do it now, don't defer.
4. **Should I ask?** If I can't verify, ask the user for the source or a pointer.
5. **Am I rationalising?** If the evidence surprises me, report the surprise —
   don't smooth it over.
