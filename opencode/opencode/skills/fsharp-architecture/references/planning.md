# Plan-Level Architectural Review

## Purpose

This reference covers checks that apply when designing or reviewing an **implementation
plan** — before any code is written. These are the mistakes that are expensive to fix
after implementation because they are baked into type signatures, module structure, and
phase dependencies.

Trigger this checklist when:
- Producing a multi-phase implementation plan
- Reviewing a proposed set of domain types, modules, or API signatures
- Adding a new module or capability to an existing plan

---

## The String Audit

For every `string` field in a proposed domain record, ask:

1. **Is this a named identifier?** (entity name, attribute name, trait name, parameter
   name, data type name, document name, namespace name) → It should be a validated
   domain type, not a raw `string`.

2. **Is this freeform text?** (description, explanation, display name) → `string` is
   correct.

3. **Is this an ambiguous SDK reference?** (could be a simple name or a corpus path,
   and the SDK doesn't distinguish) → `string` is acceptable; document why.

4. **Is this a filesystem path?** (adapter paths, root paths) → `string` is correct;
   filesystem paths don't follow domain validation rules.

5. **Is this a cross-system identifier?** (attribute names in relationship discovery
   that may include generated identifiers) → `string` is acceptable; document why.

If none of these apply, the field needs a domain type or the design needs re-examination.

**Apply this audit to extraction types, not just authoring types.** Fields populated
by reading from the SDK are subject to the same rules as fields populated by user input.

---

## The Existing Type Check

For every new domain type proposed in the plan, ask:

1. **Does an existing type already have the same fields and validation rules?**
   If yes, reuse it. A sub-manifest declaration is just a manifest declaration.

2. **Does an existing type have a superset of the fields?** If yes, consider whether
   the new type is a specialisation that carries genuine semantic weight, or just a
   structural duplicate with fewer fields.

3. **Would a consumer be confused by two types with the same shape but different names?**
   If yes, they should be the same type.

---

## The Capability Growth Check

For every capability record in the plan, project its size at the end of all planned phases:

1. **How many fields will the flat record have?** If more than 6–8, nest by module.

2. **How many `FakeCapabilities` constructors will be needed?** If every new capability
   field requires updating every existing constructor, the structure is not composable.

3. **Can a new module's capabilities be added without breaking existing test code?**
   If not, the capability record needs restructuring before more modules are added.

**When to restructure:** at the phase where the third module's capabilities are added,
or earlier if the plan makes it clear more modules are coming. The restructuring is
cheapest when done alongside other mechanical rewrites (e.g. API decomposition).

---

## The Sync / Async Classification

For every proposed public API function, ask:

1. **Does this operation perform I/O?** (network, disk, SDK async call) → Async.
   Returns `Task<Result<_, DomainError>>`. Takes `CancellationToken`. Goes through
   `runConnected`. Gets an activity span.

2. **Does this operation only read in-memory state from an already-loaded handle?**
   → Sync. Returns `Result<_, DomainError>`. No `CancellationToken`. No activity span.
   Calls the gateway directly.

3. **Is this operation a simple property setter on a live handle?** → Sync if the
   SDK call cannot fail or only fails via exception (guarded by `trySync`). Does not
   need `runConnected`.

**Common mistake:** routing a sync gateway call through the async capability system
because "everything should be consistent." This adds unnecessary `Task` wrapping,
requires a `CancellationToken` that can never actually cancel anything, and forces
the operation into the `FakeCapabilities` record where it doesn't belong.

---

## The SDK Side-Effect Audit

For every domain type that will be used as **input to an SDK method**, ask:

1. **What will the SDK silently do with this value?** Check for format detection,
   naming conventions, special-cased values, and implicit coercion.

2. **Does the SDK have different code paths based on string patterns?** (e.g. file
   extensions, naming prefixes, reserved names) → The domain type's validation rules
   must reject values that would trigger unintended code paths.

3. **Does the SDK mutate state as a side effect of this call?** → Document it in both
   the gateway function and the public API function's doc comments.

---

## The Module Assignment Sanity Check

For every operation in the plan, verify the assignment rule is applied consistently:

1. **What is the primary SDK object this operation acts upon?** That determines the
   module.

2. **For creation operations:** the module matches **what is being created**, not the
   container it's created in. `Entity.create` takes a `DocumentHandle` parameter but
   lives in the `Entity` module because the entity is what's being created.

3. **For extraction operations:** the module matches the **source**, not the result.
   `Manifest.readEntities` lives in `Manifest` because the manifest is the source,
   even though the result is `EntityDeclaration list`.

4. **Would a consumer be surprised to find this operation in this module?** If yes,
   the assignment may be wrong.

---

## The Phase Dependency Check

For each phase, verify:

1. **Every type referenced in signatures is either already defined or introduced in
   this phase.** No forward references to types defined in later phases.

2. **Every gateway function referenced by a public API function is either already
   implemented or introduced in this phase.** No dangling wiring.

3. **Test fixtures for integration tests are either hand-written (no dependency) or
   constructable using operations from prior phases.**

4. **The done criteria are specific enough to write a test from.** Not "tests pass"
   — name the exact behaviours being verified, including error paths and cancellation.

---

## Summary Checklist

Use this as a final pass before committing a plan:

- [ ] Every `string` field in a domain record has been audited (string audit)
- [ ] No new type duplicates an existing type (existing type check)
- [ ] Capability records scale to the planned module count (capability growth check)
- [ ] Every function is correctly classified as sync or async (sync/async classification)
- [ ] Every domain input type accounts for SDK behaviour (SDK side-effect audit)
- [ ] Every operation is in the right module (module assignment sanity check)
- [ ] Every phase has no forward dependencies (phase dependency check)
- [ ] Done criteria are test-writeable, not vague
