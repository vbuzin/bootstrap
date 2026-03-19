# Type Modelling and Module Organisation

## Core Type Modelling Rules

### Records for product types
```fsharp
// CORRECT — named fields, immutable, composable
type DocumentDeclaration = { Name: DocumentName; Path: ResourcePath }

// WRONG — class with mutable state
type DocumentDeclaration() =
    member val Name = "" with get, set
```

### Discriminated Unions for sum types
```fsharp
// CORRECT — exhaustive matching enforced by compiler
type OverwritePolicy =
    | AllowOverwrite
    | DenyOverwrite

// WRONG — string or int constants
[<Literal>]
let AllowOverwrite = "allow"
```

### No classes in domain modelling
Classes are permitted **only** when an external library requires implementing an interface
or extending a base class. Domain types are records and DUs.

---

## Opaque Type Pattern

Use opaque types (private constructor + companion module) for **validated value types**
— any type where "constructed = valid" is an invariant you need to enforce.

```fsharp
// The type is opaque: only create and value exist in the public surface
type ResourcePath = private ResourcePath of string

[<RequireQualifiedAccess>]
module ResourcePath =
    let create (raw: string) : Result<ResourcePath, DomainError> =
        // validation logic here
        Ok (ResourcePath normalised)

    let value (ResourcePath v) = v
```

**When to use an opaque type:**
- The raw type (usually `string`) admits values that are invalid in this context
- Construction is fallible — validation can return an error
- You want to prevent callers from constructing values directly

**When NOT to use an opaque type:**
- The raw type is already constrained enough (e.g. an enum-backed DU)
- There is no validation logic — it is just a rename

---

## Extraction Types — The Same Rules Apply

Fields in domain records that are populated by extracting from external SDK objects
are subject to the **same validation rules** as user-authored fields. The extraction
pipeline should validate and fail early, not carry raw strings.

### Why this matters
When the wrapper extracts data from an SDK object (e.g. reading attribute names from
a loaded entity), the temptation is to use `string` because "it came from the SDK, it
must be valid." But:

1. The SDK may contain user-authored content that violates our invariants.
2. Raw strings in domain records bypass compile-time safety — a `string` field named
   `attributeName` can be accidentally assigned a corpus path or a description.
3. Downstream code that consumes the extracted record cannot rely on the field being
   valid without re-checking.

### The rule
If a field in an extraction record represents a **named identifier** (an entity name,
attribute name, trait name, parameter name, data type name), it should be a validated
domain type, not a raw `string`.

```fsharp
// CORRECT — extraction validates just like authoring
type TypeAttributeInfo =
    private {
        Name: AttributeName           // validated during extraction
        DataTypeName: DataTypeName option  // validated during extraction
        Description: string option    // freeform text — string is correct
    }

// WRONG — raw strings for identifiers
type TypeAttributeInfo =
    private {
        Name: string
        DataTypeName: string option
        Description: string option
    }
```

### Exceptions where `string` is acceptable in extraction records:
- **Freeform text fields**: descriptions, explanations, display names — these have no
  validation constraints.
- **Ambiguous SDK references**: fields where the SDK produces either a simple name or a
  corpus path and the wrapper cannot distinguish which (e.g. `ExtendsEntity` on an
  entity definition, `EntityReference` on an entity attribute). Wrapping in a validated
  type would reject legitimate SDK output.
- **Cross-system identifiers**: attribute names in relationships (`FromAttribute`,
  `ToAttribute`) that may include generated identifiers from the SDK's relationship
  discovery, which may not follow the wrapper's naming rules.

### What to flag
| Issue | Description |
|---|---|
| Raw `string` for a named identifier in an extraction record | Should be a validated domain type |
| Validated type used for a freeform text field | Over-constraining — `string` is correct |
| Validated type used for an ambiguous SDK reference | May reject legitimate SDK output |

---

## `[<NoEquality; NoComparison>]` for Live Resources

Any type that wraps a live external object, connection, or other non-value resource
should carry `[<NoEquality; NoComparison>]` to prevent callers from comparing or
storing them based on structural identity, which is meaningless for live resources.

```fsharp
[<NoEquality; NoComparison>]
type ConnectionHandle = private ConnectionHandle of ExternalSdkConnection
```

---

## Module Organisation Rules

### Companion modules follow their types immediately
The module for a type must be in the same file, directly after the type:

```fsharp
type DocumentName = private DocumentName of string

[<RequireQualifiedAccess>]
module DocumentName =
    let create ...
    let value ...
```

### `[<RequireQualifiedAccess>]` on companion modules
Prevents unqualified `create` or `value` from polluting the caller's namespace.
Without it, `open MyApp.Domain` exposes bare `create` and `value` names that collide
across all types in the namespace.

### One concept per file
Each file should have one primary type or a small cluster of closely related types.
Files that grow to hold multiple unrelated types are a sign the concepts should be split.

### File header convention
Every file should begin with a two-line comment declaring its layer and responsibility:
```fsharp
// Layer: Infrastructure | Responsibility: Load and extract documents from external storage.
namespace MyApp.Infrastructure
```

---

## Shared Validation Helpers

Shared validation primitives (null check, length check, character set check) belong in
a single `internal` helper module in the Domain layer. Duplicating them across domain
type files means a rule change requires multiple edits and creates inconsistency risk.

```fsharp
// Domain/ValidationHelpers.fs — shared by all domain type constructors
module internal ValidationHelpers =
    let fail field rule actual =
        Error (Validation { Field = field; Rule = rule; Actual = actual })

    let hasControlCharacters (s: string) =
        s |> Seq.exists Char.IsControl
```

---

## Existing Type Reuse Check

Before introducing a new domain type, ask: **does an existing type already model this
concept?** Two types with the same fields, the same validation rules, and the same
semantic meaning are the same type — even if they are extracted from different parent
containers.

```fsharp
// WRONG — SubManifestDeclaration duplicates ManifestDeclaration
type SubManifestDeclaration = { Name: ManifestName; DefinitionPath: CorpusPath }
type ManifestDeclaration    = { Name: ManifestName; Path: CorpusPath }

// CORRECT — reuse ManifestDeclaration for both self-description and sub-manifest pointers
```

If the types share shape but differ semantically, the distinction should be captured
by naming (e.g. different accessor function names) or by usage context, not by
creating a structurally identical twin type.

---

## What to Flag

| Issue | Description |
|---|---|
| Mutable auto-properties | `member val X = "" with get, set` in a domain type |
| String constants instead of DU cases | Magic strings where a DU case would make states explicit |
| Raw string where an opaque type should exist | `path: string` crossing a layer boundary |
| Raw string for a named identifier in an extraction record | Should be a validated domain type |
| Validated type unwrapped too early | `.value` called at domain layer instead of at interop |
| Companion module in a different file from its type | Navigability cost with no benefit |
| Missing `[<RequireQualifiedAccess>]` on companion module | `create` and `value` leak into global namespace |
| Validation logic duplicated across type files | Should live in a shared `ValidationHelpers` module |
| New type structurally identical to an existing type | Reuse the existing type instead |
