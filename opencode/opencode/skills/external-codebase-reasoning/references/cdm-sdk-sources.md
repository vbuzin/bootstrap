# CDM C# SDK — Source Navigation Guide

This reference helps you find and verify CDM SDK behaviour from primary sources.
Use it whenever you need to check how a specific CDM API actually works, rather
than relying on memory or inference.

---

## Primary Sources

### Source Code (ground truth)

Repository: `https://github.com/microsoft/CDM`
SDK root: `objectModel/CSharp/Microsoft.CommonDataModel.ObjectModel/`

Key directories within the SDK project:

| Directory | Contains |
|---|---|
| `Cdm/` | All CDM object definitions — the core classes you interact with |
| `Enums/` | Enumeration types (`CdmObjectType`, `CdmStatusLevel`, etc.) |
| `Persistence/` | Serialisation/deserialisation logic for `.cdm.json` and `model.json` |
| `Storage/` | Storage adapter abstractions and local/remote implementations |
| `Utilities/` | Resolution options, logging, collections, and helper classes |
| `ResolvedModel/` | Internal resolution machinery — rarely needed for wrapper design |

### Key Classes to Know

These are the classes most relevant to wrapper design. When in doubt about
behaviour, these are the files to read:

| Class | File path (from SDK root) | Wrapping concern |
|---|---|---|
| `CdmCorpusDefinition` | `Cdm/CdmCorpusDefinition.cs` | Lifecycle, storage, fetch |
| `CdmManifestDefinition` | `Cdm/CdmManifestDefinition.cs` | Manifest operations, inherits `CdmDocumentDefinition` |
| `CdmDocumentDefinition` | `Cdm/CdmDocumentDefinition.cs` | Document load/save, imports, definitions |
| `CdmEntityDefinition` | `Cdm/CdmEntityDefinition.cs` | Entity structure, attributes, traits |
| `CdmTraitDefinition` | `Cdm/CdmTraitDefinition.cs` | Trait definitions and parameters |
| `CdmTraitReference` | `Cdm/CdmTraitReference.cs` | Trait usage with arguments |
| `CdmFolderDefinition` | `Cdm/CdmFolderDefinition.cs` | Folder navigation and document containers |
| `CdmCollection<T>` | `Cdm/CdmCollection.cs` | Mutable SDK collections — understand `Add`, `Remove` patterns |
| `StorageManager` | `Storage/StorageManager.cs` | Adapter mounting, namespace resolution |
| `ResolveOptions` | `Utilities/ResolveOptions.cs` | Resolution configuration — used in many internal calls |

### Official Documentation

Base URL: `https://learn.microsoft.com/en-us/common-data-model/`

Key pages:

| Topic | Path |
|---|---|
| SDK overview and concepts | `sdk/overview` |
| Technical fundamentals (corpus paths, imports, formats) | `sdk/fundamentals` |
| Working with model.json and manifest.cdm.json | `sdk/from-modeljson-to-the-future` |
| Sample code guide | `samples` |

### Official Samples

Sample code in the repo: `https://github.com/microsoft/CDM/tree/master/samples`

The samples demonstrate patterns like reading manifests, creating entities,
and working with storage adapters. They are useful for understanding *intended*
usage, but always cross-reference with the source for edge cases.

### SDK Tests

Test project: `objectModel/CSharp/Microsoft.CommonDataModel.ObjectModel.Tests/`

SDK tests are invaluable for understanding expected behaviour, especially around
edge cases, null handling, and format detection. When you want to know "what
happens when X", search the test project first.

---

## How to Search Effectively

When verifying SDK behaviour:

1. **Start with the class file.** If you want to know about `SaveAsAsync`, go
   directly to `CdmDocumentDefinition.cs` and read the method.

2. **Check the base class.** Many CDM classes inherit from `CdmObjectDefinitionBase`
   or `CdmObjectReferenceBase`. The method you're looking for might be defined
   there. `CdmManifestDefinition` inherits from `CdmDocumentDefinition`, which
   is a crucial relationship.

3. **Check the persistence layer.** If your question is about how `.cdm.json`
   files are parsed or written, look in `Persistence/CdmFolder/` — the
   serialisation logic lives there, not in the Cdm classes themselves.

4. **Search the test project.** For questions like "does X return null or throw?",
   the tests often encode the answer directly.

5. **Read the enums.** `CdmObjectType` tells you what object types the SDK
   distinguishes. This is relevant when the SDK dispatches based on type
   (e.g., manifest detection by file suffix).

---

## Known Subtleties

These are SDK behaviours that have caused issues in the past — areas where
the "obvious" assumption was wrong. When encountering similar patterns in any
SDK, be especially vigilant.

### File suffix determines type

The CDM SDK uses file suffixes to determine how to parse and construct objects.
A file ending `.manifest.cdm.json` is parsed as a manifest, not a plain document.
This means a `DocumentName` type that allows `.manifest.cdm.json` will cause
silent misinterpretation by the SDK. The wrapper must reject this suffix at the
domain type level.

**Lesson:** When an SDK uses strings for dual purposes (identifier + format
control), the wrapper's validation rules must account for both purposes. The
validation isn't just "is this a valid string?" — it's "what will the SDK *do*
with this string?".

### Inheritance hides type differences

`CdmManifestDefinition` inherits from `CdmDocumentDefinition`. This means a
manifest IS-A document in the SDK's type hierarchy. Methods that accept or return
`CdmDocumentDefinition` might hand you back a manifest. The wrapper's opaque
handle types must account for this — a `DocumentHandle` might secretly be wrapping
a manifest.

**Lesson:** Read the class hierarchy before designing handle types. Check what
`FetchObjectAsync<T>` actually returns, not just what type you asked for.

### Null is a valid return in many places

The CDM SDK frequently returns null where you might expect an exception: object
not found, failed resolution, missing properties. The wrapper's interop layer must
treat every SDK return as potentially null and route it through `ofObj`.

**Lesson:** Never assume an SDK method throws on failure. Check the source.

### Collections are mutable and side-effecting

CDM's `CdmCollection<T>` methods like `Add` not only add the item but also set
ownership, document references, and dirtyness flags on the containing document.
This means "adding an import" has side effects beyond what the return type suggests.

**Lesson:** Read the `Add` method body, not just its signature. Understand what
mutations occur so the wrapper can document them or protect against them.

---

## When You Can't Find the Source

If you cannot access the specific source file or doc page you need:

1. **Tell the user what you're looking for.** Be specific: "I need to see the
   implementation of `CdmDocumentDefinition.SaveAsAsync` to verify whether it
   returns false or throws on failure."

2. **Ask for a pointer.** The user may be able to share the relevant code, or
   link to the specific file on GitHub.

3. **State your assumption explicitly.** If you must proceed without verification,
   mark the assumption clearly: "I'm *assuming* that `SaveAsAsync` returns false
   on failure based on the boolean return type — please verify this against the
   source before relying on it."

4. **Never silently guess.** The worst outcome is advice that sounds verified
   but isn't. If you can't verify, say so.
