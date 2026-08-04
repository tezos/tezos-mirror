# Etherlink changelog fragments

`etherlink/CHANGES_TEZOSX.md` and `etherlink/CHANGES_NODE.md` are not edited
directly. Instead, each merge request adds **one file named after its own merge
request number** in the directory of the changelog section it belongs to:

```
etherlink/.changes/<changelog>/<section>/<mr-number>.md
```

Two merge requests therefore never write to the same lines: no rebase
conflicts, and no entry stranded above a release header that was cut on
`master` while the merge request was open. The fragments are assembled into the
changelog once, when a release is cut.

| Directory | Changelog | Documents |
|---|---|---|
| `.changes/tezosx/` | `CHANGES_TEZOSX.md` | Tezos X — the kernel |
| `.changes/node/` | `CHANGES_NODE.md` | the EVM node |

A merge request that changes both declares an entry in both.

## Adding an entry

1. Pick the section. For **Tezos X** (`.changes/tezosx/`):

   | Directory | Changelog section |
   |---|---|
   | `evm_runtime/` | `### EVM Runtime` |
   | `michelson_runtime/` | `### Michelson Runtime` |
   | `nac/` | `### Native Atomic Composability` |
   | `storage_versions/` | `### Storage versions` |
   | `internals/` | `### Internals` |

   For the **EVM node** (`.changes/node/`):

   | Directory | Changelog section |
   |---|---|
   | `breaking/` | `### Breaking changes` |
   | `configuration/` | `### Configuration changes` |
   | `rpcs/` | `### RPCs changes` |
   | `monitoring/` | `### Monitoring changes` |
   | `cli/` | `### Command-line interface changes` |
   | `execution/` | `### Execution changes` |
   | `storage/` | `### Storage changes` |
   | `documentation/` | `### Documentation changes` |
   | `experimental/` | `### Experimental features changes` |

2. Write the markdown bullet(s) to insert under that section in
   `<section>/<mr-number>.md`, wrapped at 80 columns with 2-space continuation
   indent, as in the changelog itself:

   ```markdown
   - Blueprints signed by a sequencer key that has since been rotated are
     rejected instead of being applied on the next level.
   ```

   **Do not write the `(!<mr-number>)` reference**: it is appended to every
   bullet when the release is assembled, and `check` rejects a fragment that
   spells out its own reference. Referring to *another* merge request in the
   text of an entry is fine.

3. A fragment may hold several bullets (one merge request changing two things in
   the same section), and a merge request may have one fragment per section.
   Each bullet gets the reference.

Say "Michelson runtime", never "Tezlink", in Tezos X entries.

Check your fragment locally:

```bash
etherlink/scripts/changelog.sh <changelog> check <mr-number>
```

## No entry needed

A merge request that needs no entry in a changelog (pure refactoring, test
fixes, …) says so with an **empty file**:

```bash
touch etherlink/.changes/<changelog>/no_changelog/<mr-number>.md
```

The declaration is explicit and reviewable in the diff, and it disappears with
the next release like any other fragment. A merge request cannot both provide an
entry and declare that it needs none.

## Never edit a changelog

`CHANGES_TEZOSX.md` and `CHANGES_NODE.md` are written by the release command
only. Their `Unreleased` section holds nothing but a pointer to the fragment
directory, and CI fails if an entry appears there — that section being empty is
what makes a changelog conflict-free.

## CI

The **`etherlink.check_<changelog>_changelog`** jobs (sanity stage) run on every
merge request that touches the code a changelog documents
(`etherlink/kernel_latest/**` for Tezos X, `etherlink/bin_node/**` for the node),
that changelog, or its fragments. Each fails when the merge request declares
nothing for it, and when a file would be silently dropped at release time — a
fragment in a misspelled section directory, or not named `<mr-number>.md`.

The manual **`etherlink.preview_<changelog>_changelog`** jobs print the section
that the fragments currently in the repository would produce.

## Cutting a release

```bash
etherlink/scripts/changelog.sh tezosx preview
etherlink/scripts/changelog.sh tezosx release <version> <kernel-commit-hash>

etherlink/scripts/changelog.sh node preview
etherlink/scripts/changelog.sh node release <version> <date>
```

`release` inserts `## Version <version> (<argument>)` in the changelog with the
assembled sections — in changelog order, most recent merge request first, empty
sections omitted — then deletes the consumed fragments. The second argument is
what that changelog's version headers carry: a kernel commit hash for Tezos X, a
release date (`YYYY-MM-DD`) for the node. The node's *Experimental features
changes* section keeps its usual backward-compatibility disclaimer, added
automatically whenever that section has entries.

Review and commit the result:

```bash
git add -A etherlink && git diff --cached etherlink
```

## Adding a changelog

Both changelogs are driven by the same script, `etherlink/scripts/changelog.sh`.
Teaching it a third one — `CHANGES_KERNEL.md`, say — means adding a line to its
`changelogs` table, a section table, the matching directories here, and a pair of
CI jobs in `etherlink/ci/etherlink_ci.ml`.
