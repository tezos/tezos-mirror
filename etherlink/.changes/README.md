# Tezos X changelog fragments

`etherlink/CHANGES_TEZOSX.md` is not edited directly. Instead, each merge
request adds **one file named after its own merge request number** in the
directory of the changelog section it belongs to:

```
etherlink/.changes/tezosx/<section>/<mr-number>.md
```

Two merge requests therefore never write to the same lines: no rebase
conflicts, and no entry stranded above a release header that was cut on
`master` while the merge request was open. The fragments are assembled into
`CHANGES_TEZOSX.md` once, when a release is cut.

## Adding an entry

1. Pick the section:

   | Directory | Changelog section |
   |---|---|
   | `evm_runtime/` | `### EVM Runtime` |
   | `michelson_runtime/` | `### Michelson Runtime` |
   | `nac/` | `### Native Atomic Composability` |
   | `storage_versions/` | `### Storage versions` |
   | `internals/` | `### Internals` |

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
etherlink/scripts/tezosx_changelog.sh check <mr-number>
```

## No entry needed

A merge request that needs no changelog entry (pure refactoring, test fixes, …)
says so with an **empty file**:

```bash
touch etherlink/.changes/tezosx/no_changelog/<mr-number>.md
```

The declaration is explicit and reviewable in the diff, and it disappears with
the next release like any other fragment. A merge request cannot both provide an
entry and declare that it needs none.

## Never edit the changelog

`CHANGES_TEZOSX.md` is written by the release command only. Its `Unreleased`
section holds nothing but a pointer to this directory, and CI fails if an entry
appears there — that section being empty is what makes the changelog
conflict-free.

## CI

The **`etherlink.check_tezosx_changelog`** job (sanity stage) runs on every merge
request that touches `etherlink/kernel_latest/**`, the changelog or this
directory. It fails when the merge request declares nothing, and when a file
would be silently dropped at release time — a fragment in a misspelled section
directory, or not named `<mr-number>.md`.

The manual **`etherlink.preview_tezosx_changelog`** job prints the section that
the fragments currently in the repository would produce.

## Cutting a release

```bash
etherlink/scripts/tezosx_changelog.sh preview
etherlink/scripts/tezosx_changelog.sh release <version> <kernel-commit-hash>
```

`release` inserts `## Version <version> (<kernel-commit-hash>)` in
`CHANGES_TEZOSX.md` with the assembled sections — in changelog order, most
recent merge request first, empty sections omitted — then deletes the consumed
fragments. Review and commit the result:

```bash
git add -A etherlink && git diff --cached etherlink
```
