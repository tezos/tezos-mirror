<!--
Template for an Octez release tracking issue: the list of changes that must land
in a given release, and the state of their backports.

Create one issue per released version (including release candidates and betas of
that version), and keep it updated until the release is out.

Conventions:
- Title: `Octez vX.Y` (e.g. `Octez v25.2`). Nothing else, so that the series is
  easy to find.
- Label: ~"release manager 👷" (added by the quick action at the bottom).
- One issue covers the whole version: use a `##` section per release phase
  (`Release candidate 1`, `Release`, ...) when the version has RCs or betas,
  and drop those sections for a minor release that goes straight to the tag.
- Inside a phase, group changes by area (`L1`, `DAL`, `Infra`, `Rollups`,
  `Etherlink`, `Packaging`, ...). Only keep the areas you need.
- One top-level checkbox per *change*, labelled with the change title (not the
  MR number), then one sub-checkbox per *target branch*: `master` first, then
  the release branch `vX-release`. This is the point of the issue: seeing at a
  glance what is merged on master but not backported yet.
- Reference merge requests as `!NNNNN`, or as a full URL when the MR lives in
  another project. Write `TBD` when the backport MR does not exist yet.
- Tick the top-level box only once *every* target branch is done.
- Prefix an entry with `(optional)` when it is nice to have but must not block
  the release, and ~~strike through~~ an entry that has been abandoned (keep it,
  so the decision stays visible).
- A single aggregated backport MR may cover several entries: repeat the same
  `!NNNNN` in each of them.

Reminders that are *not* tracked here:
- Documentation changes to do for every release (`docs/releases/version-X.rst`,
  changelog snapshot to `docs/CHANGES.rst`, OpenAPI specifications): see the
  meta-issue #5718.
- Protocol-side coordination: see
  https://octez.tezos.com/docs/developer/protocol_release_checklist.html
- What the release pipelines publish and how:
  https://octez.tezos.com/docs/releases/releases.html
-->

# Octez vX.Y

<!-- Release branch: `vX-release`. Remove the phase sections you do not need. -->

### L1

- [ ] <Change title>
  - [ ] !NNNNN - master
  - [ ] TBD - vX-release

### DAL

- [ ] <Change title>
  - [ ] !NNNNN - master
  - [ ] !NNNNN - vX-release

### Infra

- [ ] <Change title>
  - [ ] !NNNNN - master
  - [ ] !NNNNN - vX-release

<!--
Entries that must land on one branch only are legitimate; say so explicitly:

- [ ] <Change title> (master only, not needed in the release)
  - [ ] !NNNNN - master
- [ ] <Change title> (vX-release only, must NOT go to master)
  - [ ] !NNNNN - vX-release
-->

/labels ~"release manager 👷"
/assign me
