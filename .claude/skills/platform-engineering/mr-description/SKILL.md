---
name: mr-description
description: Write a good GitLab merge request description (and title) for a Platform Engineering (PE) merge request on the tezos/tezos project, following the conventions PE reviewers expect and the project's pre-review guidelines. Use this skill when the change's PRIMARY CONCERN is PE-owned infrastructure, judged by the NATURE of the change rather than who authored it. The PE-owned categories are — CI pipeline generation & configuration; CI/build/base Docker images; OS packaging (Debian, RPM, Homebrew); and release & CI tooling, including PE monitoring (Teztale, Grafazos). Representative paths for these categories (illustrative and drift over time, not an exhaustive or authoritative list): `ci/`, `.gitlab/`, `.gitlab-ci.yml`; `images/`, root `Dockerfile*`; `scripts/packaging/`, `debian/`, rpm specs, homebrew formulae; `scripts/ci/`, `scripts/*release*`; `teztale/`, `grafazos/`. Produces a focused single-concern description using the repo's What / Why / How / Manually testing template, a correctly-formatted title, and the evidence reviewers look for — convincing motivation (bug occurrence, who-uses-it, ADR link), discussed alternatives, justified compromises, announced side-effects/breaking changes, reachable test results (test-pipeline links, local results, benchmarks), reproduction instructions, and well-split commits. Use when such a PE MR needs its description or title written, drafted, or improved (e.g. "write a description for this MR", "draft the MR text", "help me phrase this MR"). Do NOT use when the change's primary concern is protocol, application, or library logic rather than CI/packaging/images/release infrastructure — for example a `src/proto_*` consensus change, a `src/lib_*`/`src/bin_*` node/client change, Etherlink kernel/node application logic, or a docs-only MR — even when the author is a PE team member; for those follow the generic project contribution guidelines instead. Conversely, a CI/packaging/images/release change is in scope even when authored by a non-PE contributor.
---

# MR Description (Tezos / GitLab) — Platform Engineering

Write MR descriptions and titles for `tezos/tezos` that match what Platform
Engineering reviewers expect on platform-engineering MRs. The goal is a
description a reviewer can trust to scope their review, plus
a title that survives a `git log` skim.

## Gather the facts first

Describe the *actual* change, not the intent. Before writing, inspect the diff:

- `git log <target>..HEAD --oneline` — the commits.
- `git diff <target>...HEAD --stat` — the touched paths.

### Find related issues and the MR stack

Pull in the references reviewers expect before drafting:

- **Related issues.** Search for issues this MR addresses or touches and link
  them in the description. Look in commit messages for `#1234` / issue URLs;
  grep the touched files for `TODO`/`FIXME` issue references; and search GitLab
  issues by keyword (via the gitlab MCP `list_issues` / `search_repositories`,
  or `glab issue list -S "<keyword>"`). Use `Closes #1234` for issues this MR
  resolves, and `Part of #1234` / `Related to #1234` otherwise.
- **MR stack.** Determine whether this MR is part of a stack:
  - The MR's target branch is another open MR's source branch (not the default
    branch) — `git log <target>..HEAD` against the default branch will show the
    parent MR's commits.
  - Branch naming or commit history references a base branch (e.g.
    `user@feature-part2` on top of `user@feature-part1`).
  When stacked, state the position explicitly (`Based on !XXXX` / `Blocked by
  !XXXX`, `Followed by !YYYY`) and note that this MR should be merged after its
  base. Cross-link every MR in the stack so a reviewer can navigate it.

## Structure

Use the repo's default template sections (`.gitlab/merge_request_templates/default.md`):
**What / Why / How / Manually testing the MR**. These map directly to the four
questions every description must answer — *what*, *why*, *how*, and *how to test*.
Keep all four even if an answer is short; only omit one when it is genuinely
obvious (e.g. "what" = "fixes a typo" makes "why" self-evident).

```
<title — see Title format below>

# What

<short imperative bullets: what this MR changes, factual, no implementation detail>

# Why

<the motivation / problem being solved — one "why" for every item in What>
<related issues: `Closes #1234` / `Part of #1234`; ADR link if one exists>

# How

<technical details: how the change achieves the What, and why this is a good way>
<obvious alternative approaches considered, and why they were not chosen>
<side-effects / breaking changes, their impact, and mitigation (changelog entry)>
<MR stack position: `Based on !XXXX`, `Followed by !YYYY`>

# Manually testing the MR

<the tests a reviewer can run, with copy-pastable commands>
<link to a successful test/dry-run pipeline; before/after numbers for perf>
<for perf MRs: benchmark results>
<prerequisites / access needed to reproduce (packages, cloud credentials)>
```

`# Notes` (CI/CD variable setup, follow-up MRs, manual-trigger caveats) can be
appended when it doesn't fit cleanly under How.

## Title format

`<Component>[, <Component>]: <verb> <object>` in imperative mood, literals in
brackets. Examples: `CI/Images: fix comment for [debian-rust]`,
`Shell: fix bug in RPC handler`, `Protocol/Alpha: optimize gas computation`.
Keep the first line under 72 characters.

## What makes the description good

The goal: a reviewer should be able to answer every question they'd have from
the description alone, so there is no back-and-forth and no reason to give up on
a thorough review.

1. **One logical concern.** Keep the MR focused on a single, coherent change.
   Fixing several things in one MR is fine when they're logically related (e.g.
   parts of the same refactor). What to avoid is bundling *independent* goals or
   unrelated subsystems — that's the signal to split. Diff size alone is not the
   test: a large diff is fine when it's mostly auto-generated code (regenerated
   CI YAML, dune/opam files, snapshots). Judge by the number of independent
   concerns, not line count.

2. **Match the diff exactly.** Every file/script named in `# What` must be in
   the diff, and every non-trivial change in the diff should be mentioned. No
   phantom files, no silent changes.

3. **Title describes the change.** Don't claim something the diff doesn't do
   ("replace Trivy with GCP" when nothing is removed; "revision pipeline" when
   it's a "packaging revision" pipeline).

4. **Every "what" has a "why", and they're consistent.** If the MR does several
   things (a refactor *and* two bug fixes), each must be justified in `# Why`.
   The "why" must plausibly be solved by the "what", and the "how" must actually
   describe the "what".

5. **Make the motivation convincing.** Match the evidence to the change type:
   - **Bug fix** → show an occurrence of the bug: a link to a failing CI job,
     error logs, or a graph/table of the problem.
   - **New feature** → say who will use it and how it helps them.
   - **Refactor** → say why it's needed: it unblocks a later commit in this MR,
     or prepares a follow-up MR (give a rough idea of what that MR will do).
   - An ADR or issue link is the strongest "why" — an ADR is already
     team-validated. If the MR mentions an ADR, make sure it's linked.

6. **Discuss obvious alternatives.** If there's another reasonably obvious way
   to implement the "what" that isn't clearly worse, `# How` should say why it
   wasn't chosen (e.g. "why Rust instead of OCaml", "why Python tests instead of
   Tezt").

7. **Announce and mitigate side-effects / breaking changes.** Call out behaviors
   introduced as a means to the goal. For breaking changes (renamed command,
   changed default, dropped support): say how hard they are to avoid or how low
   the impact is, and what mitigates them — at minimum a user-facing changelog
   entry (`CHANGES.rst`, or `docs/protocols/alpha.rst` for protocol/environment).

8. **Make tests convincing and their results reachable.** Suggest tests that
   would actually convince a reviewer the code works.
   - Tests run by the MR pipeline (`before_merging` / `merge_train`) run anyway —
     **don't** ask the reviewer to run them or clutter the description with them.
     This includes most Tezt tests and sanity checks like `make -C ci; git diff`.
   - Tests needing a **different** pipeline (e.g. modifying a scheduled pipeline)
     → run it and link the successful pipeline, on a commit close to the latest.
   - Tests run **locally** by the author → include the result, or state the
     expected result ("you should get an empty diff", "you should see ...").
   - **Performance** MRs → benchmark results, not just a speed claim.

   **Get the actual link — don't leave a pipeline TODO if you can resolve it.**
   Before writing `<!-- TODO: link a pipeline -->`, do the work:
   - **Check the MR's existing pipelines first.** Query the pipelines already run
     on the MR's head (gitlab MCP `list_pipelines` / `get_pipeline` on the source
     branch or head SHA, or gitlab-pipelines MCP `get_pipeline_status`). If a
     relevant one is **green** (`success`) and on (or close to) the current head,
     link it directly in `# Manually testing the MR`. Don't link a pipeline on a
     stale/superseded commit, and don't link a `failed`/`running`/`canceled` one —
     say so to the user instead.
   - **If the needed pipeline hasn't run, trigger it and link the successful run.**
     Use the gitlab-pipelines MCP (`trigger_merge_request_pipeline` for the
     `before_merging` set; `trigger_scheduled_pipeline` with the right
     `schedule_kind` — e.g. `debian.daily`, `rpm.daily` — for scheduled job sets).
     Wait for it to finish (`get_pipeline_status`), and only link it once it is
     `success`. If it isn't green, report the outcome rather than linking a red
     pipeline.
   - **Only fall back to a `TODO` when you genuinely cannot resolve it** — e.g.
     triggering is blocked (`insufficient_granular_scope` → the token lacks
     `Pipeline: Create`), the branch isn't pushed yet, or the run is still in
     progress. State the concrete blocker in the note so the user knows exactly
     what to do.
   - The triggered pipeline runs against the branch **as pushed on the remote**;
     if you just rebased/changed commits locally, the run will be on the old tip
     until the user pushes. Trigger after the push (or flag the mismatch).

9. **Give clear reproduction instructions.** A skeptical reviewer must be able
    to re-run the tests: copy-pastable command sequences (not "run it via
    Kubernetes"), plus how to get prerequisites (system packages) and access
    (who grants the cloud credentials / permissions).

10. **Quantify performance claims.** Back any "optimizes / speeds up" claim with
    before/after numbers or side-by-side pipeline links. When it's a micro
    optimization that's hard to measure, don't make an unquantified speed claim —
    frame it as a code-quality improvement instead.

11. **Document new CI/CD variables.** When the change references a new secret /
    bucket / token, state where it's defined (group vs project) and that it must
    be Protected / Masked / Hidden.

12. **Note follow-ups and partial migrations.** If the MR only does part of a
    migration (adds an image but swaps it in one place; adds a job to be used
    later), say so and link the follow-up.

13. **Call out caveats** a reviewer should know: a job left manual, an
    append-only or destructive step.

14. **Link related issues and the MR stack.** Reference the issues this MR
    addresses (`Closes` / `Part of`), and if it's stacked on or blocking other
    MRs, state the position and cross-link them. See *Find related issues and
    the MR stack* above.

15. **Check the commits are well split.** A description can't compensate for a
    single giant commit doing several things. If the history mixes a refactor
    with feature/bug-fix work, suggest splitting it (refactor first, then one
    commit per fix) — unless splitting is impractical (mechanical sweep across
    many files, or the code wouldn't compile split). See the `write-code-and-mr`
    skill for producing well-split commits.

## Apply metadata (labels, priority, assignees, reviewers, draft)

After writing the description, set the MR metadata with `glab`. For assignees
and reviewers, the default set is **onurb, Killian-Delarue, romain.nl** — these
three are usually enough. Ask the user before adding more (e.g. neo.nl); don't
assign the full PE roster by default. Drop the author from the lists.

- **Label:** `platform-engineering` (the canonical PE label — id 43366461; *not*
  the lookalikes `platform_engineering` or `platform-compatibility`).
- **Priority:** pick one of `priority::low` / `priority::medium` /
  `priority::high` / `priority::critical`. Default to `priority::medium` unless
  the user says otherwise. (`marge-priority::high` / `marge-priority::critical`
  are separate marge-bot queue labels — only add when merge urgency is the
  point, not as the work priority.)
- **Draft:** mark the MR as draft so it isn't picked up before it's ready.

One `glab mr update` call does all of it (run from the repo; `<iid>` is the MR
number, drop the author from the assignee/reviewer lists):

```bash
glab mr update <iid> \
  --label "platform-engineering,priority::medium" \
  --assignee onurb,Killian-Delarue,romain.nl \
  --reviewer onurb,Killian-Delarue,romain.nl \
  --draft
```

Notes:
- `--label` is additive (keeps existing labels); use `--unlabel "<name>"` to
  remove one.
- Toggle draft back off with `--ready`.
- Confirm the current team list with the maintainers if membership may have
  changed.

## Pre-output self-check

Before emitting, verify the description against the points above — a reviewer
shouldn't have to ask anything it could have answered:

- [ ] What / Why / How / Manually testing all answered (or obviously moot).
- [ ] Every What has a matching, consistent Why and How.
- [ ] Motivation backed by change-appropriate evidence; alternatives, side-effects
      and breaking changes addressed (+ changelog).
- [ ] Test results reachable (green pipeline linked / local result / benchmark),
      not left for the reviewer to re-run; a pipeline `TODO` only with a stated blocker.
- [ ] Reproduction steps copy-pastable, prerequisites/access named; commits well
      split (or flagged why not).

If information is missing, leave a marked `<!-- TODO: ... -->` and tell the user
what to fill in rather than inventing it.

## Output

Produce the full title + description ready to paste, satisfying every
applicable point above for the change at hand. Then run (or print, if the user
prefers to run it themselves) the `glab mr update` command that applies the
`platform-engineering` label, the priority label, the team as
assignees/reviewers, and the draft flag.
