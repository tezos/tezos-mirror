# Hooks

The cue workflow is enforced by two kinds of hooks: `cue` hooks (run
by `cue` on task-store operations) and git hooks (run by `git` on
main-repository operations).

## Installation

All hooks live in `contrib/workflow/hooks/` and are installed by a
single script:

```bash
bash contrib/workflow/hooks/install.sh
```

The script copies the git hooks into `.git/hooks/` (with `.bak`
backups of pre-existing hooks) and the cue hooks into
`.cue-store/.cue-hooks/`, and commits the cue hooks in the cue
store's git repository.

## Cue hooks

- **pre-set**: validates any `cue set` operation.
  - Field values: `track`, `criticality`, and `status` must use
    allowed values.
  - Status transitions: only transitions documented in the
    status-specific files are permitted.
  - Author bypass: when the running agent (identified by
    `CUE_AGENT_ID`) is the task author, transition restrictions are
    skipped. Only the task author can transition to `done`.
  - One transition per run: while `CUE_RUN_ID` is set, only one
    status transition is allowed per run (tracked via the
    `last-transition-run-id` field).
  - Description validation: ensures the required sections are
    present in the task description for the target status.
  - Clean repository and rebase: before transitioning out of the
    `test`, `execution`, `documentation`, or `agent-review` statuses,
    the main git repository must be clean (no uncommitted or
    untracked changes) and the current branch must be rebased on top
    of `origin/master`.

- **post-add**: finalizes a newly-created task.
  - Inherits the `author` field from the currently-claimed task, so
    a sub-task opened by an agent is authored by the human behind
    the agent's claim rather than by the agent itself.
  - Sets the initial status to `triage`.

- **post-set**: performs bookkeeping after a `cue set` operation.
  - On transition to `human-review`, sets the `assignee` to the task
    author.
  - After any status transition, records `CUE_RUN_ID` into
    `last-transition-run-id` so pre-set can enforce the
    one-transition-per-run rule.

- **pre-edit**: rejects description edits when the status is not one
  of `design` or `decomposition`.

## Git hooks

- **pre-commit**: the branch name must match the cue workflow
  conventions from [git.md](./git.md) (`poc@<task>` during the `poc`
  step, `<task>` during `test`, `execution`, or `documentation`).
- **commit-msg**: the commit message must follow the
  `<Component>: <message>` format, with the first line under 72
  characters.
