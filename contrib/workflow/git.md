# Git

In addition to the general rules from [AGENTS.md](../../AGENTS.md),
agents following the cue workflow use git as follows.

## Branch name

During the `poc` step, the git branch is the name of the current task
(as returned by `cue claim`) prefixed with "poc@".

During the `test`, `execution`, and `documentation` steps, the git
branch is simply the name of the current task (as returned by `cue
claim`).

## Commit strategy

During the `poc` step, there is no constraint on the commits produced
by the agent.

During the `test`, `execution`, and `documentation` steps, commits are
small and atomic. The agent rewrites its git history to make its work
easy to review commit-by-commit.

## Clean repository and rebase

Before any file modification, the agent checks the name of the
current branch (it must match the rule above for the current status)
and rebases the current branch on top of `origin/master`.

Before transitioning out of the `test`, `execution`, `documentation`,
or `agent-review` statuses, the agent leaves the git repository clean
(no uncommitted changes, no untracked files) and keeps the current
branch rebased on top of `origin/master`. The pre-set hook enforces
both requirements.
