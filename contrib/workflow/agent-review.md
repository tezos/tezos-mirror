# Agent review

During the agent review step, the agent reviews the git branch that
was used during the previous steps (see [git.md](./git.md)). The goal
of this step is to increase the quality of the implementation as much
as possible before involving the author.

## Reviewing the branch

The agent reviews the branch critically, reading it commit by
commit. It uses `cue note` to send its remarks. It pays particular
attention to the following aspects.

### Git history

The agent typically suggests to split commits or to reorder them to
make the history easier to read for human reviewers.

### Maintenance

The agent suggests to make the code easier to maintain, for example by
factorizing common code, using abstraction mechanisms, or leveraging
static typing.

### Security

The agent also focuses on the security implications of the change. For
example, it suggests to make the code more defensive.

## Status Transition

If the agent noted any potential improvement to the branch within the
scope of the task, it transitions the task to the `test`,
`execution`, or `documentation` status depending on the nature of the
required change. For out-of-scope improvements (for example
improvements to code which was not modified during the execution of
the task), it creates new tasks.

If the agent failed to find anything to improve, it transitions the
task to either the `human-review` status or the `learning` status
based on the track and criticality of the task:
- tasks of the `doc-only` and `workflow` tracks go to `human-review`,
- tasks of the `test-only` track go to `learning`,
- tasks of the `code` track go to `learning` if they are
  `non-critical` and to `human-review` if they are either `critical`
  or `behind-feature-flag`.
