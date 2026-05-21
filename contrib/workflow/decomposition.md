# Decomposition

During the decomposition step, the agent decomposes the task into
atomic subtasks. Atomicity is not a matter of size, a task is
considered atomic if none of its subparts can be executed
independently of the others.

For example, adding two new features can be decomposed (one subtask
per feature) but adding a feature and testing this feature cannot be
decomposed into subtasks because the workflow prevents adding failed
tests and untested features.

If the task is already atomic, or if it has already been decomposed in
a previous iteration, the agent transitions it to the `estimation`
status. Otherwise, the decomposition is done as follows:
- A cue project is created whose name is simply the name of the task
  being decomposed as returned by `cue claim`: `cue project add "$(cue
  claim)"`.
- A new task is opened for each identified atomic subtask using the
  `cue add <subtask-name> project="$(cue claim)"` command (the
  `post-add` hook inherits the author from the current claim and
  initializes the status to `triage`). When the subtask is created,
  dependencies to other tasks can be set using the `depends` field.
- The task being decomposed is added to the project and marked as
  depending on all the identified subtasks (with `cue set
  project=$(cue claim) depends=...`).
- The agent stops without changing the status of the task.
