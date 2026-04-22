# Design

During the design step, the agent and the author of the task iterate
on the task description to make it as precise as possible. Executing
an ambiguous task is dangerous, the goal of this step is to minimize
this risk.

The role of the agent during this step is to detect ambiguities, ask
open questions, and propose options. The agent never takes any design
decision. The agent also scans the open tasks (using `cue list
status.not:done` and `cue show <task-name>`) and sets the appropriate
dependencies (with `cue set depends=...`).

The role of the author of the task is to answer the open questions and
clarify what she had in mind when she opened the task.

The typical outcome of an agent run during the design step is to edit
the task description.

If the agent fails to find anything to improve in the task
description, it transitions the task based on the track:
- in the `question` track, no transition is performed by the agent
  (only the task author can transition to `done` when she is happy
  with the answer),
- in the `workflow`, `test-only`, and `doc-only` tracks, transition to
  `estimation`,
- in the `code` track, transition to `decomposition` if the
  feasibility of the task is obvious, otherwise to `poc`.
