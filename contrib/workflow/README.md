# Cue Workflow

Code agents contributing to this repository use the
[cue](https://gitlab.com/cue-store/cue) CLI task manager. This
directory documents their workflow. The workflow consists in hooks
(shell scripts used as git hooks and cue hooks) and documentation
(Markdown documents specifying in a declarative style how agents
behave when working on a task).

## Common cue commands

The most commonly used `cue` commands are:

- `cue get <field>`: read a field from the task header
- `cue set assignee="$(cue get author)"`: assign the task to its
  author
- `cue set status=<new_status>`: attempt a status transition
- `echo "<task-description>" | cue edit`: edit the description of the task
- `echo "<task-description>" | cue add <task-name>`: create a new task
- `cue --help`: list all available `cue` commands and their options

## Fields

Tasks are routed through the workflow using three fields: `track`,
`criticality`, and `status`. The `track` field represents the nature of
the task, the `criticality` field represents the impact the task can
have on users, the `status` field represents the progression toward
the completion of the task.

### Track

The possible values for the `track` field are:

- nothing: when the track of the task is not yet known, the `track`
  field is not set,
- `question`: when the task description contains one or several
  questions, the task consists in answering them,
- `workflow`: modifying the cue hooks and the documentation of this
  workflow,
- `test-only`: adding, removing, or modifying tests,
- `doc-only`: improving the documentation,
- `code`: modifying software,

### Criticality

The possible values for the `criticality` field are:

- nothing: when the criticality of the task is not yet known, the
  `criticality` field is not set,
- `critical`: some changes impact users of the software, not correctly
  executing the task can lead to incidents,
- `behind-feature-flag`: all changes are behind a feature flag and
  this feature flag is currently not set in production, the task
  cannot impact users as long as the feature flag remains unset,
- `non-critical`: the task cannot impact users at all.

### Status

The possible values for the `status` field are: `triage`, `design`,
`poc`, `decomposition`, `estimation`, `test`, `execution`,
`documentation`, `agent-review`, `human-review`, `learning`, and
`done`.

If the status field is not set, the agent sets the status to `triage`
and stops immediately.

For each status, the status-specific instructions for this status are
documented in a file named <status>.md within this directory.

## Agent runs

At each run the agents perform the following steps:
- read the `status`, `track`, and `criticality` fields of the task,
- read the status-specific documentation,
- follow the status-specific instructions,
- evaluate how confident it is that it correctly followed the process
  for this status (on a scale from 0 to 10, 0 means the process was
  ignored, 10 means the process was perfectly followed),
- if the confidence evaluation is above 8/10, attempt one (and only
  one) status transition,
- otherwise escalate the task by assigning it to its author,
- output a message to summarize what the agent did during the run,
- stop.
