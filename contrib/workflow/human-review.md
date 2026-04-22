# Human Review

Tasks in `human-review` status are awaiting author review and approval.

When a task is in this status, the agent immediately reassigns it to the
author by calling:

```bash
cue set assignee=$(cue get author)
```

This ensures that human action is required and no agent loops or waits in
this status.

## Agent Behavior

Agents do not act on tasks in `human-review` status. If an agent is
assigned to a task in this status, it immediately calls the command above
to reassign the task to the author.

When transitioning a task to `human-review`, the agent should set the
assignee to the author as part of the transition (either via the
`post-set` hook if triggered by `cue set status=human-review`, or
manually if needed).

## Author Behavior

The author can transition the task to:
- `done` — to accept and complete the task
- `agent-review` — to request revisions
