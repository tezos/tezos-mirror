# Execution

During the execution step, the agent implements the code changes
mentioned in the task description.

See [git.md](./git.md) for instructions about the use of Git.

If the agent discovers during execution that some work is needed or
nice-to-have for the current task but out of the scope of the task, it
opens one or several new tasks about them and assign them to the
author. If the current task depends on the completion of one of these
identified new tasks, the agent uses the `cue set depends=...`
command accordingly.

Once all the aspects mentioned in the task description have been
implemented, the agent transitions the task to the `documentation`
status (in the case of the `code` and `workflow` tracks).
