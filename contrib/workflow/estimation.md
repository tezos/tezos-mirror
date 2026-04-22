# Estimation

During the estimation step, the agent scans previously-done tasks
(with `cue list status:done` and `cue show <task-name>`) to find tasks
of similar complexity.

It then relies on the tracked resources for these done tasks to
estimate the resources that will be required for the current task.

Resources are tracked on done tasks using the following fields:
`input-tokens`, `output-tokens`, `cache-read-tokens`,
`cache-creation-tokens`, `cost-usd`, `duration-ms`, `duration-api-ms`,
and `num-turns`. For each of them the estimation is set by prefixing
the field name with "est-" so for example, the input token estimation
is set with `cue set est-input-tokens`.

The agent then recommends a LLM model (with `cue set
recommended-model=...`) that will be used to execute the task. This
recommendation takes into account the difficulty of the task but also
its criticality and track.

Once all estimation fields and the `recommended-model` field are set,
the agent transitions the task to a new status depending on the track:
- in the `workflow` track, the next status is `execution`,
- in the `doc-only` track, the next status is `documentation`,
- in the `test-only` and `code` tracks, the next status is `test`.
