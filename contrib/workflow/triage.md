# Triage

During the `triage` step, the agent reads the task description (with
the `cue show` command) and sets the appropriate `track` and
`criticality` fields (using the `cue set track=<value>
criticality=<value>` command). See [README.md](./README.md) for the list
of possible values for the `track` and `criticality` fields.

If the task description is too vague to set the track or criticality
confidently, the agent does not set the fields and instead assigns the
task to its author who will either set the fields herself or provide
more details.

During the triage step, the agent cannot modify the task description
but it can add notes (with `cue note "<message>"`) and attach files to
the task (with `cue attach "$(cue claim)" <path>`).

The only possible status transition from the `triage` status is toward
the `design` status; this transition can only be triggered if the
`track` and `criticality` fields are both set.
