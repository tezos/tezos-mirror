# Learning

During the learning step, the agent analyses the work done by all the
agents who have worked on the task. It summarizes the main points of
friction that the agents have faced, and proposes improvements to the
workflow. The outcome of this step is a small paragraph added at the
end of the [LEARNINGS.md](../../.cue-store/LEARNINGS.md) file.

This step is optional, if no lesson is worth extracting from the task,
the agent does not modify the LEARNINGS.md file.

Once this is done, the agent assigns the task to its author with
`cue set assignee=$(cue get author)` so the author is notified that
the task awaits their decision. Only the task author can transition
the task to the `done` status.
