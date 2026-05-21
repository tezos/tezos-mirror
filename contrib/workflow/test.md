# Test

The agent follows a strict test-driven development methodology. Before
modifying the code base, it writes the tests that will serve to
validate the task execution.

During the test step, the agent only adds, remove, or modify tests; it
does not modify the code base at all.

See [git.md](./git.md) for instructions about the use of Git,

In the `test-only` track, the agent checks that the added and modified
tests pass. If they do, it transitions the task to the `agent-review`
status.

In the `code` track, the agent checks that the added and modified
tests fail. Once the agent has added tests covering all aspects of the
yet-to-be-implemented code change described in the task and checked
that they all fail, it transitions the task to the `execution` status.
