# Proof-of-concept

In the proof-of-concept ("POC" for short) step, the goal is to produce
a prototype implementation of the task to quickly validate its
feasibility.

The POC is developed on a separate git branch that will never be
merged. The name of the branch is the name of the task prefixed with
`poc@` (see [git.md](./git.md)).

Since the code of the POC is not going to be used in production nor
maintained, the quality requirements are very low.

The expected outcome of this step is code that compiles and one or a
few user stories that the author of the task can use to check if the
agent who wrote the POC had a correct understanding of the task.

The only possible transition from the `poc` status is `decomposition`.
