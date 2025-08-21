# The vision for the monorepo

While Tobi can be useful on its own, ultimately its purpose is only to solve
one part of a bigger problem: how to scale our monorepo.

## The vision

We want to be able to `cd COMPONENT` and act as if `COMPONENT` was alone
in the repository.

## Interfaces

When working on a component C, one does need to keep parts of other components in mind:

- the interface of dependencies of C;
- the interface C provides to reverse dependencies.

Interfaces are not only the list of functions, types etc.
but also the pre- and post-conditions for those functions
and the invariants that hold those functions together.

Defining clean, small interfaces makes it easier to work with dependencies
and reverse dependencies, and is a prerequisite for modularity.

## Modular tests

When one works in a repository with a single component,
one expects to find the tests for this component in this repository.
To be consistent with our vision, one should thus find the tests for `COMPONENT`
in the `COMPONENT` directory, for instance in `COMPONENT/tests`.

But the vision goes further. If you work on a project in a separate
repository, you probably do not expect to have to run tests for all reverse
dependencies for all merge requests.
A consequence of the vision is thus that if a component is modified,
only tests for this component are run by default; tests for its reverse
dependencies are not run.

With a clear interface, tests for a component can focus on testing that this
interface is satisfied. If a reverse dependencies breaks, it can mean that
a test is missing, or that the reverse dependency relied on undocumented behavior.

## Modular CI

When one works in a repository with a single component,
one expects the CI jobs related to this component to be defined in this repository,
and one does not expect to find CI jobs that are not related to this component.
To be consistent with our vision, the configuration for the CI jobs related
to `COMPONENT` should thus be in the `COMPONENT` directory, for instance in `COMPONENT/ci`.

The pipelines for merge requests would contain the union of the jobs for all components,
but it would only trigger those that are relevant for the current merge request.
Which is easy if all components are separated in different directories
and if we don't require running the tests for reverse dependencies.
A similar approach also makes sense for `master` branch pipelines.

Each component should also have its own dedicated scheduled pipelines.
If such a pipeline fails, it does not interfere with the pipelines of other components.

## Modular documentation

When one works in a repository with a single component,
one may expect to find the documentation for this component in this repository.
To be consistent with our vision, one should thus find the documentation of `COMPONENT`
in the `COMPONENT` directory, for instance in `COMPONENT/doc`.
One should also be able to build this documentation independently.

That being said, this does not apply to all documentation.
One can distinguish between components with loosely-connected documentation,
and those with strongly-connected documentation.

Loosely-connected documentation is documentation with few mutual references between
the documentation of those components.
Tobi's documentation is a good example.
For such components, it makes sense to keep the documentation independent
in a dedicated directory `COMPONENT/doc`.
If needed, each component can choose its own build system for the documentation,
its own style etc.

Strongly-connected documentation is the opposite.
Sometimes, the documentation of multiple components share a significant set of common concepts
that do not naturally belong to one particular component.
Hence, those concepts are hard to explain in isolation.
This can manifest itself in the form of circular dependencies between those documentations.
This can also imply a large amount of cross-references and a need to check them automatically.
In that case, it can make more sense to regroup the documentation of those components
and to share the same build system.

The documentation at [https://octez.tezos.com/docs/](https://octez.tezos.com/docs/)
is a good example of strongly-connected documentation.
In a way, this documentation is its own, separate component,
that depends on several other components and documents them.

It doesn't mean that one cannot publish both kinds of documentations together.
If one imagines a library, loosely-connected documentations would be individual
books in a shelf, containing independent stories, while a strongly-connected documentation
would be a thick book in the shelf (a "bible") consisting of several stories that reference each other.
The library itself is the book's index that allows to navigate between the documentation
of all those components.

## Modular packages

When one works in a repository with a single component,
all dependencies are external by definition
(otherwise, the repository would contain more than one component).
As such they are usually installed using package managers such as Opam, npm etc.
To be consistent with our vision, one should thus be able
to install other components as if they were external dependencies.
Tobi is a way to do that.
It is a package manager that specializes in internal dependencies
to be able to act as if they were external.

## Modular releases

When one works in a repository with a single component,
one may expect to be able to release the component as its own package.
To be consistent with our vision, one should thus be able
to release a single component at a time.
The CI specification in `COMPONENT/ci` should be able to define a release pipeline,
for instance a pipeline triggered by a tag named `COMPONENT-VERSION`,
that would build and publish `COMPONENT`.
