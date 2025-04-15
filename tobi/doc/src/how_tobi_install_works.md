# How `tobi install` works

## Read configuration

Tobi reads `tobi/config` and the relevant Opam files for the required components.
It does so lazily: it only reads those files when it needs to.
This is important because it may need to read files from multiple Git commits.
(In fact, all commands read those files lazily.)

## Solve dependencies

Tobi computes the set of components that needs to be installed.
This is the transitive closure of the set of components that were required by the user,
on the dependency graph, ordered using a topological sort.
This is linear in the size of the dependency graph.

The result is a list of components such that those components are all
transitive dependencies of the original set, and such that if A depends on B,
then B is before A is this list.

Such a simple algorithm is only possible because Tobi only considers dependencies
on internal components. It ignores external packages. Internal components
[can only have equality version constraints](why_only_equality_constraints_for_internal_dependencies.md).
This is what makes it possible to solve dependencies in linear time.
In general, dependency solving is NP-complete.

## Install each component one by one

For each component in the list returned by the dependency solver, Tobi does the following.

- Tobi checks out the code into `/tmp` using `git restore`.
  Only the code of this component is checked out.

- Tobi runs the build instructions from the Opam file, in this temporary directory.

- Tobi reads the resulting `.install` file.

- Tobi copies the `.install` file and all the files that are listed in this `.install` file
  into `_tobi/cache`.

- Tobi creates symbolic links from `_opam` to `_tobi/cache`,
  applying the [semantics of Opam `.install` files](https://opam.ocaml.org/doc/Manual.html#lt-pkgname-gt-install)
  (e.g. files declared in the `lib` section go into the `_opam/lib/PACKAGE` directory
  where `PACKAGE` is the Opam package name).
