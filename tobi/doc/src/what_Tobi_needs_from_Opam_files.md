# What Tobi needs from Opam files

Components are defined in Opam files.
Components are thus Opam packages in particular.
Tobi does not need all the information which is usually in an Opam package however.

## Dependencies on other components

The `depends` section of Opam files shall list internal dependencies in particular.
An internal dependency is a dependency on another component in the same repository.

More precisely, when reading `component.opam`, Tobi reads this `depends` section
and filters it to only keep packages with a name that matches one of the components
defined in [Tobi's configuration file](configuration_file.md).
Tobi interprets this resulting list as the list of internal dependencies of `component`.

## Version constraints

The `depends` section of Opam files may specify version constraints for internal dependencies.
Those constraints [must be equality constraints](why_only_equality_constraints_for_internal_dependencies.md)
on Git references (typically Git tags). Those Git references must correspond
to commits in which the dependency exists as a component, i.e. declared in
[Tobi's configuration file](configuration_file.md) and with an Opam file.

The `version` variable used in version constraints within an Opam file is evaluated by Tobi as the hash of the
commit of the Opam file.
For instance, if you ask Tobi to install `component.1234abcd`,
and in commit `1234abcd` the file `opam/component.opam`
mentions dependency `dep { = version }`, Tobi will install `dep` version `1234abcd`
before installing `component`.

## Build instructions

The `build` section of Opam files shall list a sequence of instructions.
Running those instructions shall produce an `.install` file
that lists the build artifacts that may be needed by other components.
This file shall be named `COMPONENT.install` where `COMPONENT` is the name of the component.
It shall follow the [Opam file format for `.install` files](https://opam.ocaml.org/doc/Manual.html#lt-pkgname-gt-install).
It is typically produced automatically by `dune build -p`.

Build instructions must be able to be run from a temporary directory
in which only the paths necessary to build the component are present.
Those paths are defined in [Tobi's configuration file](configuration_file.md).
