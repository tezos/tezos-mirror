# How tobi build works

## Read configuration

Like [`tobi install`](how_tobi_install_works.md),
`tobi build` reads the configuration lazily.

## Solve dependencies

Like [`tobi install`](how_tobi_install_works.md),
`tobi build` solves dependencies. Technically it does not need to order
dependencies, it only needs the transitive closure.

## Find out which components are installed

Like [`tobi list --installed`](how_tobi_list_installed_works.md),
`tobi build` finds out which components are installed.

## Compute the set of visible components

Given a set of target components to build, the set of visible components is
the transitive closure of the target set, minus installed components.

## Run Dune

Finally, Tobi runs:

    dune build --only-packages VISIBLE PATHS

where:

- `VISIBLE` is the set of visible components as computed above;
- `PATHS` is the set of paths of all components that the user required to build
  (paths are configured in [Tobi's configuration file](configuration_file.md)).

This is the only real dependency that Tobi has on Dune.
To support other build systems, one would need to adapt this call.
