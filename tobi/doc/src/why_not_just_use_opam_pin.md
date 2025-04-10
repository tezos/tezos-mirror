# Why not just use `opam pin`

`tobi install` is similar to `opam pin`: it installs a specific version of a package
into `_opam`. For instance, instead of:

    tobi install octez-libs

you could run something like:

    opam pin octez-libs git+file:///$PWD#HEAD

So why not just use `opam pin`?

## Opam pin is not recursive

`opam pin octez-libs` would only install `octez-libs`.
In fact, it would fail unless you also pinned the dependencies of `octez-libs` first.
Tobi recursively installs dependencies for you.

## Opam pin is slow

Compared to `tobi install`, `opam pin` is very slow.
This is due to the following reasons.

- `opam pin` runs a general solver on all dependencies, including external dependencies.
  Solving dependencies is an NP-complete problem in general.
  Tobi's solver only deals with equality constraints.
  This results in a problem that can be solved in linear time
  (on the number of nodes plus the number of dependency relationships).

- `opam pin` clones the whole repository.
  Tobi only clones the paths that are part of the component you want to install.

- `opam pin` copies build artifacts into `_opam`.
  Tobi creates symbolic links from `_opam` to `_tobi/cache`.
  This makes switching between two versions of a component faster.

## Hiding packages from the build system

Depending on the build system, `opam pin` may not be enough.
For instance, if Dune sees the source of a package,
it builds it instead of using the installed version.
One has to pass the `--only-packages` flag to `dune build`,
with a non-trivial list of packages, to make Dune ignore those source files.
`tobi build` does that for you, and it would have a harder time doing that
if packages were installed with `opam pin`.

## Drawback

As a drawback, `opam pin` won't list packages installed by Tobi,
even though they are installed in `_opam`.
