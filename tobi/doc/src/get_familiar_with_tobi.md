# Tutorial: get familiar with Tobi

In this Tutorial, we will install Tobi and use it to speed up compilation of `octez-node`
by installing `octez-libs`.

All instructions from this tutorial must be run from the root of the repository.

## Install Tobi

Run:

    make build-deps
    eval $(opam env)
    dune exec tobi -- install tobi

The last command outputs:

    Installing: tobi

Let's check that Tobi was installed correctly.
Run:

    tobi --help

This outputs [Tobi's main help page](tobi--help.html).

## Install Tobi with Tobi

Let's install Tobi again, but with Tobi.
And let's do it in verbose mode, to learn what Tobi is doing under the hood.
Run:

    tobi install tobi -v

This outputs:

    Installing: tobi.HEAD (explicitly requested)
    Found in cache.
    -> _opam/lib/tobi/META
    -> _opam/lib/tobi/dune-package
    -> _opam/bin/tobi

The first line is familiar: Tobi tells us that it is installing `tobi`.
But this time, it also tells us which version of Tobi it is installing: `HEAD`.
This is a Git reference to the current commit.

The second line is new: Tobi tells us that it was able to find an already-compiled
version of Tobi in its cache. This means that it didn't have to recompile anything.

The last three lines are Tobi telling us which files it installed.
Let's take a look at those files.
Run:

    ls -l _opam/lib/tobi/{META,dune-package} _opam/bin/tobi

This outputs something that looks like this (prefix truncated for brevity):

    _opam/bin/tobi -> tezos/_tobi/cache/tobi/cfedbfc7d427f7d90216d279eee0bf388dc8940b/_build/install/default/bin/tobi
    _opam/lib/tobi/dune-package -> tezos/_tobi/cache/tobi/cfedbfc7d427f7d90216d279eee0bf388dc8940b/_build/install/default/lib/tobi/dune-package
    _opam/lib/tobi/META -> tezos/_tobi/cache/tobi/cfedbfc7d427f7d90216d279eee0bf388dc8940b/_build/install/default/lib/tobi/META

meaning that the files Tobi installed in `_opam` are actually symbolic links to `_tobi/cache`.

## Play with Tobi's cache

Let's remove Tobi from its own cache and reinstall Tobi.
Run:

    rm -rf _tobi/cache/tobi
    tobi install tobi

Oops! This fails, because now Tobi is no longer installed. Let's install it via Dune instead.
Run:

    dune exec tobi -- install tobi

This outputs:

    Installing: tobi

This time it could not find the files in the cache, so it had to recompile.

## Install an actual component

Let's install `octez-libs` with Tobi. Run:

    tobi install octez-libs

This takes a while. It outputs:

    Installing: bls12-381
    Installing: octez-alcotezt
    Installing: octez-distributed-internal
    Installing: octez-distributed-lwt-internal
    Installing: octez-internal-libs
    Installing: octez-libs

Before installing `octez-libs`, Tobi installed the dependencies of `octez-libs`,
the dependencies of those dependencies, etc.

## Build the L1 node

Let's build `octez-node` with Tobi.
Run:

    tobi build octez-node

This also takes a while. It outputs:

    The following components are installed and will not be built:
    - bls12-381
    - octez-alcotezt
    - octez-distributed-internal
    - octez-distributed-lwt-internal
    - octez-internal-libs
    - octez-libs

Let's build it again in verbose mode.
Don't worry, it will be faster this time.
Run:

    tobi build octez-node -v

This outputs:

    The following components are installed and will not be built:
    - bls12-381.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    - octez-alcotezt.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    - octez-distributed-internal.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    - octez-distributed-lwt-internal.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    - octez-internal-libs.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    - octez-libs.cfedbfc7d427f7d90216d279eee0bf388dc8940b
    dune build --only-packages 'octez-rust-deps,octez-version,octez-riscv-api,octez-riscv-pvm,octez-proto-libs,octez-protocol-compiler-compat,octez-protocol-compiler,tezos-benchmark,octez-shell-libs,octez-node-config,octez-rpc-process,tezos-protocol-000-Ps9mPmXa,tezos-protocol-021-PsQuebec,tezt-tezos,tezos-dal-node-services,dal_node_migrations,octez-performance-metrics,octez-l2-libs,tezos-dal-node-lib,octez-crawler,octez-injector,octez-protocol-021-PsQuebec-libs,tezos-protocol-022-PsRiotum,octez-protocol-022-PsRiotum-libs,octez-node' '--display=progress' src/bin_node

The last line is new. It tells us that `tobi build` actually just runs `dune build`
with the `--only-packages` argument. Dune will ignore any other package.
In particular, we don't see `octez-libs` in this list, nor any of its dependencies.
The packages that we installed using Tobi are hidden from Dune when running `tobi build`.

## Simulate a Git pull and rebuild the L1 node

After a `git pull`, we get all sorts of changes.
Some of those changes may be in `octez-libs`.
Let's simulate such a situation by adding a value to `data-encoding`.
Run:

    echo 'let dummy_value = 0' >> data-encoding/src/data_encoding.ml
    echo 'val dummy_value: int' >> data-encoding/src/data_encoding.mli

Now let's rebuild `octez-node`.
Run:

    tobi build octez-node

Despite `data-encoding` being used all over the place
and being very low in the dependency tree, this takes only a few seconds.
This is the time it takes Dune to realize it has nothing to do.

The reason Dune has nothing to do is that Tobi tells it to ignore `data-encoding/src/`
and to instead use the version of `octez-libs` that Tobi installed in `_opam`.
`tobi build octez-node` will thus not recompile `octez-libs` if `octez-libs`
is installed, whether there are important changes in `octez-libs` or not.
See also [Why and when can Tobi save time](why_and_when_can_Tobi_save_time.md).

## Clean up

Throughout this tutorial, we have installed a few components.
Let's ask Tobi for the list.
Run:

    tobi list -i

`-i` tells `tobi list` to only list installed components.
This outputs:

    bls12-381
    octez-alcotezt
    octez-distributed-internal
    octez-distributed-lwt-internal
    octez-internal-libs
    octez-libs
    tobi

Let's uninstall those components.
Run:

    tobi reset

This outputs nothing.
Let's check that components were uninstalled successfully.
Run:

    tobi list -i

Oops! This fails, because Tobi is no longer installed.
Should have seen this one coming.
Let's check what is in `_opam` instead.

    ls _opam/lib/octez-libs/base

There is no file there, only empty directories.

Let's finish cleaning up by removing Tobi's cache.
You can skip this step if you intend to use the installed `octez-libs`
and don't want to waste time reinstalling it.
Run:

    rm -rf _tobi

## Conclusion

In this tutorial, we discovered the following commands:

- `tobi install`
- `tobi build`
- `tobi list`
- `tobi reset`

We used Dune to install Tobi, then Tobi to install Tobi.
We installed `octez-libs` and used this installed `octez-libs` to build `octez-node`.
We also had a look at Tobi's cache and how Tobi installs components in `_opam`.

You should now be familiar enough with Tobi to:

- speed up compilation times in some scenarios;
- store multiple versions of a component in Tobi's cache to quickly
  switch between them.

To explore more about Tobi, go back to the [index](index.md).
