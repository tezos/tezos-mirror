# How to compile and install Tobi

All commands in this guide must be ran from the root of the repository.

## Prerequisites

To compile Tobi, you need:

- [OCaml](https://ocaml.org/)
- [Dune](https://github.com/ocaml/dune)
- [Clap](https://github.com/rbardou/clap)
- [opam-file-format](https://github.com/ocaml/opam-file-format)

To install them all, run:

    make build-deps
    eval $(opam env)

from the root of the repository.

## Run Tobi without installing

Run:

    dune exec tobi --

## Install Tobi

Run:

    dune exec tobi -- install tobi

This installs Tobi as `_opam/bin/tobi`.

If Tobi is already installed, you can also run this instead:

    tobi install tobi

## Run Tobi after installing

To check that Tobi was installed correctly, run:

    tobi --help

If this fails with exit code 127, check that `_opam/bin` is in your `PATH`.
This should be the case if you ran `eval $(opam env)`.
You can add it manually with:

    export PATH=$PWD/_opam/bin:$PATH
