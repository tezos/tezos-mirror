# Octez Manifest

This directory contains all the information which is needed to
generate `dune` and `.opam` files for Octez. Module `Manifest`
provides functions to express those `dune` and `.opam` files,
and file `main.ml` uses it to declare Dune targets and Opam packages.
File `manifest.mli` documents how to declare them.

## Usage

After you add, remove or modify a target, run `make -C manifest` from
the root directory of the repository to regenerate all `dune`
and `.opam` files. If you are removing targets, you need to remove the
corresponding `dune` and `.opam` files manually. Then, commit both
your modifications to the manifest and to the `dune` and `.opam` files.

## VS Code

If you use VS Code to edit the files in this directory, you may want
to add a `dune` file for better editor integration with `ocaml-lsp-server`.
This `dune` file can simply contain:

    (executable (name main))

Then, instead of using `make -C manifest` to build, use
`dune exec manifest/main.exe` (from the root directory of the repository).

You can see more information about this issue in:
https://gitlab.com/tezos/tezos/-/merge_requests/3366#note_763223998
