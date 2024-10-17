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

## Alternative build system

The manifest comes with a `dune` configuration that mainly serves for
`ocaml-lsp-server` users to have a working integration with `VS Code` or any
other editor with an LSP client. You can see more information about this issue in:
https://gitlab.com/tezos/tezos/-/merge_requests/3366#note_763223998

This `dune` configuration generates a fully working manifest, as such instead of
using `make -C manifest` to build, you can use `dune exec manifest/main.exe`
(from the root directory of the repository).
