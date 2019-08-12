#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.07.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=0965ca9934ba0f86a18637984d23e4c0e8dc0100
full_opam_repository_tag=4c2bd0f948ae6a0fc602f73631cc8a8ae6e83c1d
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
