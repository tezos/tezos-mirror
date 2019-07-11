#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.07.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=b98d1114e1bafbe50f017e619cbfcede77df0149
full_opam_repository_tag=4c2bd0f948ae6a0fc602f73631cc8a8ae6e83c1d
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
