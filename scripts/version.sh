#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.09.0
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=3fa284bda863ab029e801005ab70ec01460ffdb8
full_opam_repository_tag=06ab3242ed00df1695e8ac04a7a0d8e1b9dd2100
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
