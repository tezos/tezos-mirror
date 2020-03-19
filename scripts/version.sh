#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.09.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=b81783bc01f71a6f66ae2f63b302bdd0f240f01b
full_opam_repository_tag=e7a02ec1b7c8fdfd71c834549fa71eb3b984a9fa
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
