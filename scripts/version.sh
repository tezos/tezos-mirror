#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.09.0
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=5aca652afd01bf0d66a9d74aa192309dcb9554b2
full_opam_repository_tag=624dc320f077db8d5f5620d557ea0e367460be14
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
