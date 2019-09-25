#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.07.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=92376e769e50f3e1483ce186dfa475a3ee58e109
full_opam_repository_tag=f335c4dcf17eddee73cd2322205429ac6ab88476
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
