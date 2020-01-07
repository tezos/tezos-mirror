#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.07.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
opam_repository_tag=060f4a470db0f20af1027b0f93533c37e995992e
full_opam_repository_tag=b394cb53d91e5083ebaab5d5d7b94997ad77d716
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag
