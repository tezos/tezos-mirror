#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.10.2
opam_version=2.0
recommended_rust_version=1.44.0

## Please update `.gitlab-ci.yml` accordingly
## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
full_opam_repository_tag=3b10ec4f01b65ac56ea15ae94b050d056ba3fbef

## opam_repository is an additional, tezos-specific opam repository.
opam_repository_tag=bbe9dcd4d8d867e5b7ba958c9d0823df833339c8
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
COVERAGE_OUTPUT=_coverage_output
