#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.10.2
opam_version=2.0
recommended_rust_version=1.44.0

## Please update `.gitlab-ci.yml` accordingly
## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
full_opam_repository_tag=eb6f20daffff6dc5c67b4b55b87bf6b0e600c4d7

## opam_repository is an additional, tezos-specific opam repository.
opam_repository_tag=ecb12b00769179e6022ad1547b69950781a45061
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
COVERAGE_OUTPUT=_coverage_output
