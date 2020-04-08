#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.09.1
opam_version=2.0

## Please update `.gitlab-ci.yml` accordingly
## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
full_opam_repository_tag=1399f13a876c930a5f11b87f7e5e309ab25345e3

## opam_repository is an additional, tezos-specific opam repository.
opam_repository_tag=0c374d7d54f4c4dcb39ae94f67b7fb6fb6f9eea6
opam_repository_url=https://gitlab.com/tezos/opam-repository.git
opam_repository=$opam_repository_url\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
COVERAGE_OUTPUT=_coverage_output
