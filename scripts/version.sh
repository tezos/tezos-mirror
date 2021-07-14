#! /bin/sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.12.0
opam_version=2.0
recommended_rust_version=1.52.0

## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
full_opam_repository_tag=364a3dc92d821c5de168ef561a9f2a79860cfafc

## opam_repository is an additional, tezos-specific opam repository.
## This value MUST be the same as `build_deps_image_version` in `.gitlab-ci.yml
opam_repository_tag=3255e6db26adf18182ec5d26ed2a9debf7a418bc
opam_repository_url=https://gitlab.com/tezos/opam-repository
opam_repository_git=$opam_repository_url.git
opam_repository=$opam_repository_git\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
COVERAGE_OUTPUT=_coverage_output
