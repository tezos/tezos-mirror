#! /bin/sh

## This script is not meant to be executed interactively. Instead it is meant to
## be used in other scripts to provide common variables for version numbers and
## hashes.
##
## Typical use:
## . "$script_dir"/version.sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/tezos-protocol-compiler.opam`

ocaml_version=4.12.1
opam_version=2.0
recommended_rust_version=1.52.1

## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
full_opam_repository_tag=b56d13f261000e799a9ff030852339661b616743

## opam_repository is an additional, tezos-specific opam repository.
## This value MUST be the same as `build_deps_image_version` in `.gitlab/ci/templates.yml
opam_repository_url=https://gitlab.com/tezos/opam-repository
opam_repository_tag=6205567ed71c685277ca111235f7eb72d51b4a79
opam_repository_git=$opam_repository_url.git
opam_repository=$opam_repository_git\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
COVERAGE_OUTPUT=_coverage_output
