#!/bin/sh

## This script is not meant to be executed interactively. Instead it is meant to
## be used in other scripts to provide common variables for version numbers and
## hashes.
##
## Typical use:
## . "$script_dir"/version.sh

## `ocaml-version` should be in sync with `README.rst` and
## `lib.protocol-compiler/octez-protocol-compiler.opam`
##
## This script is also sourced in the Makefile, as such it should be compatible
## with both the make and sh syntax

export ocaml_version=4.14.0
export opam_version=2
export recommended_rust_version=1.60.0
export recommended_node_version=16.18.1

## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
export full_opam_repository_tag=87ad2e05409b6a345cda6cc5762f877a49e9b7de

## opam_repository is an additional, tezos-specific opam repository.
## This value MUST be the same as `build_deps_image_version` in `.gitlab/ci/templates.yml
export opam_repository_url=https://gitlab.com/tezos/opam-repository
export opam_repository_tag=d4ea347f85148486daaf6246bf70ea79a4613fb8
export opam_repository_git=$opam_repository_url.git
export opam_repository=$opam_repository_git\#$opam_repository_tag

## Other variables, used both in Makefile and scripts
export COVERAGE_OUTPUT=_coverage_output

export sapling_output_parameters_sha256=2f0ebbcbb9bb0bcffe95a397e7eba89c29eb4dde6191c339db88570e3f3fb0e4
export sapling_spend_parameters_sha256=8e48ffd23abb3a5fd9c5589204f32d9c31285a04b78096ba40a79b75677efc13
