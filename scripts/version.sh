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

export ocaml_version=4.14.1
export opam_version=2
export recommended_rust_version=1.71.1
export recommended_node_version=18.18.2

# The Alpine minor version used to build the opam-repository images
# and used to run the `trigger` job in the CI. This value SHOULD
# correspond to the Alpine minor version given by the `trigger` job's
# `image:`.
export alpine_version='3.18'

## full_opam_repository is a commit hash of the public OPAM repository, i.e.
## https://github.com/ocaml/opam-repository
export full_opam_repository_tag=2314da5646931ec7f643bdc9aaa39177971ac857

## opam_repository is an additional, tezos-specific opam repository.
## This value MUST be reflected in the `build_deps_image_version` variable
## of `.gitlab-ci.yml`, which is ensured by running `make -C ci` from the root.
export opam_repository_url=https://gitlab.com/tezos/opam-repository
export opam_repository_tag="${OPAM_REPOSITORY_TAG:-df242dbcfa69587ccc182e4f3c3c02288407a097}"
export opam_repository_git="$opam_repository_url.git"
export opam_repository="$opam_repository_git"\#"$opam_repository_tag"

## Other variables, used both in Makefile and scripts
export COVERAGE_OUTPUT=_coverage_output
