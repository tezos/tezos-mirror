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
# Used for sanity check in Makefile.
export opam_version_major=2
# The recommended rust version should equal the version set in the
# root rust-toolchain file, such that:
# rust-toolchain == recommended_rust_version
export recommended_rust_version=1.74.0
export recommended_node_version=18.18.2

# The Alpine minor version used to build the opam-repository images
# and used to run the `trigger` job in the CI. This value SHOULD
# correspond to the Alpine minor version given by the `trigger` job's
# `image:`.
export alpine_version='3.19'

## full_opam_repository is a commit hash of the public opam repository, i.e.
## https://github.com/ocaml/opam-repository
export full_opam_repository_tag=0bdad08fb7e8f889d600aa06619fdb88cd179258

## `opam_repository_tag` is a commit hash of the
## https://gitlab.com/tezos/opam-repository repository. This
## repository is used to build the build-deps images used by the
## tezos/tezos CI to run jobs and to produce the Octez Docker
## distribution.
## The value of `opam_repository_tag` MUST be reflected in the `build_deps_image_version` variable
## of `.gitlab-ci.yml`, which is ensured by running `make -C ci` from the root.
export opam_repository_tag="${OPAM_REPOSITORY_TAG:-74d935062182df67cfcbac67ee09e5dd07716f13}"

# SHA-256 hashes of the DAL SRSs, as used in 'scripts/install_dal_trusted_setup.sh' to verify
# integrity of downloaded SRS.
export dal_srsu_g1_sha=c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e
export dal_srsu_g2_sha=e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c

## Other variables, used both in Makefile and scripts
export COVERAGE_OUTPUT=_coverage_output
