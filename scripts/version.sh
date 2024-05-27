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

## full_opam_repository is a commit hash of the public opam repository, i.e.
## https://github.com/ocaml/opam-repository
export full_opam_repository_tag=0bdad08fb7e8f889d600aa06619fdb88cd179258

# SHA-256 hashes of the DAL SRSs, as used in 'scripts/install_dal_trusted_setup.sh' to verify
# integrity of downloaded SRS.
export dal_srsu_g1_sha=c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e
export dal_srsu_g2_sha=e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c

##
## Versions installed in the images/opam-repository
##

# The builds of these image also rely on variables set above.

# The Alpine minor version used to build the opam-repository images
# and used to run the `trigger` job in the CI. This value SHOULD
# correspond to the Alpine minor version given by the `trigger` job's
# `image:`.
export alpine_version='3.19'

# Installed via apk rust
export rust_version='1.76.0'

# Installed via apk cargo
export cargo_version='1.76.0'

# Installed via apk rust
export opam_version='2.1.5'

# Installed via apk python3-dev
export python_version='3.11.9'

# Installed via apk
export poetry_version='1.7.1'

##
## Other variables, used both in Makefile and scripts
##
export COVERAGE_OUTPUT=_coverage_output

##
## Image artifact repositories
##

# GCP registry
#
#  - Locally, it will point to the protected registry.
#  - In the CI, on protected (resp. unprotected) refs, it will point
#    to the protected (resp. unprotected) registry.
#
# See also the description of this variable in 'ci/bin/main.ml'.
export GCP_REGISTRY="${GCP_REGISTRY:-us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry}"

# Unprotected GCP Artifact Registry
export GCP_PUBLIC_REGISTRY='us-central1-docker.pkg.dev/nl-gitlab-runner/registry'

##
## Image names
##

# By default, points to the versions in GCP_REGISTRY. Thus by default,
# local image builds use images produced either locally, or on the
# CI's protected refs. CI image builds pushes to and pulls from protected
# (resp. unprotected) images on protected (resp. unprotected) refs.

export build_deps_image_name="${GCP_REGISTRY}/tezos/tezos/opam-repository"

export rust_toolchain_image_name="${GCP_REGISTRY}/tezos/tezos/rust-toolchain"
