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

export ocaml_version=4.14.2
# Used for sanity check in Makefile.
export opam_version_major=2
# The recommended rust version should equal the version set in the
# root rust-toolchain file, such that:
# rust-toolchain == recommended_rust_version
export recommended_rust_version=1.78.0
export recommended_node_version=18.18.2

## opam_repository is a commit hash of the public opam repository, i.e.
## https://github.com/ocaml/opam-repository
export opam_repository_tag=0f4d0ee5b69b496a4e26f305891c31400f0b4b5f

# SHA-256 hashes of the DAL SRSs, as used in 'scripts/install_dal_trusted_setup.sh' to verify
# integrity of downloaded SRS.
export dal_srsu_g1_sha=c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e
export dal_srsu_g2_sha=e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c

export dal_srs_g1_sha=1ecc6debe5fdf363afd77073111bf84947dd72b26ed7f27db8871f9279d34604
export dal_srs_g2_sha=97d76e266e657cc3c859c3359c717136f55cfe4c0256ea418f907406cb130218

##
## Versions installed in the images/ci
##

# The builds of these image also rely on variables set above.

# The Alpine minor version used to build the images/ci images
# and used to run the `trigger` job in the CI. This value SHOULD
# correspond to the Alpine minor version given by the `trigger` job's
# `image:`.
export alpine_version='3.20'

# Installed via apk rust
export rust_version='1.78.0'

# Installed via apk cargo
export cargo_version='1.78.0'

# Installed via apk opam
export opam_version='2.2.1'

# Installed via apk python3-dev
export python_version='3.12.6'

# Installed via apk
export poetry_version='1.8.3'

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

export ci_image_name="${GCP_REGISTRY}/tezos/tezos/ci"

export rust_toolchain_image_name="${GCP_REGISTRY}/tezos/tezos/rust-toolchain"

export jsonnet_image_name="${GCP_REGISTRY}/tezos/tezos/jsonnet"
