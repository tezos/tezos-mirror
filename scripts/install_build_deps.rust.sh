#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=version.sh
. "$script_dir"/version.sh

# This script installs Rust dependencies such as librustzcash and the zcash
# parameters. It assumes that cargo is installed in the system, use your package
# manager or https://rustup.rs

# by specifying the env var RUST_VERSION the user can decide to use a
# different version of rust (recommended_rust_version is a variable
# declared in scripts/version.sh)
rust_version=${RUST_VERSION:-$recommended_rust_version}

if [ "$recommended_rust_version" != "$rust_version" ]; then
  echo "\
WARNING: you selected a different version of rust. Tezos is tested only
with Rust $recommended_rust_version. Do this at your own peril."
  sleep 3
fi

if [ ! -x "$(command -v rustup)" ] && \
   [[ ! -x "$(command -v rustc)" || ! -x "$(command -v cargo)" ]]; then
    echo "The Rust compiler is not installed. Please install Rust $recommended_rust_version."
    echo "See instructions at: https://tezos.gitlab.io/introduction/howtoget.html#setup-rust"
    exit 1
fi

if ! [[ "$(rustc --version | cut -d' ' -f2)" == *"$rust_version"* ]]; then
    echo "\
Wrong Rust version. This is probably because you have used `rustup
override` in the past. Run the following command from your
favorite shell, and retry to install the dependencies:
$ rustup override unset"
    exit 1
fi

LIBRARY_DIR="${OPAM_SWITCH_PREFIX}/lib/tezos-rust-libs"
ZCASH_PARAMS="${OPAM_SWITCH_PREFIX}/share/zcash-params"

BUILD_DIR=_build_rust

function cleanup () {
    echo "Cleaning up build directory ${BUILD_DIR}"
    rm -rf "${BUILD_DIR}"
}
trap cleanup EXIT INT

mkdir -p "${LIBRARY_DIR}"
mkdir -p "${BUILD_DIR}/opam-repository"
cd "${BUILD_DIR}"/opam-repository

if [ ! -d .git ] ; then
  git init
  git remote add origin "$opam_repository_url"
fi

git fetch --depth 1 origin "$opam_repository_tag"
git reset --hard "$opam_repository_tag"

# this compilation option is important for the CI to avoid linking
# statically musl, here is just for consistency
RUSTFLAGS='-C target-feature=-crt-static' cargo build --release --manifest-path rust/Cargo.toml

## librustzcash (Sapling)
echo "Installing Rust libraries of Sapling and their header in ${LIBRARY_DIR}"
cp rust/target/release/librustzcash.a "${LIBRARY_DIR}"
cp rust/librustzcash/include/librustzcash.h "${LIBRARY_DIR}"

## BLS12-381
echo "Installing Rust libraries of BLS12-381 and their header in ${LIBRARY_DIR}"
cp rust/rustc-bls12-381/include/rustc_bls12_381.h "${LIBRARY_DIR}"
cp rust/target/release/librustc_bls12_381.a "${LIBRARY_DIR}"

## Required for Sapling.
echo "Installing Sapling parameters in ${ZCASH_PARAMS}"
rm -rf "${ZCASH_PARAMS}"
mkdir -p "${ZCASH_PARAMS}"
cp zcash-params/* "${ZCASH_PARAMS}"
