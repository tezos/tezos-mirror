#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=version.sh
. "$script_dir"/version.sh

# This script installs Rust dependencies such as librustzcash and the zcash
# parameters. It assumes that cargo is installed in the system, use your package
# manager or https://rustup.rs

rust_version=1.39.0

if ! [[ "$(rustc --version | cut -d' ' -f2)" == *"$rust_version"* ]]; then
    echo "\
Wrong Rust version, use your package manager or rustup:
$ rustup toolchain install $rust_version
$ rustup default $rust_version"
    exit 1
fi

install_dir="${OPAM_SWITCH_PREFIX}/lib"
# Install the headers in `include`
HEADER_DIR="${OPAM_SWITCH_PREFIX}/include"
zcash_params="${OPAM_SWITCH_PREFIX}/share/zcash-params"

BUILD_DIR="$(mktemp -d)"
function cleanup () {
    echo "Cleaning up build directory ${BUILD_DIR}"
    rm -rf "${BUILD_DIR}"
}
trap cleanup EXIT INT

mkdir -p "${HEADER_DIR}"
mkdir -p "${BUILD_DIR}/opam-repository"
cd "${BUILD_DIR}/opam-repository"
git init
git config --local protocol.version 2
git remote add origin "$opam_repository_url"
git fetch --depth 1 origin "$opam_repository_tag"
git reset --hard "$opam_repository_tag"

echo "Installing Rust dependencies in ${install_dir}"
# this compilation option is important for the CI to avoid linking
# statically musl, here is just for consistency
RUSTFLAGS='-C target-feature=-crt-static' cargo build --release --manifest-path rust/Cargo.toml
cp rust/target/release/*.a "${install_dir}"
# NB: Add the headers that bindings require while compiling the C stubs.
echo "Installing headers in ${HEADER_DIR}"
cp rust/librustzcash/include/librustzcash.h "${HEADER_DIR}"
cp rust/rustc-bls12-381/include/rustc_bls12_381.h "${HEADER_DIR}"

echo "Installing zcash parameters in ${zcash_params}"
rm -rf "${zcash_params}"
mkdir -p "${zcash_params}"
cp zcash-params/* "${zcash_params}"

cd "${script_dir}"
rm -rf "${BUILD_DIR}"
