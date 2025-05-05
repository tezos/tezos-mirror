#!/usr/bin/env bash

# TODO RV-622: Remove Hermit from the code base
# Remove this script once the use of Hermit has been removed from the SDK

# Ensure the toolchain is installed.
# The second command triggers installation for Rustup 1.28+.
rustup show active-toolchain || rustup toolchain install

version="$(rustc --version | awk '{print $2}')"
url="https://github.com/hermit-os/rust-std-hermit/releases/download/${version}/rust-std-${version}-riscv64gc-unknown-hermit.tar.gz"

td="$(mktemp -d)"
mkdir -p "$td"

curl -L "$url" | tar xz -C "$td" --strip-components=1
"$td/install.sh"

rm -rf "$td"
