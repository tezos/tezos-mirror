#!/bin/sh

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# This is a build script to be used in the Dune rule to make writing the rule less difficult.

# Ensure that failures bubble up.
set -e

OUT_DIR=$(dirname "$0")
CARGO_TARGET_DIR="${OCTEZ_RUST_DEPS_TARGET_DIR:-$OUT_DIR/target}"

# Make sure the subsequent processes have access to these.
export OCTEZ_RUST_DEPS_NO_WASMER_HEADERS=1
export CARGO_TARGET_DIR

# Build the Rust project.
cargo build --release --locked --target-dir="$CARGO_TARGET_DIR"

# Copy the built artifacts to the output directory.
cp -f "$CARGO_TARGET_DIR/release/liboctez_rust_deps.a" "$OUT_DIR/liboctez_rust_deps.a"

if [ -r "$CARGO_TARGET_DIR/release/liboctez_rust_deps.so" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_rust_deps.so" "$OUT_DIR/dlloctez_rust_deps.so"
fi

if [ -r "$CARGO_TARGET_DIR/release/liboctez_rust_deps.dylib" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_rust_deps.dylib" "$OUT_DIR/dlloctez_rust_deps.so"
fi
