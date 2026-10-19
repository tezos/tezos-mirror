#!/bin/sh

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# This is a build script to be used in the Dune rule to make writing the rule less difficult.

# Ensure that failures bubble up.
set -e

OUT_DIR=$(dirname "$0")
# Dune passes a persistent cache outside its context directory.
CARGO_TARGET_DIR="${1:?Dune must supply the Cargo cache directory}"

# The remaining arguments are the shared Cargo build command supplied by Dune.
shift

# Make sure the subsequent processes have access to these.
export CARGO_TARGET_DIR

# Build the Rust project.
"$@" --release --target-dir="$CARGO_TARGET_DIR"

# Copy the built artifacts to the output directory.
cp -f "$CARGO_TARGET_DIR/release/liboctez_libcrux_ml_dsa.a" "$OUT_DIR/liboctez_libcrux_ml_dsa.a"

if [ -r "$CARGO_TARGET_DIR/release/liboctez_libcrux_ml_dsa.so" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_libcrux_ml_dsa.so" "$OUT_DIR/dlloctez_libcrux_ml_dsa.so"
fi

if [ -r "$CARGO_TARGET_DIR/release/liboctez_libcrux_ml_dsa.dylib" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_libcrux_ml_dsa.dylib" "$OUT_DIR/dlloctez_libcrux_ml_dsa.so"
fi
