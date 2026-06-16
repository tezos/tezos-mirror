#!/bin/sh

# SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Build script for the rollup node Rust dependencies.

set -e

OUT_DIR=$(dirname "$0")
CARGO_TARGET_DIR="${OCTEZ_ROLLUP_NODE_RUST_DEPS_TARGET_DIR:-$OUT_DIR/target}"

export OCTEZ_RUST_DEPS_NO_WASMER_HEADERS=1
export CARGO_TARGET_DIR

cargo build --release --locked --target-dir="$CARGO_TARGET_DIR"

cp -f "$CARGO_TARGET_DIR/release/liboctez_rollup_node_rust_deps.a" "$OUT_DIR/liboctez_rollup_node_rust_deps.a"

if [ -r "$CARGO_TARGET_DIR/release/liboctez_rollup_node_rust_deps.so" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_rollup_node_rust_deps.so" "$OUT_DIR/dlloctez_rollup_node_rust_deps.so"
elif [ -r "$CARGO_TARGET_DIR/release/liboctez_rollup_node_rust_deps.dylib" ]; then
  cp -f "$CARGO_TARGET_DIR/release/liboctez_rollup_node_rust_deps.dylib" "$OUT_DIR/dlloctez_rollup_node_rust_deps.so"
else
  # Staticlib-only: create a stub .so for dune's bytecode mode.
  # The real symbols are linked from the .a archive in native mode.
  cp -f "$OUT_DIR/liboctez_rollup_node_rust_deps.a" "$OUT_DIR/dlloctez_rollup_node_rust_deps.so"
fi
