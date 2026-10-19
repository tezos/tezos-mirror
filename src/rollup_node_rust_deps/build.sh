#!/bin/sh

# SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Build script for the rollup node Rust dependencies.

set -e

OUT_DIR=$(dirname "$0")
# Dune passes a persistent cache outside its context directory.
CARGO_TARGET_DIR="${1:?Dune must supply the Cargo cache directory}"

# The remaining arguments are the shared Cargo build command supplied by Dune.
shift

export OCTEZ_RUST_DEPS_NO_WASMER_HEADERS=1
export CARGO_TARGET_DIR

# librocksdb-sys runs bindgen, which needs libclang and clang's builtin
# headers to parse rocksdb/c.h. Cargo names neither package when they are
# missing, so say it here. Match on cargo's message rather than probe the
# filesystem: on Fedora the headers are installed where clang cannot resolve
# them, so their presence proves nothing.
build_log=$(mktemp)
cargo_status=$(mktemp)

# errexit off so a failing cargo does not kill the subshell before its exit
# code is recorded. /bin/sh has no pipefail, hence the status file.
set +e
{
  "$@" --release --locked --target-dir="$CARGO_TARGET_DIR" 2>&1
  echo "$?" > "$cargo_status"
} | tee "$build_log"
set -e

status=$(cat "$cargo_status")
rm -f "$cargo_status"
[ -n "$status" ] || status=1

if [ "$status" -ne 0 ]; then
  if grep -qE 'libclang|stdbool\.h' "$build_log"; then
    cat >&2 << 'EOF'

Error: RocksDB's bindgen step could not use libclang. Install it with:

  Debian, Ubuntu       libclang-dev
  Fedora, RHEL, Rocky  clang-devel
  macOS                Xcode command line tools, or the Homebrew llvm formula

If libclang is installed elsewhere, set LIBCLANG_PATH to the directory
holding it.
EOF
  fi
  rm -f "$build_log"
  exit "$status"
fi
rm -f "$build_log"

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
