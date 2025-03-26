#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Runs the ISA test suite, gathering code-coverage metrics

set -e

# Use nightly for access to llvm-tools-preview
export RUST_TOOLCHAIN="nightly-2025-01-30"

USAGE="[ -d: install deps ]"

while getopts "dh" OPTION; do
  case "$OPTION" in
  d)
    rustup component add llvm-tools-preview
    cargo install grcov --locked --force --version "0.8.20"
    exit 0
    ;;
  h)
    echo "$USAGE"
    echo "Running without any options will generate code coverage"
    exit 0
    ;;
  *)
    echo "unrecognized option, try with -h for help"
    exit 1
    ;;
  esac
done

rm -rf target/coverage

LLVM_PROFILE_FILE="$(pwd)/target/coverage/cargo-test-%p-%m.profraw"
export LLVM_PROFILE_FILE

CARGO_INCREMENTAL=0 \
  RUSTFLAGS='-Cinstrument-coverage' \
  cargo test --workspace -- test_suite

grcov \
  target/coverage \
  --binary-path ./target/debug/deps/ \
  -s . \
  --output-type html,cobertura \
  --branch \
  --ignore-not-existing \
  --ignore '../*' \
  --ignore "/*" \
  --keep-only 'lib/src*' \
  -o target/coverage

echo "Coverage files located in ./target/coverage"

xmllint --xpath "concat('Coverage: ', 100 * string(//coverage/@line-rate), '%')" target/coverage/cobertura.xml
