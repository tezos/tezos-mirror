#!/bin/bash

BUILD_LOGS="$(mktemp)"
START_DIR="$(pwd)"
TOPLEVEL="$(git rev-parse --show-toplevel)"
FILES=("wasm_runtime_gen.ml" "wasm_runtime_gen.mli")

cd "$TOPLEVEL/etherlink/lib_wasm_runtime" || exit 3

# shellcheck disable=SC2317
function cleanup() {
  for file in "${FILES[@]}"; do
    rm -f "ocaml-api/.expected-$file"
  done
  rm -f "$BUILD_LOGS"

  cd "$START_DIR" || return
}

trap 'cleanup' SIGINT SIGTERM
trap 'cleanup; exit' EXIT

# ensure the necessary toolchain is installed
rustup show active-toolchain > /dev/null 2> /dev/null
# ensure the build script will be run
touch build.rs

if ! LINT=yes cargo build 2> "$BUILD_LOGS"; then
  echo "'cargo build' failed:"
  cat "$BUILD_LOGS"

  exit 2
fi

exit_code="0"

for file in "${FILES[@]}"; do
  if ! diff "ocaml-api/$file" "ocaml-api/.expected-$file"; then
    echo "$file is outdated"
    exit_code=1
  fi
done

if [ "$exit_code" -ne 0 ]; then
  echo "Run 'cargo clean; cargo build' to regenerate outdated files"
fi

exit $exit_code
