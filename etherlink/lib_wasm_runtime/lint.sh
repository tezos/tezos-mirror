#!/bin/bash

BUILD_LOGS="$(mktemp)" || exit 2
START_DIR="$(pwd)"
TOPLEVEL="$(git rev-parse --show-toplevel)"
FILES=("wasm_runtime_gen.ml" "wasm_runtime_gen.mli")

RUNTIME_DIR="etherlink/lib_wasm_runtime"

cd "$TOPLEVEL" || exit 3

# shellcheck disable=SC2317,SC2329
function cleanup() {
  rm -f "$BUILD_LOGS"

  cd "$START_DIR" || return
}

trap 'exit 130' SIGINT
trap 'exit 143' SIGTERM
trap 'cleanup || exit 2' EXIT

if ! make gen_wasm_runtime_bindings > "$BUILD_LOGS" 2>&1; then
  echo "'make gen_wasm_runtime_bindings' failed:"
  cat "$BUILD_LOGS"

  exit 2
fi

exit_code="0"

git diff --exit-code HEAD -- "${FILES[@]/#/$RUNTIME_DIR/ocaml-api/}" || exit_code=$?

if [ "$exit_code" -ne 0 ]; then
  echo "From the repository root, run the following to update outdated files:"
  echo "make gen_wasm_runtime_bindings"
fi

exit $exit_code
