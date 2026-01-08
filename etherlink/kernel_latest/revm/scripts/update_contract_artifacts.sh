#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 || "${1:-}" == "--help" ]]; then
  echo "Usage: $(basename "$0") <path/to/contract.sol> <path/to/constants.rs> [path/to/contract.abi]"
  echo
  echo "What it does:"
  echo "  - Compiles the given Solidity contract using forge build"
  echo "  - Outputs the build artifacts to a temporary directory"
  echo "  - Extracts the deployed bytecode and replaces the matching Rust constant"
  echo "  - If a third argument is provided, extracts the ABI and writes it to that file"
  echo
  echo "The constant is named automatically as: {UPPERCASED_FILENAME}_SOL_CONTRACT"
  echo "This constant must already exist in the code to be replaced"
  echo
  exit 0
fi

CONTRACT_PATH="$1"
CONSTANTS_FILE="$2"
ABI_FILE="${3:-}"

FILENAME="$(basename "$CONTRACT_PATH")"
CONTRACT_NAME="$(basename "$FILENAME" .sol | awk '{print toupper($0)}')"
CONSTANT_NAME="${CONTRACT_NAME}_SOL_CONTRACT"

OUT_DIR=$(mktemp -d)
JSON_PATH="$OUT_DIR/$FILENAME"

# Build the contract
forge build --via-ir "$CONTRACT_PATH" --out "$OUT_DIR" && forge clean

# Find JSON file
JSON_FILE=$(find "$JSON_PATH" -name '*.json' | head -n 1)

# Extract deployed bytecode
BYTECODE=$(jq -r '.deployedBytecode.object' "$JSON_FILE")

# Replace the constant in Rust file
perl -pi -e "s/const $CONSTANT_NAME.*/const $CONSTANT_NAME: \&str = \"$BYTECODE\";/g" "$CONSTANTS_FILE"

# Extract ABI if ABI file path is provided
if [[ -n "$ABI_FILE" ]]; then
  jq '.abi' "$JSON_FILE" > "$ABI_FILE"
  echo "Wrote ABI to $ABI_FILE"
fi

echo "Updated $CONSTANT_NAME in $CONSTANTS_FILE"
