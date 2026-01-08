#!/usr/bin/env bash
set -euo pipefail

if [[ $# -eq 0 || "${1:-}" == "--help" ]]; then
  echo "Usage: $(basename "$0") <path/to/contract.sol> <path/to/constants.rs>"
  echo
  echo "What it does:"
  echo "  - Compiles the given Solidity contract using forge build"
  echo "  - Outputs the build artifacts to a temporary directory"
  echo "  - Extracts the deployed bytecode from the generated json"
  echo "  - Replaces the corresponding constant in the provided file"
  echo
  echo "The constant is named automatically as: {UPPERCASED_FILENAME}_SOL_CONTRACT"
  echo "This constant must already exist in the code to be replaced"
  echo "If it does not, add this placeholder: pub(crate) const {UPPERCASED_FILENAME}_SOL_CONTRACT"
  echo
  exit 0
fi

CONTRACT_PATH="$1"
CONSTANTS_FILE="$2"
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

# Replace the line defining the constant
perl -pi -e "s/const $CONSTANT_NAME.*/const $CONSTANT_NAME: \&str = \"$BYTECODE\";/g" "$CONSTANTS_FILE"

echo "Updated $CONSTANT_NAME in $CONSTANTS_FILE"
