#!/usr/bin/env sh

set -e

usage() {
  echo "usage: etherlink/scripts/update_preimages.sh <GIT_COMMIT> [NETWORK]"
}

GIT_COMMIT="${1:?$(usage)}"
NETWORK="${2:-mainnet}"

case "$NETWORK" in
"mainnet" | "ghostnet" | "shadownet") ;;
*)
  echo "unsupported network: $NETWORK, expected one of 'mainnet', 'ghostnet', 'shadownet'"
  exit 1
  ;;
esac

PREIMAGES="https://snapshots.tzinit.org/etherlink-$NETWORK/wasm-$NETWORK.tar.gz"

TMP_DIR=$(mktemp -d) || {
  echo "Failed to create temp dir" >&2
  exit 1
}
readonly TMP_DIR
trap 'rm -rf -- "$TMP_DIR"' EXIT
mkdir -p "$TMP_DIR"

# downloading current preimages archive
wget "$PREIMAGES" -O "$TMP_DIR/wasm-$NETWORK.tar.gz" -o /dev/null
tar xzf "$TMP_DIR/wasm-$NETWORK.tar.gz" -C "$TMP_DIR"

# requires running etherlink/scripts/build-wasm.sh to have been run before
ROOT_HASH="$(sed -e 's/ROOT_HASH: //' < "etherlink/kernels-$GIT_COMMIT/root_hash")"

smart-rollup-installer get-reveal-installer -u "etherlink/kernels-$GIT_COMMIT/evm_kernel.wasm" -P "$TMP_DIR/wasm_2_0_0" --output /dev/null > /dev/null

# checking the preimages dir
octez-evm-node download kernel "$ROOT_HASH" --preimages-dir "$TMP_DIR/wasm_2_0_0" --preimages-endpoint ""

# building the archive
tar -czf "wasm-$NETWORK.tar.gz" --no-xattrs --exclude ".*" -C "$TMP_DIR" "wasm_2_0_0"
