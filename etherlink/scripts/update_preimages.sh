#!/usr/bin/env sh

set -e

usage() {
  echo "usage: etherlink/scripts/update_preimages.sh <GIT_COMMIT>... [--commits C1,C2,...] [--network NETWORK]"
}

NETWORK="mainnet"
# Collect the git commits as positional arguments while extracting the
# optional --network flag (which may appear anywhere on the command line).
# COMMITS accumulates the commit hashes (safe to word-split: they contain
# no whitespace or glob characters).
COMMITS=""
while [ "$#" -gt 0 ]; do
  case "$1" in
  --network)
    [ "$#" -ge 2 ] || {
      echo "missing value for --network" >&2
      usage
      exit 1
    }
    NETWORK="$2"
    shift 2
    ;;
  --network=*)
    NETWORK="${1#--network=}"
    shift
    ;;
  --commits)
    [ "$#" -ge 2 ] || {
      echo "missing value for --commits" >&2
      usage
      exit 1
    }
    # Split the comma-separated list into individual commits.
    COMMITS="$COMMITS $(echo "$2" | tr ',' ' ')"
    shift 2
    ;;
  --commits=*)
    COMMITS="$COMMITS $(echo "${1#--commits=}" | tr ',' ' ')"
    shift
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  -*)
    echo "unknown option: $1" >&2
    usage
    exit 1
    ;;
  *)
    COMMITS="$COMMITS $1"
    shift
    ;;
  esac
done

# shellcheck disable=SC2086 # intentional word splitting of the commit list
set -- $COMMITS

if [ "$#" -eq 0 ]; then
  echo "no git commit provided" >&2
  usage
  exit 1
fi

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

# requires running etherlink/scripts/build-wasm.sh to have been run before,
# for each commit whose kernel we want to add to the archive.
for GIT_COMMIT in "$@"; do
  echo "adding preimages for commit $GIT_COMMIT"

  ROOT_HASH="$(sed -e 's/ROOT_HASH: //' < "etherlink/kernels-$GIT_COMMIT/root_hash")"

  smart-rollup-installer get-reveal-installer -u "etherlink/kernels-$GIT_COMMIT/evm_kernel.wasm" -P "$TMP_DIR/wasm_2_0_0" --output /dev/null > /dev/null

  # checking the preimages dir
  octez-evm-node download kernel "$ROOT_HASH" --preimages-dir "$TMP_DIR/wasm_2_0_0" --preimages-endpoint ""
done

# building the archive
tar -czf "wasm-$NETWORK.tar.gz" --no-xattrs --exclude ".*" -C "$TMP_DIR" "wasm_2_0_0"
