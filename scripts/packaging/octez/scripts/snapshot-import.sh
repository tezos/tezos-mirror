#!/bin/sh

set -eu

# Check if the script is run as the 'tezos' user
if [ "$(whoami)" != "tezos" ]; then
  echo "Error: This script must be run as the 'tezos' user."
  echo "Run this script as sudo su tezos -c $0"
  exit 1
fi

# Default values
NETWORK=mainnet
HISTORY_MODE=rolling
SNAPSHOT_NO_CHECK=""

if [ -e /etc/default/octez-node ]; then
  #shellcheck disable=SC1091
  . /etc/default/octez-node
fi

# Function to display help message
display_help() {
  echo "Usage: $0 [options]"
  echo
  echo "Options:"
  echo "  -n NETWORK           Specify the Tezos network (e.g., mainnet (default), ghostnet)."
  echo "  -h HISTORY_MODE      Specify the history mode (e.g., rolling (default), full)."
  echo "  -s SNAPSHOT_OPTION   Specify the snapshot import option (e.g., --no-check)."
  echo "  --help               Display this help message."
  echo
  echo "Example:"
  echo "  $0 -n ghostnet -m rolling -s --no-check"
  exit 0
}

# Parse command-line arguments
while [ "$#" -gt 0 ]; do
  case "$1" in
  -n)
    NETWORK="$2"
    shift 2
    ;;
  -h)
    HISTORY_MODE="$2"
    shift 2
    ;;
  -s)
    SNAPSHOT_NO_CHECK="$2"
    shift 2
    ;;
  --help)
    display_help
    ;;
  *)
    echo "Unknown option: $1"
    display_help
    ;;
  esac
done

# Set up a trap to remove the file on error (non-zero exit status)
trap 'rm -f /tmp/$HISTORY_MODE' EXIT

# We need access to the config.json as the tezos user.
# We simply check if the config file exist. But we don't
# try to update the octez node configuration. Trying to
# do anything automatically might lead to data loss.
if [ ! -e "$DATADIR/config.json" ]; then
  echo "Init node in $DATADIR"
  /usr/bin/octez-node config init --data-dir="$DATADIR" \
    --network="$NETWORK" \
    --history-mode="$HISTORY_MODE" \
    --net-addr="[::]:9732" \
    --rpc-addr="127.0.0.1:8732"
fi

# Commands execution
echo "Downloading snapshot for network: $NETWORK, history mode: $HISTORY_MODE..."
rm -f "/tmp/$HISTORY_MODE"
curl -s -o "/tmp/$HISTORY_MODE" "https://snapshots.tzinit.org/$NETWORK/$HISTORY_MODE"

echo "Importing snapshot with option: $SNAPSHOT_NO_CHECK..."
# shellcheck disable=SC2086
octez-node snapshot import $SNAPSHOT_NO_CHECK "/tmp/$HISTORY_MODE"

echo "Cleaning up temporary files..."
rm "/tmp/$HISTORY_MODE"

echo "Snapshot import completed!"
