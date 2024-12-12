#!/bin/sh

set -eu

# Upgrade the storage before starting the node

# Check if the script is run as the 'tezos' user
if [ "$(whoami)" != "tezos" ]; then
  echo "Error: This script must be run as the 'tezos' user."
  echo "This script is meant to run in the prestart section of octez-node.service"
  echo "Run this script with sudo su tezos -c $0"
  exit 1
fi

DATADIR=/var/tezos/.tezos-node

# Function to get a Debconf value
get_debconf_value() {
  PACKAGE=octez-node # Package name
  TEMPLATE=$1

  # Query the value using debconf-communicate
  echo GET "$TEMPLATE" |
    debconf-communicate "$PACKAGE" 2> /dev/null | awk '/^0/ {print $2}'
}

# Either we upgrade the store if exists, or we import a snapshot

if [ -d "$DATADIR/store" ]; then
  echo "Storage upgrade. This might take some time."
  octez-node upgrade storage
  rm -rf "$DATADIR/lmdb_store_to_remove"
else
  # we import the snapshot automatically only if the user wants to do so.
  if [ "$(get_debconf_value "octez-node/snapshot-import")" = "true" ]; then
    network=$(get_debconf_value "octez-node/network")
    history_mode=$(get_debconf_value "octez-node/history-mode")
    no_check=
    if [ "$(get_debconf_value "octez-node/snapshot-no-check")" = "true" ]; then
      no_check="-s --no-check"
    fi

    # shellcheck disable=SC2086
    /usr/share/octez-node/snapshot-import.sh \
      -n "$network" \
      -h "$history_mode" \
      $no_check

  fi
fi
