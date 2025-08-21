#!/usr/bin/env bash

set -e

node_dirs=()
node_pids=()

bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
node="$bin_dir/../../_build/default/src/bin_dal_node/main.exe"

start_sandboxed_dal_node() {
  id=$1
  max_peer_id=${max_peer_id:-9}
  shift 1

  l1_rpc_port=$((18730 + id))
  port=$((29730 + id))
  rpc=$((28730 + id))
  expected_pow="${expected_pow:-0.0}"
  if [ -n "$DATA_DIR" ]; then
    node_dir="$DATA_DIR"
  else
    node_dir="$(mktemp -d -t tezos-dal-node.XXXXXXXX)"
    node_dirs+=("$node_dir")
  fi

  $node config init \
    --data-dir "$node_dir" \
    --net-addr "127.0.0.1:$port" \
    --rpc-addr "127.0.0.1:$rpc" \
    --expected-pow "$expected_pow" \
    --endpoint "http://127.0.0.1:$l1_rpc_port"

  $node run --data-dir "$node_dir" "$@" &
  node_pids+=("$!")
}

cleanup_nodes() {
  [ -z "${node_pids[0]}" ] || kill "${node_pids[@]}"
  for pid in "${node_pids[@]}"; do wait "$pid"; done
  rm -rf "${node_dirs[@]}"
}

main() {

  if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
    echo "Small script to launch a DAL node on a local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: $0 <id>"
    echo "  where <id> should be an integer between 1 and 9."
    exit 1
  fi

  cleanup() {
    set +e
    echo Cleaning up...
    cleanup_nodes
  }
  trap cleanup EXIT INT

  start_sandboxed_dal_node "$@"
  for pid in "${node_pids[@]}"; do wait "$pid"; done

}

if [ "$0" == "${BASH_SOURCE[0]}" ]; then
  main "$@"
fi
