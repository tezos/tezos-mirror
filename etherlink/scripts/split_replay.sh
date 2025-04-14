#!/bin/bash

set -e

usage() {
  cat << EOF
Usage:
  $0 <start_block> <end_block> <data_dir> <num_splits>

Arguments:
  start_block    Starting block number for the replay.
  end_block      Ending block number for the replay.
  data_dir       Path to the node directory containing the blocks.
  num_splits     Number of processes (or CPU cores) to parallelize the replay.

Example:
  $0 1 500 sandbox_evm_node1 4
EOF
  exit 1
}

if [ "$#" -ne 4 ]; then
  usage
fi

start_block=$1
end_block=$2
data_dir=$3
num_splits=$4

range=$((end_block - start_block + 1))
blocks_per_split=$((range / num_splits))
remainder=$((range % num_splits))
pids=()

# Trap SIGINT (Ctrl+C) and terminate all background jobs
cleanup() {
  echo "Caught SIGINT, stopping all processes..."
  for pid in "${pids[@]}"; do
    kill "$pid" 2> /dev/null || true
  done
  wait
  exit 1
}
trap cleanup SIGINT

for i in $(seq 0 $((num_splits - 1))); do
  from=$((start_block + i * blocks_per_split))
  to=$((from + blocks_per_split - 1))

  if [ "$i" -eq $((num_splits - 1)) ]; then
    to=$((to + remainder))
  fi

  echo "Launching replay from $from to $to in $data_dir"

  ./octez-evm-node replay blueprints from "$from" to "$to" \
    --data-dir "$data_dir" \
    --profile minimal &

  pids+=($!)

  # Sleep to avoid simultaneous SQL open calls
  sleep 0.2
done

wait
