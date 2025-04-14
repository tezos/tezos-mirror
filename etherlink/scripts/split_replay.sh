#!/bin/bash

set -e

if [ "$#" -ne 4 ]; then
  echo "Usage: $0 <start_block> <end_block> <data_dir> <num_splits>"
  exit 1
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
