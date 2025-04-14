#!/bin/bash

set -euo pipefail

usage() {
  echo "usage: $0 <cores> <subset> <graph_output_file>  [--clean]"
  exit 1
}

if [ $# -eq 3 ]; then
  clean_tmp=false
elif [ $# -eq 4 ] && [ "$4" == "--clean" ]; then
  clean_tmp=true
else
  usage
fi

cores=$1
subset=$2
graph_output_file=$3

log=$(mktemp)
tmp_root_dir=""

cleanup() {
  rm -f "$log"
  if $clean_tmp && [[ -n "$tmp_root_dir" && -d "$tmp_root_dir" ]]; then
    if rm -rf "$tmp_root_dir"; then
      echo "Temporary files have been removed"
    else
      echo "error: failed to delete $tmp_root_dir" >&2
    fi
  fi
}

trap cleanup INT TERM EXIT

dune exec -- etherlink/bin_benchmark_producer/main.exe --keep-temp -t "$subset" | tee "$log"

tmp_files_path=$(grep -oE '/[^ ]+/tezt-[0-9]+/[0-9]+' "$log")
last_block=$(grep -oE 'Block [0-9]+' "$log" | awk '{print $2}' | tail -1)

if [[ -z "$tmp_files_path" || -z "$last_block" ]]; then
  echo "error: failed to extract tmp_files_path or last_block from log"
  exit 1
fi

tmp_root_dir=$(dirname "$tmp_files_path")

./etherlink/scripts/split_replay.sh 1 "$last_block" "$tmp_files_path/sandbox_evm_node1" "$cores"
./etherlink/scripts/create_graph.sh "$tmp_files_path/sandbox_evm_node1/kernel_logs" "$graph_output_file"
