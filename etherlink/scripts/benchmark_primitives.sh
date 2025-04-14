#!/bin/bash

set -euo pipefail

usage() {
  cat << EOF
Usage:
  $0 <cores> <subset> <graph_output_file> [--clean]

Arguments:
  cores               Number of CPU cores to use during replay
  subset              Category of contracts or operations to benchmark
  graph_output_file   Path to the output graph image (e.g. results.png)
  --clean             (Optional) Remove temporary files after execution

<subset> categories:
  general_contracts     Typical ERC contracts (ERC20, ERC1155), contract loops,
                        storage operations, and signature verification utilities.

  problematic_opcodes   Contracts containing gas-heavy or problematic EVM opcodes,
                        such as SHA3, CREATE2, SSTORE.

  gas_sinks             Contracts designed to consume large amounts of gas,
                        including deep loops or memory-intensive logic.

  precompiled           Contracts that invoke EVM precompiles:
                        ECRecover, SHA256, RIPEMD160, MODEXP, BLAKE2f.

Example:
  $0 4 gas_sinks results.png --clean
EOF
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
