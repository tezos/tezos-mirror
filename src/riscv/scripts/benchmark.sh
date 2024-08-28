#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Build & run jstz on two different commits,
# where the first commit is the "base / master" commit hash
# and the second commit is the current change / MR tested

set -e

# Iterations for RISCV and for native are different because usually for native you would want way more runs
USAGE="Usage: -t <num_transfers> -i <num_iterations_riscv> -n <num_iterations_riscv> -c <change_commit_hash> [ -b <base_commit_hash>: default master ]"

RISCV_IT=""
NATIVE_IT=""
TX=""
BASE_COMM="master"
CHANGE_COMM=""
NATIVE=""

while getopts "t:i:n:c:b" OPTION; do
  case "$OPTION" in
  t)
    TX="$OPTARG"
    ;;
  i)
    RISCV_IT="$OPTARG"
    ;;
  n)
    NATIVE_IT="$OPTARG"
    ;;
  c)
    CHANGE_COMM="$OPTARG"
    ;;
  b)
    BASE_COMM="$OPTARG"
    ;;
  *)
    echo "$USAGE"
    exit 1
    ;;
  esac
done

if [ -z "$TX" ] || [ -z "$RISCV_IT" ] || [ -z "$NATIVE_IT" ] || [ -z "$CHANGE_COMM" ]; then
  echo "$USAGE"
  exit 1
fi

NATIVE=$(make --silent -C jstz print-native-target | grep -wv make)

CURR=$(pwd)
RISCV_DIR=$(dirname "$0")/..
cd "$RISCV_DIR"

git checkout "$CHANGE_COMM" --quiet

echo "[INFO]: Run on change commit ($CHANGE_COMM) on ($NATIVE)"
"$RISCV_DIR/scripts/jstz-bench.sh" -t "$TX" -i "$NATIVE_IT" -s -n 2>&1 | tee "CHANGE_NATIVE_$CHANGE_COMM.run"

echo "[INFO]: Run on change commit ($CHANGE_COMM) on (RISCV)"
"$RISCV_DIR/scripts/jstz-bench.sh" -t "$TX" -i "$RISCV_IT" -s 2>&1 | tee "CHANGE_RISCV_$CHANGE_COMM.run"

git checkout "$BASE_COMM" --quiet

echo "[INFO]: Run on base commit ($BASE_COMM) on ($NATIVE)"
"$RISCV_DIR/scripts/jstz-bench.sh" -t "$TX" -i "$NATIVE_IT" -s -n 2>&1 | tee "BASE_NATIVE_$BASE_COMM.run"

echo "[INFO]: Run on base commit ($BASE_COMM) on (RISCV)"
"$RISCV_DIR/scripts/jstz-bench.sh" -t "$TX" -i "$RISCV_IT" -s 2>&1 | tee "BASE_RISCV_$BASE_COMM.run"

cd "$CURR"
