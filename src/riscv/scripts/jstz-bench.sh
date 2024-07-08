#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Build and run the jstz TPS benchmark with the specified number of transfers

set -e

USAGE="Usage: -t <num_transfers> [ -s: static inbox ] [ -p: profile with samply ] [ -n: run natively ]"
DEFAULT_ROLLUP_ADDRESS="sr163Lv22CdE8QagCwf48PWDTquk6isQwv57"

TX=""
STATIC_INBOX=""
SANDBOX_BIN="riscv-sandbox"
PROFILING_WRAPPER=""
SAMPLY_OUT="riscv-sandbox-profile.json"
NATIVE=""

CURR=$(pwd)
RISCV_DIR=$(dirname "$0")/..
cd "$RISCV_DIR"

while getopts "st:pn" OPTION; do
  case "$OPTION" in
  t)
    TX="$OPTARG"
    ;;
  s)
    STATIC_INBOX="y"
    ;;
  p)
    SANDBOX_BIN="riscv-sandbox.prof"
    PROFILING_WRAPPER="samply record -s -o $SAMPLY_OUT"
    ;;
  n)
    NATIVE=$(make --silent -C jstz print-native-target | grep -wv make)
    ;;
  *)
    echo "$USAGE"
    exit 1
    ;;
  esac
done

if [ -z "$TX" ]; then
  echo "$USAGE"
  exit 1
fi

if [ -n "$NATIVE" ] && [ -z "$STATIC_INBOX" ]; then
  echo "Native compilation without static inbox unsupported"
  echo "$USAGE"
  exit 1
fi

echo "[INFO]: building sandbox"
make "$SANDBOX_BIN" &> /dev/null
echo "[INFO]: building bench tool"
make -C jstz inbox-bench &> /dev/null

DATA_DIR=${DATA_DIR:=$(mktemp -d)}

echo "[INFO]: generating $TX transfers"
INBOX_FILE="${DATA_DIR}/inbox.json"
RUN_INBOX="$INBOX_FILE"
LOG="${DATA_DIR}/log.out"
./jstz/inbox-bench generate --inbox-file "$INBOX_FILE" --transfers "$TX"

##########
# RISC-V #
##########
build_jstz_riscv() {
  if [ "$STATIC_INBOX" = "y" ]; then
    INBOX_FILE="$INBOX_FILE" make -C jstz build-kernel-static &> /dev/null
    RUN_INBOX="$DATA_DIR"/empty.json
    echo "[]" > "$RUN_INBOX"
  else
    make -C jstz build-kernel &> /dev/null
  fi
}

run_jstz_riscv() {
  $PROFILING_WRAPPER "./$SANDBOX_BIN" run \
    --pvm \
    --input ../../tezt/tests/riscv-tests/hermit-loader \
    --initrd jstz/target/riscv64gc-unknown-hermit/release/jstz \
    --inbox-file "$RUN_INBOX" \
    --address "$DEFAULT_ROLLUP_ADDRESS" \
    --timings > "$LOG" 2> /dev/null
}

##########
# native #
##########
build_jstz_native() {
  INBOX_FILE=$INBOX_FILE make -C jstz build-kernel-native &> /dev/null
}

run_jstz_native() {
  $PROFILING_WRAPPER ./jstz/target/"$NATIVE"/release/jstz \
    --timings > "$LOG" 2> /dev/null
}

#################
# Build and run #
#################
echo "[INFO]: building jstz"

if [ -z "$NATIVE" ]; then
  build_jstz_riscv
  echo "[INFO]: running $TX transfers (riscv)"
  run_jstz_riscv
else
  build_jstz_native
  echo "[INFO]: running $TX transfers ($NATIVE)"
  run_jstz_native
fi

if [ -n "$PROFILING_WRAPPER" ]; then
  echo "[INFO]: Samply data saved to: $SAMPLY_OUT"
fi

echo "[INFO]: collecting results"
echo -e "\033[1m"
./jstz/inbox-bench results --inbox-file "$INBOX_FILE" --log-file "$LOG"
echo -e "\033[0m"

if [ -n "$PROFILING_WRAPPER" ]; then
  samply load $SAMPLY_OUT
fi

cd "$CURR"
