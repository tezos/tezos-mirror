#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Build and run the jstz TPS benchmark with the specified number of transfers

set -e

CURR=$(pwd)

RISCV_DIR=$(dirname "$0")/..

cd "$RISCV_DIR"

echo "[INFO]: building deps"
make riscv-sandbox &> /dev/null
make -C jstz build &> /dev/null

DATA_DIR=${DATA_DIR:=$(mktemp -d)}

echo "[INFO]: generating $1 transfers"
INBOX_FILE="${DATA_DIR}/inbox.json"
./jstz/inbox-bench generate --inbox-file "$INBOX_FILE" --transfers "$1"

echo "[INFO]: running $1 transfers"
LOG="${DATA_DIR}/log.out"
./riscv-sandbox rvemu \
  --input ../../tezt/tests/riscv-tests/hermit-loader \
  --initrd jstz/target/riscv64gc-unknown-hermit/release/jstz \
  --inbox-file "$INBOX_FILE" \
  --timings > "$LOG"

echo "[INFO]: collecting results"
echo -e "\033[1m"
./jstz/inbox-bench results --inbox-file "$INBOX_FILE" --log-file "$LOG"
echo -e "\033[0m"

cd "$CURR"
