#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Find all the non-enabled tests, and explain which instructions are missing

set -e

top_level=$(git rev-parse --show-toplevel)
cd "${top_level}"/src/riscv

# Collect passing/ignored tests
echo "[INFO]: examining tests"

declare -a ignored
declare -a succeed

while read -r t; do
  status=$(echo "$t" | awk '{print $4}')
  name=$(echo "$t" | awk '{print $2}' | sed 's/test_suite_//' | sed 's/_/-/g')

  if [ "$status" = "ok" ]; then
    succeed+=("$name")
  elif [ "$status" = "ignored" ]; then
    ignored+=("$name")
  fi
done < <(cargo test -- test_suite_rv64 2> /dev/null | grep 'test_suite_rv64')

# Collect all 'passing' instructions
echo "[INFO]: collecting tested instructions"

generated_path=${top_level}/src/riscv/assets/generated

declare -A working

function instrs() {
  file="$1"

  if [ ! -f "$1" ]; then
    # shellcheck disable=SC2001
    file=$(echo "$1" | sed 's/\(.*\)-/\1_/')
  fi

  riscv64-unknown-linux-gnu-objdump -d -M no-aliases "$file" |
    awk '{print $3}' | sort | uniq
}

for t in "${succeed[@]}"; do
  while read -r instr; do
    if [ -n "$instr" ]; then
      working["$instr"]=""
    fi
  done < <(instrs "$generated_path/$t")
done

echo "[INFO]: found ${#working[@]} tested instructions"

# Examing ignored test requirements
echo "[INFO]: examing ignored tests"

for t in "${ignored[@]}"; do
  echo -en "$(basename "$t")\t\t"

  while read -r instr; do
    if [ -z "$instr" ]; then
      continue
    fi

    if ! [[ -v working["$instr"] ]]; then
      echo -n "${instr},"
    fi
  done < <(instrs "$generated_path/$t")
  echo ""
done
