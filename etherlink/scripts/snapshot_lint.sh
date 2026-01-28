#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

# Compares a kernel snapshot directory (from the worktree) against
# etherlink/kernel_latest as it existed in a given branch.
#
# Usage: snapshot_lint.sh <path> <branch>
#
# <path> is the path to the snapshot directory (e.g. etherlink/kernel_farfadet).
# Only .rs, .bin, and Cargo.toml files are compared.

set -euo pipefail

# Color support (when outputting to a terminal)
if [ -t 1 ]; then
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  BOLD='\033[1m'
  RESET='\033[0m'
else
  RED=''
  GREEN=''
  BOLD=''
  RESET=''
fi

if [ $# -ne 2 ]; then
  echo "Usage: $0 <path> <branch>" >&2
  echo "  <path>    path to the snapshot directory (e.g. etherlink/kernel_farfadet)" >&2
  echo "  <branch>  git ref for kernel_latest source (e.g. upstream/etherlink-farfadet)" >&2
  exit 1
fi

BRANCH="$2"

if [ ! -d "$1" ]; then
  echo "Error: directory $1 does not exist" >&2
  exit 1
fi

SNAPSHOT_PATH="$(cd "$1" && pwd)"

REPO_ROOT="$(git rev-parse --show-toplevel)"

# Verify the branch ref exists
if ! git -C "$REPO_ROOT" rev-parse --verify "$BRANCH" > /dev/null 2>&1; then
  echo "Error: git ref '$BRANCH' does not exist" >&2
  exit 1
fi

# Compute the snapshot directory name for display
SNAPSHOT_DIR="$(basename "$SNAPSHOT_PATH")"

KERNEL_LATEST_PREFIX="etherlink/kernel_latest"

found_diff=0

# Pass 1: Walk the worktree snapshot directory
while IFS= read -r -d '' file; do
  rel="${file#"$SNAPSHOT_PATH/"}"
  branch_path="$KERNEL_LATEST_PREFIX/$rel"

  branch_content=$(git -C "$REPO_ROOT" show "$BRANCH:$branch_path" 2> /dev/null) || {
    echo -e "${GREEN}Only in ${BOLD}$SNAPSHOT_DIR${RESET}${GREEN}: $rel${RESET}"
    found_diff=1
    continue
  }

  file_diff=$(diff -u --color=always --label "$BRANCH:$branch_path" --label "$SNAPSHOT_DIR/$rel" \
    <(echo "$branch_content") "$file" 2> /dev/null) || true

  if [ -n "$file_diff" ]; then
    echo -e "$file_diff"
    found_diff=1
  fi
done < <(find "$SNAPSHOT_PATH" -path '*/target' -prune -o \( -name '*.rs' -o -name '*.bin' -o -name 'Cargo.toml' \) -print0)

# Pass 2: Walk kernel_latest on the branch, find files missing from snapshot
while IFS= read -r branch_file; do
  rel="${branch_file#"$KERNEL_LATEST_PREFIX/"}"
  local_file="$SNAPSHOT_PATH/$rel"

  if [ ! -f "$local_file" ]; then
    echo -e "${RED}Only in ${BOLD}$BRANCH:$KERNEL_LATEST_PREFIX${RESET}${RED}: $rel${RESET}"
    found_diff=1
  fi
done < <(git -C "$REPO_ROOT" ls-tree -r --name-only "$BRANCH" -- "$KERNEL_LATEST_PREFIX/" |
  grep -E '(\.rs|\.bin|Cargo\.toml)$')

exit $found_diff
