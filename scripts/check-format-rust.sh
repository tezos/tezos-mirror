#!/usr/bin/env bash

set -e

self=$(dirname "$0")
repo_args=()
ls_args=()
fmt_args="--check"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  --fix)
    fmt_args=""
    shift
    ;;
  -h | --help)
    echo "Usage: $0 [--fix]"
    echo ""
    echo "Options:"
    echo "  --fix             Apply formatting changes instead of just checking"
    echo "  -h, --help        Show this help message"
    exit 0
    ;;
  *)
    echo "Unknown option: $1"
    echo "Use --help for usage information"
    exit 1
    ;;
  esac
done

# Are we using Jujutsu?
if ! git rev-parse --show-toplevel 2> /dev/null && jj root 2> /dev/null > /dev/null; then
  jj_root="$(jj root)"
  jj_tree="$(jj log -r @ -T commit_id --no-graph)"

  # shellcheck disable=SC2016
  repo_args+=(-C "$jj_root/.jj/repo/store/git")
  ls_args+=("--with-tree=$jj_tree")
fi

format_all() {
  # shellcheck disable=SC2016
  xargs -0 -I{} sh -c "cd $self/../\$(dirname {}) && cargo fmt $fmt_args"
}

find_files() {
  # shellcheck disable=SC2016
  git "${repo_args[@]}" ls-files "${ls_args[@]}" -z --full-name \
    'Cargo.lock' \
    '**/Cargo.lock' \
    ':!:src/riscv' # Uses a nightly formatter which doesn't get picked up automatically
}

find_files | format_all
