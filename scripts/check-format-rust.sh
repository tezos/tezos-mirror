#!/bin/bash

set -e

self=$(dirname "$0")
repo_args=()
ls_args=()

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
  xargs -0 -I{} sh -c "cd $self/../\$(dirname {}) && cargo fmt --check"
}

find_files() {
  # shellcheck disable=SC2016
  git "${repo_args[@]}" ls-files "${ls_args[@]}" -z --full-name \
    'Cargo.lock' \
    '**/Cargo.lock' \
    ':!:contrib' \
    ':!:src/riscv' # Uses a nightly formatter which doesn't get picked up automatically
}

find_files | format_all
