#!/bin/sh

set -e

self=$(dirname "$0")

format_all() {
  # shellcheck disable=SC2016
  xargs -0 -I{} sh -c "cd $self/../\$(dirname {}) && cargo fmt --check"
}

find_files_jj() {
  jj_root="$(jj root)"
  jj_tree="$(jj log -r @ -T commit_id --no-graph)"

  # shellcheck disable=SC2016
  git -C "$jj_root/.jj/repo/store/git" ls-files --full-name --with-tree="$jj_tree" -z 'Cargo.lock' '**/Cargo.lock' ':!:contrib'
}

find_files_git() {
  # shellcheck disable=SC2016
  git ls-files -z --full-name 'Cargo.lock' '**/Cargo.lock' ':!:contrib'
}

if jj root 2> /dev/null > /dev/null; then
  find_files_jj | format_all
else
  find_files_git | format_all
fi
