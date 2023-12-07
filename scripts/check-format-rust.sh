#!/bin/sh

set -e

# shellcheck disable=SC2016
git ls-files -z '**/Cargo.lock' ':!:contrib' | \
  xargs -0 -I{} sh -c 'cd $(dirname {}) && cargo fmt --check'
