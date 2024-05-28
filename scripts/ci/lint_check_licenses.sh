#!/bin/sh

set -eu

# Make sure that CI_MERGE_REQUEST_DIFF_BASE_SHA is present.
if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}" ] &&
  ! git cat-file -t "$CI_MERGE_REQUEST_DIFF_BASE_SHA" > /dev/null 2>&1; then
  git fetch origin "$CI_MERGE_REQUEST_DIFF_BASE_SHA"
fi
CHECK_LICENSES_DIFF_BASE=${CI_MERGE_REQUEST_DIFF_BASE_SHA:-} \
  ./scripts/lint.sh --check-licenses-git-new
