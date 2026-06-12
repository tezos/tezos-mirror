#!/bin/sh

set -eu

start_total=$(date +%s)

# Make sure that CI_MERGE_REQUEST_DIFF_BASE_SHA is present.
if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA:-}" ] &&
  ! git cat-file -t "$CI_MERGE_REQUEST_DIFF_BASE_SHA" > /dev/null 2>&1; then
  echo "Fetching merge request diff base ${CI_MERGE_REQUEST_DIFF_BASE_SHA}..."
  start_fetch=$(date +%s)
  git fetch --depth=1 origin "$CI_MERGE_REQUEST_DIFF_BASE_SHA"
  echo "git fetch: $(($(date +%s) - start_fetch))s"
fi

start_check=$(date +%s)
CHECK_LICENSES_DIFF_BASE=${CI_MERGE_REQUEST_DIFF_BASE_SHA:-} \
  ./scripts/lint.sh --check-licenses-git-new
echo "license check: $(($(date +%s) - start_check))s"
echo "lint_check_licenses total: $(($(date +%s) - start_total))s"
