#!/bin/sh

set -eu

if [ "${CI_COMMIT_BRANCH:-}" = "$TEZOS_DEFAULT_BRANCH" ] || [ "${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-}" = "$TEZOS_DEFAULT_BRANCH" ]; then
  # On the default branch (master), we fetch coverage from the latest merged MR.
  COVERAGE_START_COMMIT=$CI_COMMIT_SHA poetry run python3 scripts/ci/coverage.py;
else
  # On the development branches, we compute coverage
  make coverage-report;
  # We rewrite the output of the summary to remove the points information
  # matching the coverage regexp below.
  make coverage-report-summary | sed 's@Coverage: [[:digit:]]\+/[[:digit:]]\+ (\(.*%\))@Coverage: \1@';
  make coverage-report-cobertura
fi
