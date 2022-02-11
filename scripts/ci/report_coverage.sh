#!/bin/sh

if [ "$CI_COMMIT_BRANCH" = "$TEZOS_DEFAULT_BRANCH" ] || [ "$CI_MERGE_REQUEST_SOURCE_BRANCH_NAME" = "$TEZOS_DEFAULT_BRANCH" ]; then
  # On the default branch (master), we fetch coverage from the latest merged MR.
  COVERAGE_START_COMMIT=$CI_COMMIT_SHA poetry run python3 scripts/ci/coverage.py;
else
  # On the development branches, we compute coverage
  CORRUPTED_FILES=$(find "$BISECT_FILE" -name \*.corrupted.coverage -type f -print | wc -l);
  if [ "$CORRUPTED_FILES" != 0 ]; then
    echo "Corrupted files were found, please report this in https://gitlab.com/tezos/tezos/-/issues/1529:";
    find "$BISECT_FILE" -name \*.corrupted.coverage -type f -print;
    if [ "$SLACK_COVERAGE_TOKEN" != "" ]; then
      scripts/send_slack_alert_coverage.sh "$SLACK_COVERAGE_TOKEN" "$SLACK_COVERAGE_CHANNEL" "$CI_PIPELINE_URL";
    fi
    exit 1;
  fi
  make coverage-report;
  # We rewrite the output of the summary to remove the points information
  # matching the coverage regexp below.
  make coverage-report-summary | sed 's@Coverage: [[:digit:]]\+/[[:digit:]]\+ (\(.*%\))@Coverage: \1@';
  make coverage-report-cobertura
fi
