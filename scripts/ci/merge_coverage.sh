#!/bin/sh

set -eu

COVERAGE_MERGED=$(echo "$CI_JOB_NAME" | tr --squeeze-repeats '[\/_ @[]+' '-')

# If the ci--no-coverage label is set, we do not attempt to merge the coverage files
if echo "${CI_MERGE_REQUEST_LABELS:-}" | grep -q '\(^\|,\)ci--no-coverage\($\|,\)' ; then
    rm "$BISECT_FILE"*.coverage || true
    echo "Coverage is disabled."
elif bisect-ppx-report merge --coverage-path "$BISECT_FILE" "$COVERAGE_MERGED".coverage; then
    # Merge was successful, meaning that no corrupted files were found
    COVERAGE_MERGED="$COVERAGE_MERGED".coverage
    rm "$BISECT_FILE"*.coverage || true
    mv "$COVERAGE_MERGED" "$BISECT_FILE"
    echo "Merged coverage files to ${BISECT_FILE}/${COVERAGE_MERGED}"
else
    # Merge was not successful, meaning that coverage was corrupted
    rm "$BISECT_FILE"*.coverage || true
    echo "Corrupted coverage files were found, please report this in https://gitlab.com/tezos/tezos/-/issues/1529";
    if [ "${SLACK_COVERAGE_TOKEN:-}" != "" ]; then
        scripts/send_slack_alert_coverage.sh "$SLACK_COVERAGE_TOKEN" "$SLACK_COVERAGE_CHANNEL" "$CI_PIPELINE_URL";
    fi
    exit 1
fi

