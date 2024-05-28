#!/bin/sh

set -eu

# Pass [-s] to [tr] to squeeze repeat (use short option) for BusyBox
# compatibility.
COVERAGE_MERGED=$(echo "$CI_JOB_NAME" | tr -s '[\/_ @[]+' '-')

# If BISECT_FILE is not set, or the ci--no-coverage label is set, we
# do not attempt to merge the coverage files
if [ -z "${BISECT_FILE:-}" ] || echo "${CI_MERGE_REQUEST_LABELS:-}" | grep -q '\(^\|,\)ci--no-coverage\($\|,\)'; then
  rm -f "$BISECT_FILE"*.coverage
  echo "Coverage is disabled."
elif bisect-ppx-report merge --coverage-path "$BISECT_FILE" "$COVERAGE_MERGED".coverage; then
  # Merge was successful, meaning that no corrupted files were found
  COVERAGE_MERGED="$COVERAGE_MERGED".coverage
  rm -f "$BISECT_FILE"*.coverage
  mv "$COVERAGE_MERGED" "$BISECT_FILE"
  echo "Merged coverage files to ${BISECT_FILE}/${COVERAGE_MERGED}"
else
  # Merge was not successful, meaning that coverage was corrupted.
  # Store failure details as an artifact that is treated in the job
  # `unified_coverage`.
  rm -f "$BISECT_FILE"*.coverage
  COVERAGE_MERGED="$COVERAGE_MERGED".corrupt.json
  printf '{"job_id": %s, "job_name": "%s", "job_web_url": "%s"}' \
    "$CI_JOB_ID" "$CI_JOB_NAME" "$CI_JOB_URL" > "$COVERAGE_MERGED"
  mv "$COVERAGE_MERGED" "$BISECT_FILE"
  echo "Corrupt coverage files detected, wrote ${BISECT_FILE}${COVERAGE_MERGED}"
fi
