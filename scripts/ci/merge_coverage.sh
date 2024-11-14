#!/bin/sh

set -eu

coverage_merged="$CI_JOB_NAME_SLUG".coverage

# If BISECT_FILE is not set, or the ci--no-coverage label is set, we
# do not attempt to merge the coverage files
if [ -z "${BISECT_FILE:-}" ] || echo "${CI_MERGE_REQUEST_LABELS:-}" | grep -q '\(^\|,\)ci--no-coverage\($\|,\)'; then
  if [ -n "${BISECT_FILE:-}" ]; then
    rm -f "$BISECT_FILE"*.coverage
  fi
  echo "Coverage is disabled."
elif bisect-ppx-report merge --coverage-path "$BISECT_FILE" "$coverage_merged"; then
  # Merge was successful, meaning that no corrupted files were found
  rm -f "$BISECT_FILE"*.coverage
  mv "$coverage_merged" "$BISECT_FILE"
  echo "Merged coverage files to ${BISECT_FILE}${coverage_merged}"
else
  # Merge was not successful, meaning that coverage was corrupted.
  # Store failure details as an artifact that is treated in the job
  # `unified_coverage`.
  rm -f "$BISECT_FILE"*.coverage
  coverage_corrupt="${BISECT_FILE}${CI_JOB_NAME_SLUG}".corrupt.json
  printf '{"job_id": %s, "job_name": "%s", "job_web_url": "%s"}' \
    "$CI_JOB_ID" "$CI_JOB_NAME" "$CI_JOB_URL" > "$coverage_corrupt"
  echo "Corrupt coverage files detected, wrote ${coverage_corrupt}"
fi
