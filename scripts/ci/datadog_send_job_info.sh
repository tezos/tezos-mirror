#!/bin/sh

# Converts dates in ISO 8601 format into timestamps.
# Compatible with the various versions of [date] available in the
# Docker images used in CI jobs (Alpine, Debian, Fedora, MacOS, etc.)
iso_to_timestamp() {
  iso_date="$1"
  clean_date="${iso_date%Z}"

  # Try GNU date (Debian, Ubuntu, etc images)
  if date -d "$clean_date" +%s 2> /dev/null; then
    return 0
  fi

  # Try BSD date (macOS)
  if date -j -f "%Y-%m-%dT%H:%M:%S" "$clean_date" +%s 2> /dev/null; then
    return 0
  fi

  # Try BusyBox date (Alpine images)
  date -D "%Y-%m-%dT%H:%M:%S" -d "$clean_date" +%s 2> /dev/null
}

# [JOB_INIT_TIME] measures the time difference between the start of
# the job and the beginning of the "step_script".
# It covers the duration of the following steps:
# - Resolving secrets
# - Preparing the "kubernetes" executor
# - Preparing environment
# - Getting source from Git repository
# - Downloading artifacts (if any)
# - Restoring cache (if any)

JOB_START_TIME=$(iso_to_timestamp "${CI_JOB_STARTED_AT}")
JOB_INIT_TIME=$((SCRIPT_STEP_BEGIN - JOB_START_TIME))
echo "JOB_INIT_TIME=$JOB_INIT_TIME"

if command -v datadog-ci > /dev/null 2>&1; then
  echo "Sending job-level info to Datadog"
  DATADOG_SITE=datadoghq.eu datadog-ci tag --level job --tags pipeline_type:"$PIPELINE_TYPE" --tags runner_tag:"$CI_RUNNER_TAGS"
  DATADOG_SITE=datadoghq.eu datadog-ci measure --level job --measures "job_init_time:$JOB_INIT_TIME"
else
  echo "'datadog-ci' not installed, no job info sent to Datadog"
fi
