#!/bin/sh

SCRIPT_STEP_END=$(date +%s)
# [SCRIPT_STEP_BEGIN] defined as SCRIPT_STEP_BEGIN=$(date +%s) at
# beginning of [before_script]
SCRIPT_STEP_TIME=$((SCRIPT_STEP_END - SCRIPT_STEP_BEGIN))
echo "SCRIPT_STEP_TIME=$SCRIPT_STEP_TIME"

if command -v datadog-ci > /dev/null 2>&1; then
  echo "Sending job-level info to Datadog"
  DATADOG_SITE=datadoghq.eu datadog-ci measure --level job --measures script_step_time:"$SCRIPT_STEP_TIME"
else
  echo "'datadog-ci' not installed, no job info sent to Datadog"
fi
