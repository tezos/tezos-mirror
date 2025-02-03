#!/bin/sh

if command -v datadog-ci > /dev/null 2>&1; then
  echo "Sending job-level info to Datadog"
  DATADOG_SITE=datadoghq.eu datadog-ci tag --level job --tags pipeline_type:"$PIPELINE_TYPE"
else
  echo "'datadog-ci' not installed, no job info sent to Datadog"
fi
