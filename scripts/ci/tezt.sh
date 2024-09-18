#!/bin/sh

# Run Tezt for the CI.

if [ ! -f selected_tezts.tsl ]; then
  echo "Cannot find the artifact of select_tezts: selected_tezts.tsl"
  # We cannot just run all tests because we don't know if the artifact is
  # available in other Tezt jobs and the auto-balancing of Tezt requires
  # the test selection to be exactly the same for all jobs.
  # Otherwise we risk skipping some tests by mistake.
  exit 1
fi

_build/default/tezt/tests/main.exe "$(cat selected_tezts.tsl)" "$@"
TEZT_EXIT_CODE="$?"
echo "Tezt exited with code: $TEZT_EXIT_CODE"

if [ $TEZT_EXIT_CODE -eq 3 ]; then
  echo "Exit code 3 from Tezt means: no test found for filters."
  echo "With Manifezt, this can happen, for instance for MRs that do not change any test dependency or if the number of selected tests is less than the number of Tezt jobs."
  exit 0
fi

if [ -n "$DATADOG_API_KEY" ]; then
  if [ -f "$JUNIT" ]; then
    echo "Uploading JUNIT=$JUNIT to Datadog..."
    DD_ENV=ci DATADOG_SITE=datadoghq.eu datadog-ci junit upload --service octez "$JUNIT"
    echo "datadog-cli exit code: $?"
  else
    echo "JUNIT=$JUNIT does not exist, will not try to send it to Datadog."
  fi
else
  echo "DATADOG_API_KEY is not set, will not try to send JUNIT=$JUNIT."
fi

exit "$TEZT_EXIT_CODE"
