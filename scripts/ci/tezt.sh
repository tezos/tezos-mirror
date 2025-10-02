#!/bin/sh

# Run Tezt for the CI.

SEND_JUNIT=no
WITH_SELECT_TEZTS=unknown

usage() {
  cat << EOF
Usage: tezt.sh SCRIPT_ARGUMENTS -- TEZT_ARGUMENTS

SCRIPT_ARGUMENTS

    --send-junit
        If specified, JUnit files are sent to Datadog.

    --with-select-tezts, --without-select-tezts (must specify one or the other)
        Whether selected_tezts.tsl is expected to exist or not.
        If it exists, its content is passed to Tezt.
        It is supposed to be the output of the select_tezts job.

TEZT_ARGUMENTS

The separator -- signals that remaining arguments are to be passed directly to Tezt.
This separator is mandatory.
EOF
}

while true; do
  case "$1" in
  "--send-junit")
    shift
    SEND_JUNIT=yes
    ;;
  "--with-select-tezts")
    shift
    if [ "$WITH_SELECT_TEZTS" = no ]; then
      usage
      echo "Error: cannot specify both --without-select-tezts and --with-select-tezts"
      exit 1
    fi
    WITH_SELECT_TEZTS=yes
    ;;
  "--without-select-tezts")
    shift
    if [ "$WITH_SELECT_TEZTS" = yes ]; then
      usage
      echo "Error: cannot specify both --without-select-tezts and --with-select-tezts"
      exit 1
    fi
    WITH_SELECT_TEZTS=no
    ;;
  "--")
    shift
    break
    ;;
  *)
    usage
    if [ "$1" = "" ]; then
      echo "Error: expected --"
    else
      echo "Error: invalid argument: $1"
    fi
    exit 1
    ;;
  esac
done

if [ "$WITH_SELECT_TEZTS" = unknown ]; then
  usage
  echo "Error: must specify one of --without-select-tezts or --with-select-tezts"
  exit 1
fi

if [ "$WITH_SELECT_TEZTS" = yes ]; then
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
else
  echo "Test selection is disabled."
  _build/default/tezt/tests/main.exe "$@"
  TEZT_EXIT_CODE="$?"
fi

if [ $TEZT_EXIT_CODE -eq 3 ]; then
  echo "Exit code 3 from Tezt means: no test found for filters."
  echo "With Manifezt, this can happen, for instance for MRs that do not change any test dependency or if the number of selected tests is less than the number of Tezt jobs."
  exit 0
fi

if [ "$SEND_JUNIT" = "yes" ]; then
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
fi

exit "$TEZT_EXIT_CODE"
