#!/bin/sh

# TODO: https://github.com/mozilla/sccache/issues/2076
# Once the above-described race-condition has been resolved, we can
# upgrade sccache and do away with this wrapper.

# Usage: . ./scripts/ci/sccache-start.sh ATTEMPTS
#
# sccache has a race-condition such that it occassionally fails on
# start up. This script works around the issue by attempting to start
# sccache ATTEMPTS times. If it has not succeeded after ATTEMPTS, it
# will export RUSTC_WRAPPER="", inhibiting sccache.

attempts=${1:-4}

while [ "${attempts}" -gt 0 ]; do
  if sccache --start-server; then
    break
  else
    attempts=$((attempts - 1))
  fi
done

if [ "${attempts}" = 0 ]; then
  export RUSTC_WRAPPER=""
fi
