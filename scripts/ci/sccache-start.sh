#!/bin/sh

set -eu

# TODO: https://github.com/mozilla/sccache/issues/2076
# Once the above-described race-condition has been resolved, we can
# upgrade sccache and do away with this wrapper.

# Usage: . ./scripts/ci/sccache-start.sh ATTEMPTS
#
# sccache has a race-condition such that it occassionally fails on
# start up. This script works around the issue by attempting to start
# sccache ATTEMPTS times. If it has not succeeded after ATTEMPTS, it
# will export RUSTC_WRAPPER="", inhibiting sccache.

if ! command -v sccache > /dev/null; then
  echo "Could not find sccache in path."
  exit 1
fi

# On protected refs GCP_SCCACHE_BUCKET is the protected bucket, which only
# protected-registry@ may read and write. The sccache server reads its GCS
# credentials once, when it starts, so the key must be in place BEFORE the
# start loop below. Without it the server runs as the GKE node service
# account, which has no access to that bucket.
# The key file is removed by sccache-stop.sh, after the server is stopped.
if [ "${CI_COMMIT_REF_PROTECTED:-}" = "true" ]; then
  echo "### Authenticating to protected GCS bucket..."
  SCCACHE_GCS_KEY_PATH="${TMPDIR:-/tmp}/sccache_protected_sa.json"
  umask 077
  echo "${GCP_PROTECTED_SERVICE_ACCOUNT}" | base64 -d > "$SCCACHE_GCS_KEY_PATH"
  umask 022
  export SCCACHE_GCS_KEY_PATH
  gcloud auth activate-service-account --key-file="$SCCACHE_GCS_KEY_PATH"
fi

max_attempts=${1:-4}
attempts="$max_attempts"

while [ "${attempts}" -gt 0 ]; do
  if sccache --start-server; then
    export RUSTC_WRAPPER="sccache"
    break
  else
    attempts=$((attempts - 1))
  fi
done

if [ "${attempts}" = 0 ]; then
  echo "Could not start sccache after ${max_attempts}, running without sccache."
  export RUSTC_WRAPPER=""
fi

echo "GCS sccache bucket: ${GCP_SCCACHE_BUCKET:-unset}"
