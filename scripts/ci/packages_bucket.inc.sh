#!/bin/sh

# Export PACKAGES_BUCKET, the GCS bucket holding the packages and Homebrew
# formulae of this ref. Source from the repository root.
#
# Protected refs (master, release branches, tags) use
# GCP_LINUX_PACKAGES_BUCKET_PROTECTED, other refs GCP_LINUX_PACKAGES_BUCKET_UNPROTECTED.
# Both are plain CI/CD variables with one value per clone of the project and no
# default here: an unset variable fails the job. Selecting the protected bucket
# from an unprotected ref is harmless, only the protected service account
# (scripts/ci/gcp_auth.sh) can write to it.

if [ -n "${PACKAGES_BUCKET+x}" ]; then
  echo "error: PACKAGES_BUCKET is already set; it is derived from the ref by $0, do not set it" >&2
  exit 1
fi

if [ "${CI_COMMIT_REF_PROTECTED:-false}" = "true" ]; then
  PACKAGES_BUCKET="${GCP_LINUX_PACKAGES_BUCKET_PROTECTED:?must be set: bucket of protected refs}"
else
  PACKAGES_BUCKET="${GCP_LINUX_PACKAGES_BUCKET_UNPROTECTED:?must be set: bucket of unprotected refs}"
fi
export PACKAGES_BUCKET
