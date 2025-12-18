#!/bin/sh
#

# This script assumes that homebrew is correctly installed
# using the script ./scripts/packaging/homebrew_install.sh
# and creates a formula scripts/packaging/octez/homebrew/Formula/octez.rb
# that is ready to be installed with brew

set -ue

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

# fetch tags for releases
git fetch -q --tags

if [ -z "${CI:-}" ]; then
  TIMESTAMP=$(date '+%Y%m%d%H%M')
  CI_COMMIT_SHORT_SHA=$(git rev-parse --short HEAD)
  CI_COMMIT_REF_NAME=$(git rev-parse --abbrev-ref HEAD)
  CI_COMMIT_TAG=$(git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD)
else
  TIMESTAMP="$(date -d "$CI_PIPELINE_CREATED_AT" '+%Y%m%d%H%M')"
fi

# set $VERSION ( for release* branches ) and $RELEASETYPE
. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  TARGETDIR="public/homebrew/RC/Formula"
  ;;
Beta | TestBeta)
  TARGETDIR="public/homebrew/BETA/Formula"
  ;;
Release | TestRelease)
  TARGETDIR="public/homebrew/Formula"
  ;;
Master)
  VERSION="$TIMESTAMP+$CI_COMMIT_SHORT_SHA"
  TARGETDIR="public/homebrew/master/Formula"
  ;;
SoftRelease)
  VERSION="$TIMESTAMP+${CI_COMMIT_TAG:-}"
  TARGETDIR="public/homebrew/${CI_COMMIT_TAG}/Formula"
  ;;
TestBranch)
  VERSION="$TIMESTAMP+$CI_COMMIT_SHORT_SHA"
  TARGETDIR="public/homebrew/$CI_COMMIT_REF_NAME/Formula"
  ;;
*)
  echo "Cannot create a repository for this branch"
  exit 1
  ;;
esac

# prepare formula
echo "Preparing homebrew formula $VERSION -> $TARGETDIR"

mkdir -p "$TARGETDIR"

#shellcheck disable=SC2046
eval $(scripts/active_protocols.sh)
sed "s|%%VERSION%%|$VERSION|; \
 s|%%CI_MERGE_REQUEST_SOURCE_PROJECT_URL%%|${CI_MERGE_REQUEST_SOURCE_PROJECT_URL:-$CI_PROJECT_URL}|; \
 s|%%CI_COMMIT_REF_NAME%%|${CI_COMMIT_REF_NAME:-master}|; \
 s|%%CI_PROJECT_NAMESPACE%%|$CI_PROJECT_NAMESPACE|; \
 s|%%PROTO_CURRENT%%|$PROTO_CURRENT|; s|%%PROTO_NEXT%%|$PROTO_NEXT|" \
  scripts/packaging/octez/homebrew/Formula/octez.rb.template > "$TARGETDIR/octez.rb"

# upload to bucket

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"

echo "https://$BUCKET.storage.googleapis.com/${TARGETDIR##public/}/octez.rb"
