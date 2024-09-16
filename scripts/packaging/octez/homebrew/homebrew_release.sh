#!/bin/sh
#

# This script assume that homebrew is correctly installed
# using the script ./scripts/packaging/homebrew_install.sh
# and creates a formula scripts/packaging/Formula/octez.rb
# that is ready to be installed with brew

set -ue

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

# set version

. scripts/version.sh
. scripts/ci/octez-release.sh

# fetch tags for releases
git fetch -q --tags

if [ -n "${gitlab_release_no_v:-}" ]; then
  VERSION=$gitlab_release_no_v
elif [ -n "${CI_COMMIT_TAG:-}" ]; then
  VERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_TAG
else
  VERSION=$(date +'%Y%m%d%H%M')+$CI_COMMIT_SHORT_SHA
fi

# prepare target dir

if [ -n "${gitlab_release_no_v:-}" ]; then
  # if it's a release tag, then it can be a RC release or a final release
  if [ -n "${gitlab_release_rc_version}" ]; then
    # Release candidate
    TARGETDIR="public/homebrew/RC/Formula"
  else
    # Release
    TARGETDIR="public/homebrew/Formula"
  fi
else
  if [ "$CI_COMMIT_REF_PROTECTED" = "false" ]; then
    if [ "$CI_COMMIT_REF_NAME" = "RC" ]; then
      echo "Cannot create a repository for a branch named 'RC'"
      exit 1
    else
      # Branch is not protected, this is for testing ordinary MRs
      TARGETDIR="public/homebrew/$CI_COMMIT_REF_NAME/Formula"
    fi
  else
    # For protected branches that are not release, we allow
    # a repository only for master.
    if [ "$CI_COMMIT_REF_NAME" = "master" ]; then
      TARGETDIR="public/homebrew/master/Formula"
    else
      if [ -n "${CI_COMMIT_TAG:-}" ]; then
        TARGETDIR="public/homebrew/${CI_COMMIT_TAG}/Formula"
      else
        echo "Cannot create a repository for a protected branch that is not associated to a tag or master"
        exit 1
      fi
    fi
  fi
fi

# prepare formula

mkdir -p "$TARGETDIR"

#shellcheck disable=SC2046
eval $(scripts/active_protocols.sh)
sed "s|%%VERSION%%|$VERSION|; \
 s|%%CI_MERGE_REQUEST_SOURCE_PROJECT_URL%%|$CI_MERGE_REQUEST_SOURCE_PROJECT_URL|; \
 s|%%CI_COMMIT_REF_NAME%%|$CI_COMMIT_REF_NAME|; \
 s|%%CI_PROJECT_NAMESPACE%%|$CI_PROJECT_NAMESPACE|; \
 s|%%PROTO_CURRENT%%|$PROTO_CURRENT|; s|%%PROTO_NEXT%%|$PROTO_NEXT|" \
  scripts/packaging/octez/homebrew/Formula/octez.rb.template > "$TARGETDIR/octez.rb"

# upload to bucket

if [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then
  echo "### Logging into protected repo ..."
  echo "${GCP_PROTECTED_SERVICE_ACCOUNT}" | base64 -d > protected_sa.json
  gcloud auth activate-service-account --key-file=protected_sa.json
else
  echo "### Logging into standard repo ..."
  # Nothing to do
fi

GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"

echo "https://$BUCKET.storage.googleapis.com/${TARGETDIR##public/}/octez.rb"
