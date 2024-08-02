#!/bin/sh

distribution=$1
release=$2

# check if it's a real or a fake release or we are testing
# the packages in a branch
if [ -n "${CI_COMMIT_TAG:-}" ]; then
  # shellcheck source=./scripts/ci/octez-release.sh
  . ./scripts/ci/octez-release.sh
fi

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
bucket="$GCP_LINUX_PACKAGES_BUCKET"
protocol=$(head -1 script-inputs/active_protocol_versions_without_number)

# This logic must be kept in sync with the script in
# ./scripts/ci/create_debian_repo.sh

# The prefix used for these packages in the repository. E.g. 'next'
if [ -n "$PREFIX" ]; then
  PREFIX=${PREFIX}/
else
  PREFIX=
fi

# if it's a release tag, then it can be a RC release
# or a final release. This can be on a protected branch or not.
if [ -n "${gitlab_release_no_v:-}" ]; then
  # It a release tag, this can be either final or release
  # candidate
  if [ -n "${gitlab_release_rc_version}" ]; then
    # Release candidate
    distribution="${PREFIX}RC/$distribution"
  fi
  # else we just that $distribution as it is
else
  # Not a release tag. This is strictly for testing.
  if [ "${CI_COMMIT_REF_PROTECTED:-false}" = true ]; then
    # this is not a release, but it's a protected branch.
    # We allow this only for the master branch.
    if [ "$CI_COMMIT_REF_NAME" = "master" ]; then
      distribution="${PREFIX}master/$distribution"
    else
      echo "Cannot test for a protected branch that \
        is not associated with a release tag or it's master"
      exit 1
    fi
  else
    # Not a release, not a protected branch
    if [ "$CI_COMMIT_REF_NAME" = "RC" ]; then
      echo "Cannot test a repository for a branch named 'RC'"
      exit 1
    else
      distribution="${PREFIX}$CI_COMMIT_REF_NAME/$distribution"
    fi
  fi
fi

set -e
set -x

# [install prerequisites]
apt-get update
apt-get install -y sudo gpg curl
# [add repository]
REPO="deb https://$bucket.storage.googleapis.com/$distribution $release main"
sudo curl "https://$bucket.storage.googleapis.com/$distribution/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
sudo apt-get update

# [install tezos]
sudo apt-get install -y octez-client
sudo apt-get install -y octez-node
sudo apt-get install -y octez-baker
sudo apt-get install -y octez-dal-node

if [ -n "$PREFIX" ]; then
  # [install octez NEXT packages]
  sudo apt-get install -y octez-smart-rollup-node
else
  # [install octez current packages]
  sudo apt-get install -y octez-smartrollup
  sudo apt-get install -y octez-evmnode
fi

# [test executables]
octez-client --version
octez-node --version
"octez-baker-$protocol" --version
"octez-accuser-$protocol" --version

# [test autoremove]
sudo apt autoremove -y octez-node octez-client octez-baker octez-dal-node
