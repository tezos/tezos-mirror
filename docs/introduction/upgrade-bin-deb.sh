#!/bin/sh

distribution=$1
distroname=$1
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

# This logic must be kept in sync with the script in
# ./scripts/ci/create_debian_repo.sh

# if it's a release tag, then it can be a release candidate
# or a final release. This can be on a protected branch or not.
if [ -n "${gitlab_release_no_v:-}" ]; then
  # It a release tag, this can be either final or release
  # candidate
  if [ -n "${gitlab_release_rc_version}" ]; then
    # Release candidate
    distribution="RC/$distribution"
  fi
  # else we just that $distribution as it is
else
  # Not a release tag. This is strictly for testing.
  if [ "${CI_COMMIT_REF_PROTECTED:-false}" = true ]; then
    # this is not a release, but it's a protected branch.
    # We allow this only for the master branch.
    if [ "$CI_COMMIT_REF_NAME" = "master" ]; then
      distribution="master/$distribution"
    else
      if [ -n "${CI_COMMIT_TAG}" ]; then
        distribution="${CI_COMMIT_TAG}/$distribution"
      else
        echo "Cannot test a repository for a protected branch that is not associated to a tag or master"
        exit 1
      fi
    fi
  else
    # Not a release, not a protected branch
    if [ "$CI_COMMIT_REF_NAME" = "RC" ]; then
      echo "Cannot test a repository for a branch named 'RC'"
      exit 1
    else
      distribution="$CI_COMMIT_REF_NAME/$distribution"
    fi
  fi
fi

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

set -e
set -x

# [install prerequisites]
apt-get update
apt-get install -y sudo gpg curl apt-utils debconf-utils jq

# [preseed debconf]
echo "debconf debconf/frontend select Noninteractive" | sudo debconf-set-selections

# [add current repository]
curl "https://packages.nomadic-labs.com/$distroname/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg

REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] https://packages.nomadic-labs.com/$distroname $release main"
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez-current.list
sudo apt-get update

# [ preeseed octez ]
# preseed octez-node for debconf. Notice we set purge_warning to yes,
# to make the `autopurge` pass and remove all the node data at the end of this
# script.
cat << EOF > preseed.cfg
octez-node octez-node/configure boolean true
octez-node octez-node/history-mode string full
octez-node octez-node/network string mainnet
octez-node octez-node/purge_warning boolean true
octez-node octez-node/snapshot-import boolean false
octez-node octez-node/snapshot-no-check boolean true
debconf debconf/frontend select Noninteractive
EOF
# preseed the package
sudo debconf-set-selections preseed.cfg

# check the package configuration
sudo debconf-get-selections | grep octez

# [install tezos]
sudo apt-get install -y octez-baker

# [add next repository]
REPO="deb [signed-by=/etc/apt/keyrings/octez-dev.gpg] https://$bucket.storage.googleapis.com/$distribution $release main"
curl "https://$bucket.storage.googleapis.com/$distroname/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez-dev.gpg
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez-next.list
sudo apt-get update

# [upgrade octez]
sudo apt-get upgrade -y octez-node
sudo apt-get upgrade -y octez-client octez-baker

# [ check configuration after the upgrade ]
# we check the debconf parameters
sudo debconf-get-selections | grep octez
# we check if the configuration of octez did not change
sudo su tezos -c "octez-node config show"

# [check executables version]
dpkg -l octez-\*
