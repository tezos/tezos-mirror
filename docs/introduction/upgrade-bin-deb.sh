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
apt-get install -y sudo gpg curl apt-utils debconf-utils

# [preseed debconf]
echo "debconf debconf/frontend select Noninteractive" | sudo debconf-set-selections

# [add current repository]
sudo curl "https://$bucket.storage.googleapis.com/$distribution/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg

REPO="deb https://$bucket.storage.googleapis.com/$distribution $release main"
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez-current.list
sudo apt-get update

# [install tezos]
sudo apt-get install -y octez-client
sudo apt-get install -y octez-node
sudo apt-get install -y octez-baker
dpkg -l octez-\*

# [setup Octez node]
sudo su tezos -c "octez-node config init --network=mainnet --history-mode=rolling --net-addr=\"[::]:9732\" --rpc-addr=\"127.0.0.1:8732\""
sudo systemctl enable octez-node
sudo systemctl enable octez-baker

# [setup baker]
PROTOCOL=$(octez-client --protocol PtParisBxoLz list understood protocols | tee | head -1)
sudo su tezos -c "octez-client -p $PROTOCOL gen keys baker"
BAKER_KEY=$(sudo su tezos -c "octez-client -p $PROTOCOL show address baker" | head -1 | awk '{print $2}')
echo "baking_key=$BAKER_KEY" >> /etc/octez/baker.conf
echo "lq_vote=yes" >> /etc/octez/baker.conf

# [add next repository]
REPO="deb https://$bucket.storage.googleapis.com/next/$distribution $release main"
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez-next.list
sudo apt-get update

# [upgrade octez]
# --force-overwrite is necessary because legacy package shipped the zcash
# parameters as part of the octez-node package.
sudo apt-get upgrade -y -o DPkg::options::="--force-overwrite" octez-node
sudo apt-get upgrade -y octez-client octez-baker

# [ check configuration after the upgrade ]
# we check the debconf parameters
sudo debconf-get-selections | grep octez
# we check if the configuration of octez did not change
sudo su tezos -c "octez-client -p $PROTOCOL show address baker"
sudo su tezos -c "octez-node config show"

# [check executables version]
dpkg -l octez-\*
