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
apt-get install -y sudo gpg curl debconf-utils apt-utils
# [add repository]
REPO="deb https://$bucket.storage.googleapis.com/$distribution $release main"
curl "https://$bucket.storage.googleapis.com/$distribution/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg
echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
sudo apt-get update

# [ preeseed octez ]
if [ -n "$PREFIX" ]; then
  # preseed octez-node for debconf. Notice we set purge_warning to yes,
  # to make the `autopurge` pass and remove all the node data at the end of this
  # script.
  cat << EOF > preseed.cfg
octez-node octez-node/configure string yes
octez-node octez-node/history-mode string full
octez-node octez-node/network string mainnet
octez-node octez-node/purge_warning string yes
debconf debconf/frontend select Noninteractive
EOF
  # preseed the package
  sudo debconf-set-selections preseed.cfg

  # check the package configuration
  sudo debconf-get-selections | grep octez
fi

# [install tezos]
sudo apt-get install -y octez-client
sudo apt-get install -y octez-node
sudo apt-get install -y octez-baker
sudo apt-get install -y octez-dal-node

# [install octez additional packages]
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

# [test autopurge]
sudo apt-get autopurge -y octez-node octez-client octez-baker octez-dal-node

# [check autopurge]
set +x

if [ -n "$PREFIX" ]; then
  # check the package configuration
  sudo debconf-get-selections | if grep -q octez; then
    echo "Leftovers in debconf db"
    sudo debconf-get-selections | grep -q octez
    exit 1
  fi

  printf "Check if the user tezos was removed:"
  if id tezos > /dev/null 2>&1; then
    echo "Tezos user not correctly removed"
    id tezos
    exit 1
  else
    echo "Ok."
  fi

  printf "Check if the datadir was correctly removed:"
  if [ -e /var/tezos ]; then
    echo "Datadir /var/tezos not correctly removed"
    ls -la /var/tezos
  else
    echo "Ok."
  fi
fi
