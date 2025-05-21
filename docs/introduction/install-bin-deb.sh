#!/bin/sh

distribution=$1
release=$2

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
bucket="$GCP_LINUX_PACKAGES_BUCKET"
protocol=$(head -1 script-inputs/active_protocol_versions_without_number)

# This logic must be kept in sync with the script in
# ./scripts/ci/create_debian_repo.sh

# The prefix used for these packages in the repository. E.g. 'old'
if [ -n "$PREFIX" ]; then
  PREFIX=${PREFIX}/
else
  PREFIX=
fi

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

if [ -n "$CI" ]; then
  . scripts/ci/octez-packages-version.sh
fi

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  distribution="${PREFIX}RC/${distribution}"
  ;;
Release | TestRelease)
  # use $distribution as it is
  : nop
  ;;
Master)
  distribution="${PREFIX}master/${distribution}"
  ;;
SoftRelease)
  distribution="${PREFIX}${CI_COMMIT_TAG}/${distribution}"
  ;;
TestBranch)
  distribution="${PREFIX}${CI_COMMIT_REF_NAME}/${distribution}"
  ;;
*)
  echo "Cannot test packages on this branch"
  exit 1
  ;;
esac

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

set -e
set -x

# these two packages are needed here, but they don't need to appear in the doc
apt-get update
apt-get install -y debconf-utils apt-utils procps

if [ "$RELEASETYPE" = "Master" ]; then
  apt-get update
  apt-get install -y sudo gpg curl
  # [add repository]

  curl -s "https://packages.nomadic-labs.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  echo "deb [signed-by=/etc/apt/keyrings/octez.gpg] https://packages.nomadic-labs.com/$distribution $release main" |
    sudo tee /etc/apt/sources.list.d/octez.list
  apt-get update
  # [end add repository]
else
  apt-get install -y sudo gpg curl
  curl -s "https://$bucket.storage.googleapis.com/$distribution/octez.asc" |
    sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
  REPO="deb [signed-by=/etc/apt/keyrings/octez.gpg] https://$bucket.storage.googleapis.com/$distribution $release main"
  echo "$REPO" | sudo tee /etc/apt/sources.list.d/octez.list
  apt-get update
fi

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
apt-get install -y octez-client
apt-get install -y octez-node

apt-get install -y octez-baker
apt-get install -y octez-dal-node

# [install octez additional packages]
apt-get install -y octez-smart-rollup-node

# [test executables]
octez-client --version
octez-node --version
"octez-baker-$protocol" --version
"octez-accuser-$protocol" --version

# [test autopurge]
apt-get autopurge -y octez-node octez-client octez-baker octez-dal-node

# [check autopurge]

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
