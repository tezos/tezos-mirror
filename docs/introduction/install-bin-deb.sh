#!/bin/sh

distribution=$1
release=$2

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

. scripts/ci/octez-packages-version.sh

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
