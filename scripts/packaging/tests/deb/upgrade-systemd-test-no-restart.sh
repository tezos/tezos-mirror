#!/bin/bash

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
REPOOLD="https://packages.nomadic-labs.com"
DISTRO=$1
RELEASE=$2

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y sudo gpg curl apt-utils debconf-utils procps jq

# [preseed debconf]
echo "debconf debconf/frontend select Noninteractive" | sudo debconf-set-selections

# [add current repository]
sudo curl "$REPOOLD/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPOOLD/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-current.list
apt-get update

# [ preeseed octez ]
# preseed octez-node for debconf. Notice we set purge_warning to yes,
# to make the `autopurge` pass and remove all the node data at the end of this
# script.
cat << EOF > preseed.cfg
octez-node octez-node/configure boolean true
octez-node octez-node/history-mode string rolling
octez-node octez-node/network string ghostnet
octez-node octez-node/purge_warning boolean true
octez-node octez-node/snapshot-import boolean false
octez-node octez-node/snapshot-no-check boolean true
octez-baker octez-baker/liquidity-vote select on
debconf debconf/frontend select Noninteractive
EOF
# preseed the package
sudo debconf-set-selections preseed.cfg
# check the package configuration
sudo debconf-get-selections | grep octez

# [install octez]
apt-get install -y octez-baker

sudo systemctl stop octez-node
sudo systemctl stop octez-dal-node
sudo systemctl stop octez-baker

# [add next repository]
sudo curl "$REPO/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez-dev.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez-dev.gpg] $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
apt-get update

# [upgrade octez]
sudo rm -f /usr/sbin/policy-rc.d
apt-get upgrade -o Dpkg::Options::="--force-confdef" -y octez-baker

systemctl is-active --quiet octez-node && {
  echo "octez-node is active when it shouldn't be"
  exit 1
}
systemctl is-active --quiet octez-baker && {
  echo "octez-baker is active when it shouldn't be"
  exit 1
}
systemctl is-active --quiet octez-dal-node && {
  echo "octez-dal-node is active when it shouldn't be"
  exit 1
}

exit 0
