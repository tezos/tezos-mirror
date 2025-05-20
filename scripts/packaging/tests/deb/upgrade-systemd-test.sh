#!/bin/sh

set -eu
set -x

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

reposityory="deb [signed-by=/etc/apt/keyrings/octez.gpg] $REPOOLD/$DISTRO $RELEASE main"
echo "$reposityory" | sudo tee /etc/apt/sources.list.d/octez-current.list
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
octez-node octez-node/snapshot-import boolean true
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

systemctl start octez-node

#shellcheck disable=SC2009
ps aux | grep octez

# [setup baker]
PROTOCOL=$(octez-client --protocol PtParisBxoLz list understood protocols | tee | head -1)
sudo su tezos -c "octez-client -p $PROTOCOL gen keys baker"
BAKER_KEY=$(sudo su tezos -c "octez-client -p $PROTOCOL show address baker" | head -1 | awk '{print $2}')
echo "BAKER_KEY=$BAKER_KEY" >> /etc/default/octez-baker

# ideally we should also start the baker, but it will timeout
# waiting for the node to sync
systemctl start octez-baker

#shellcheck disable=SC2009
ps aux | grep baker

sudo su tezos -c "octez-node config show"

# [add next repository]
sudo curl "$REPO/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/keyrings/octez-dev.gpg
repository="deb [signed-by=/etc/apt/keyrings/octez-dev.gpg] $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
apt-get update

# [upgrade octez]
apt-get upgrade -y octez-baker

cat /etc/default/octez-node
cat /etc/default/octez-baker

systemctl restart octez-node.service
systemctl status octez-node.service

systemctl enable octez-baker
systemctl restart octez-baker.service

systemctl status octez-baker.service

systemctl status octez-baker.service

ERR=0

# [ check configuration after the upgrade ]
# we check the debconf parameters

#shellcheck disable=SC1091
. /etc/default/octez-baker

# we check if the configuration of octez did not change
BAKER_KEY_AFTER=$(sudo su tezos -c "octez-client -p $PROTOCOL show address baker" | head -1 | awk '{print $2}')
if [ "$BAKER_KEY" != "$BAKER_KEY_AFTER" ]; then
  echo "Client key differ $BAKER_KEY <> $BAKER_KEY_AFTER"
  ERR=1
fi

BAKER_KEY_DEBCONF=$(sudo debconf-get-selections | grep octez-baker/baker-key | awk '{print $4}')
if [ "$BAKER_KEY_DEBCONF" != "$BAKER_KEY_AFTER" ]; then
  echo "Debconf baker key differ $BAKER_KEY <> $BAKER_KEY_AFTER"
  ERR=1
else
  echo "Debconf baker key differ migrated successfully $BAKER_KEY"
fi

LQVOTE_DEBCONF=$(sudo debconf-get-selections | grep octez-baker/liquidity-vote | awk '{print $4}')
if [ "$LQVOTE_DEBCONF" != "on" ]; then
  echo "Debconf liquidity vote differ $LQVOTE_DEBCONF <> on"
  ERR=1
else
  echo "Debconf liquidity vote migrated successfully \"$LQVOTE_DEBCONF\""
fi

if [ "$LQVOTE" != "on" ]; then
  echo "Liquidity vote differ $LQVOTE <> on"
  ERR=1
else
  echo "Liquidity vote migrated successfully \"$LQVOTE\""
fi

NETWORK_AFTER=$(sudo su tezos -c "octez-node config show" | jq -r .network)
if [ "$NETWORK_AFTER" != "ghostnet" ]; then
  echo "Node network differ $NETWORK_AFTER <> ghostnet"
  ERR=1
else
  echo "Node network migrated successfully $NETWORK_AFTER"
fi

HISTORY_AFTER=$(sudo su tezos -c "octez-node config show" | jq -r .shell.history_mode)
if [ "$HISTORY_AFTER" != "rolling" ]; then
  echo "Node history mode differ $HISTORY_AFTER <> rolling"
  ERR=1
else
  echo "Node history mode migrated successfully $HISTORY_AFTER"
fi

# [check executables version]
dpkg -l octez-\*

exit "$ERR"
