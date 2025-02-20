#!/bin/sh

set -eu
set -x

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
REPOOLD="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/old/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y sudo gpg curl apt-utils debconf-utils procps jq

# [preseed debconf]
echo "debconf debconf/frontend select Noninteractive" | sudo debconf-set-selections

# [add current repository]
sudo curl "$REPOOLD/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg

reposityory="deb $REPOOLD/$DISTRO $RELEASE main"
echo "$reposityory" | sudo tee /etc/apt/sources.list.d/octez-current.list
sudo apt-get update

# [install octez]
sudo apt-get install -y octez-client
sudo apt-get install -y octez-node
sudo apt-get install -y octez-baker
dpkg -l octez-\*

# [setup Octez node]
sudo su tezos -c "octez-node config init --data-dir=/var/tezos/.tezos-node --network=ghostnet --history-mode=rolling --net-addr=\"[::]:9732\" --rpc-addr=\"127.0.0.1:8732\""

curl https://snapshots.tzinit.org/ghostnet/rolling -o /tmp/rolling
sudo su tezos -c "octez-node snapshot import --data-dir=/var/tezos/.tezos-node --no-check /tmp/rolling"

sudo /etc/init.d/octez-node start

#shellcheck disable=SC2009
ps aux | grep octez

# [setup baker]
PROTOCOL=$(octez-client --protocol PtParisBxoLz list understood protocols | tee | head -1)
sudo su tezos -c "octez-client -p $PROTOCOL gen keys baker"
BAKER_KEY=$(sudo su tezos -c "octez-client -p $PROTOCOL show address baker" | head -1 | awk '{print $2}')
echo "baking_key=$BAKER_KEY" >> /etc/octez/baker.conf
echo "lq_vote=yes" >> /etc/octez/baker.conf

# ideally we should also start the baker, but it will timeout
# waiting for the node to sync
#sudo /etc/init.d/octez-baker start

#shellcheck disable=SC2009
ps aux | grep baker

sudo su tezos -c "octez-node config show"

# [add next repository]
repository="deb $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
sudo apt-get update

# [upgrade octez]
# --force-overwrite is necessary because legacy package shipped the zcash
# parameters as part of the octez-node package.
sudo apt-get upgrade -y -o DPkg::options::="--force-overwrite" octez-baker

sudo systemctl enable octez-node
sudo systemctl enable octez-baker-active

cat /etc/default/octez-node
cat /etc/default/octez-baker-active

sudo systemctl restart octez-node.service
sudo systemctl status octez-node.service

sudo systemctl restart octez-baker-active.service
sudo systemctl status octez-baker-active.service

# [ check configuration after the upgrade ]
# we check the debconf parameters
sudo debconf-get-selections | grep octez
# we check if the configuration of octez did not change
sudo su tezos -c "octez-client -p $PROTOCOL show address baker"
sudo su tezos -c "octez-node config show"

# [check executables version]
dpkg -l octez-\*
