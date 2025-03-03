#!/bin/sh

set -eu
set -x

REPO="https://storage.googleapis.com/${GCP_LINUX_PACKAGES_BUCKET:-tezos-linux-repo}/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2
DATADIR=${3:-}

# For the upgrade script in the CI, we do not want debconf to ask questions
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y sudo gpg curl apt-utils debconf-utils procps jq

sudo curl "$REPO/$DISTRO/octez.asc" | sudo gpg --dearmor -o /etc/apt/trusted.gpg.d/octez.gpg

# [add next repository]
repository="deb $REPO/$DISTRO $RELEASE main"
echo "$repository" | sudo tee /etc/apt/sources.list.d/octez-next.list
sudo apt-get update

# [ preeseed octez ]
if [ -z "$PREFIX" ]; then
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
fi

sudo apt-get install -y octez-baker

if [ -n "$DATADIR" ]; then
  echo "Setup Custom data dir"
  usermod -m -d /custom tezos
  if [ -e /var/tezos/.tezos-node ]; then
    cp -a /var/tezos/.tezos-node /custom/
  fi
  echo "DATADIR=/custom/.tezos-node" >> /etc/default/octez-node
fi

sudo systemctl start octez-node.service
sudo systemctl status octez-node.service

# give some time to the node to create the identity
# otherwise the octez-client call below will give an error
/usr/share/octez-baker/wait-for-node-up.sh

sudo su tezos -c "octez-client gen keys alice"
key=$(sudo su tezos -c "octez-client show address alice" | grep Hash: | awk '{ print $2 }')
echo "BAKER_KEY=$key" >> /etc/default/octez-baker

sudo systemctl enable octez-baker
sudo systemctl start octez-baker.service

sudo systemctl status octez-baker.service

sudo systemctl status octez-baker.service

sudo su tezos -c "octez-node config show"

echo "-----------------------"
cat /etc/default/octez-node

echo "-----------------------"
cat /etc/default/octez-baker

echo "-----------------------"
tail /var/log/tezos/node.log

echo "-----------------------"
for logfile in /var/log/tezos/baker-P*.log; do
  tail "$logfile"
done
