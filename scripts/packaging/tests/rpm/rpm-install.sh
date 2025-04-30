#!/bin/sh
#

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

set -eu

REPO="https://storage.googleapis.com/$GCP_LINUX_PACKAGES_BUCKET/$CI_COMMIT_REF_NAME"
DISTRO=$1
RELEASE=$2

# wait for systemd to be ready
count=0
while [ "$(systemctl is-system-running)" = "offline" ]; do
  count=$((count + 1))
  if [ $count -ge 10 ]; then
    echo "System is not running after 10 iterations."
    exit 1
  fi
  sleep 1
done

# Update and install the config-manager plugin
dnf -y update
dnf -y install dnf-plugins-core

# Add the repository
dnf -y config-manager --add-repo "$REPO/$DISTRO/dists/$RELEASE"
if [ "$DISTRO" = "rockylinux" ]; then
  dnf -y config-manager --set-enabled devel
fi
dnf -y update

# Install public key
rpm --import "$REPO/$DISTRO/octez.asc"

dnf -y install sudo procps util-linux

dnf -y install octez-client octez-node octez-baker

#shellcheck disable=SC2129,SC1091
echo "NETWORK=ghostnet" >> /etc/default/octez-node
echo "HISTORY_MODE=rolling" >> /etc/default/octez-node
echo "SNAPSHOT_NO_CHECK=" >> /etc/default/octez-node
#shellcheck disable=SC1091
. /etc/default/octez-node

echo "RUNTIME_OPTS=\"--keep-alive --dal-node http://127.0.0.1:10732\"" >> /etc/default/octez-baker
echo "LQVOTE=off" >> /etc/default/octez-baker

rm -f "$DATADIR/config.json"
su tezos -c "/usr/bin/octez-node config init \
      --data-dir=$DATADIR \
      --network=$NETWORK \
      --history-mode=$HISTORY_MODE \
      --net-addr=\"[::]:9732\" \
      --rpc-addr=\"127.0.0.1:8732\""

# if systemd is available we test the service scripts
if [ "$(ps --no-headers -o comm 1)" = "systemd" ]; then

  # This file include the systemd tests and diagnistic common
  # to both rpm and deb
  . scripts/packaging/tests/tests-systemd-common.inc.sh

fi
