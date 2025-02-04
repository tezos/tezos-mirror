#!/bin/bash

distroname=$1
release=$2

bucket="$GCP_LINUX_PACKAGES_BUCKET"
protocol=$(head -1 script-inputs/active_protocol_versions_without_number)

. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  distribution="RC/${distroname}"
  ;;
Release | TestRelease)
  : nop
  ;;
Master)
  distribution="master/${distroname}"
  ;;
SoftRelease)
  distribution="${CI_COMMIT_TAG}/${distroname}"
  ;;
TestBranch)
  distribution="${CI_COMMIT_REF_NAME}/${distroname}"
  ;;
*)
  echo "Cannot test packages on this branch"
  exit 1
  ;;
esac

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

set -e
set -x

if [ "$RELEASETYPE" = "Master" ]; then
  # [add repository]
  # Update and install the config-mananger plugin
  dnf -y update
  dnf -y install dnf-plugins-core

  # Add the repository
  dnf -y config-manager --add-repo "https://packages.nomadic-labs.com/next/$distribution/dists/$release/"
  if [ "$distroname" = "rockylinux" ]; then
    dnf -y config-manager --set-enabled devel
  fi
  dnf -y update

  # Install public key
  rpm --import "https://packages.nomadic-labs.com/next/$distribution/octez.asc"
  # [end add repository]
else
  # Update and install the config-mananger plugin
  dnf -y update
  dnf -y install dnf-plugins-core

  # Add the repository
  dnf -y config-manager --add-repo "https://storage.googleapis.com/$bucket/next/$distribution/dists/$release/"

  if [ "$distroname" = "rockylinux" ]; then
    dnf -y config-manager --set-enabled devel
  fi
  dnf -y update

  # Install public key
  rpm --import "https://storage.googleapis.com/$bucket/next/$distribution/octez.asc"

fi

dnf -y install sudo procps

# [install tezos]
sudo dnf -y install octez-node

# if systemd is available we test the service scripts
if [ "$(ps --no-headers -o comm 1)" = "systemd" ]; then
  systemctl enable octez-node
  systemctl start octez-node

  sleep 5
  systemctl status octez-node

  journalctl -xeu octez-node.service

fi

sudo dnf -y install octez-client
sudo dnf -y install octez-node
sudo dnf -y install octez-baker
sudo dnf -y install octez-dal-node
sudo dnf -y install octez-smart-rollup-node

# [test executables]
octez-client --version
octez-node --version
"octez-baker-$protocol" --version
"octez-accuser-$protocol" --version

# If systemd is available we stop the service scripts started above.
if [ "$(ps --no-headers -o comm 1)" = "systemd" ]; then
  systemctl stop octez-node
fi

# [test autopurge]
sudo dnf -y remove octez-node octez-client octez-baker octez-dal-node
