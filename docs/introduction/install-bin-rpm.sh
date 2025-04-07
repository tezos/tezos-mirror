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

set -e
set -x

if [ "$RELEASETYPE" = "Master" ]; then
  # [add repository]
  # Update and install the config-mananger plugin
  dnf -y update
  dnf -y install dnf-plugins-core

  # Add the repository
  dnf -y config-manager --add-repo "https://packages.nomadic-labs.com/$distribution/dists/$release/"
  if [ "$distroname" = "rockylinux" ]; then
    dnf -y config-manager --set-enabled devel
  fi
  dnf -y update

  # Install public key
  rpm --import "https://packages.nomadic-labs.com/$distribution/octez.asc"
  # [end add repository]
else
  # Update and install the config-mananger plugin
  dnf -y update
  dnf -y install dnf-plugins-core

  # Add the repository
  dnf -y config-manager --add-repo "https://storage.googleapis.com/$bucket/$distribution/dists/$release/"

  if [ "$distroname" = "rockylinux" ]; then
    dnf -y config-manager --set-enabled devel
  fi
  dnf -y update

  # Install public key
  rpm --import "https://storage.googleapis.com/$bucket/$distribution/octez.asc"

fi

dnf -y install sudo

# [install tezos]
sudo dnf -y install octez-node
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

# [test autopurge]
sudo dnf -y remove octez-node octez-client octez-baker octez-dal-node
