#!/bin/bash

distroname=$1
release=$2

bucket="$GCP_LINUX_PACKAGES_BUCKET"

. scripts/ci/octez-packages-version.sh

# include apt-get function with retry
. scripts/packaging/tests/tests-common.inc.sh

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
  # Install public key
  rpm --import "https://packages.nomadic-labs.com/$distribution/octez.asc"
  # [end add repository]

  if [ "$distroname" = "rockylinux" ]; then
    dnf -y config-manager --set-enabled devel
  fi
  dnf -y update

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
