#!/bin/sh

set -eu

# Install dependencies to use gsutils on Fedora/rockylinux

# Architecture detection
ARCH=$(uname -m)

#shellcheck disable=SC1091
if ! . /etc/os-release; then
  echo "Error: Failed to source /etc/os-release" >&2
  exit 1
fi

echo "$ID"
echo "$PLATFORM_ID"

# cf. https://docs.cloud.google.com/sdk/docs/install-sdk#rpm
# gcloud Repo for RHEL 7/8/9-compatible distributions. (In our case, Fedora 39, 41, 42 ; Rockylinux 9)
SDK="cloud-sdk-el9-$ARCH"
KEY="rpm-package-key.gpg"
case "$ID" in
"rocky" | "rockylinux")
  case "$PLATFORM_ID" in
  "platform:el10")
    # Repo for RHEL 10-compatible distributions (Rockylinux 10 in our case)
    SDK="cloud-sdk-el10-$ARCH"
    KEY="rpm-package-key-v10.gpg"
    ;;
  esac
  ;;
*)
  : noop
  ;;
esac

tee -a /etc/yum.repos.d/google-cloud-sdk.repo << EOM
[google-cloud-cli]
name=Google Cloud CLI
baseurl=https://packages.cloud.google.com/yum/repos/$SDK
enabled=1
gpgcheck=1
repo_gpgcheck=0
gpgkey=https://packages.cloud.google.com/yum/doc/$KEY
EOM

# Install core dependencies
dnf install -y gpg dnf-plugins-core

dnf install -y google-cloud-cli

# Optional: Verify installation
gcloud --version

echo "Google Cloud SDK installation completed successfully"
