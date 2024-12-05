#!/bin/sh

. ./scripts/version.sh
DISTRO=$(
  # shellcheck disable=SC1091
  . /etc/os-release
  echo "$ID"
)

echo "Running on $DISTRO / $DISTRIBUTION"
if [ "$DISTRO" = "rocky" ]; then
  dnf makecache --refresh
  dnf install -y 'dnf-command(config-manager)'
  dnf config-manager --set-enabled devel
  dnf config-manager --set-enabled crb
  dnf -y install epel-release
fi

dnf update -y
dnf install -y \
  libev-devel gmp-devel hidapi-devel libffi-devel zlib-devel \
  libpq-devel m4 perl git pkg-config rpmdevtools python3-devel \
  python3-setuptools wget rsync which cargo autoconf mock \
  systemd systemd-rpm-macros cmake python3-wheel \
  python3-tox-current-env gcc-c++ protobuf-compiler protobuf-devel \
  sqlite-devel jq openssl-devel

if [ "$DISTRO" = "rocky" ]; then
  # to update the install script :
  # curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh \
  #   -o scripts/pkg-common/install_opam.sh
  # this scripts won't run without a little confirmation.
  echo "" | sh scripts/pkg-common/install_opam.sh
else
  dnf install -y opam
fi
