#!/bin/sh

usage() {
  cat >&2 << !EOF
usage:
  $0 [rc]
!EOF
}

if [ $# -eq 1 ] && [ "$1" = "rc" ]; then
  # [setup rc repository]
  REPO="@Serokell/Tezos-rc"
  # [end]
elif [ $# -eq 0 ]; then
  # [setup stable repository]
  REPO="@Serokell/Tezos"
  # [end]
else
  usage
  exit 1
fi

# TODO tezos/tezos#2170: search shifted protocol name/number & adapt
set -e
set -x
# [install prerequisites]
dnf install -y dnf-plugins-core
# [install tezos]
dnf copr enable -y $REPO && dnf update -y
dnf install -y tezos-client
dnf install -y tezos-node
dnf install -y tezos-baker-PtNairob
dnf install -y tezos-accuser-PtNairob
# [test executables]
octez-client --version
octez-node --version
octez-baker-PtNairob --version
octez-accuser-PtNairob --version
