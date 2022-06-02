#!/bin/sh

usage () {
    cat >&2 <<!EOF
usage:
  $0 [rc]
!EOF
}

if [ $# -eq 1 ] && [ "$1" = "rc" ]
then
  # [setup rc repository]
  REPO="@Serokell/Tezos-rc"
  # [end]
elif [ $# -eq 0 ]
then
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
dnf install -y tezos-baker-012-Psithaca
dnf install -y tezos-accuser-012-Psithaca
# [test executables]
tezos-client --version
tezos-node --version
tezos-baker-012-Psithaca --version
tezos-accuser-012-Psithaca --version
