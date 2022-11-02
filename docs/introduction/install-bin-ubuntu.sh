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
  REPO="ppa:serokell/tezos-rc"
  # [end]
elif [ $# -eq 0 ]
then
  # [setup stable repository]
  REPO="ppa:serokell/tezos"
  # [end]
else
  usage
  exit 1
fi

# TODO tezos/tezos#2170: search shifted protocol name/number & adapt
set -e
set -x
# [install prerequisites]
apt-get update
apt-get install sudo
apt-get install -y software-properties-common </dev/null
# [install tezos]
sudo add-apt-repository -y $REPO && sudo apt-get update
sudo apt-get install -y tezos-client
sudo apt-get install -y tezos-node
sudo apt-get install -y tezos-baker-014-ptkathma
sudo apt-get install -y tezos-accuser-014-ptkathma
# [test executables]
tezos-client --version
tezos-node --version
tezos-baker-014-PtKathma --version
tezos-accuser-014-PtKathma --version
