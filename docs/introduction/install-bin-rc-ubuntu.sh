#!/bin/sh

# TODO tezos/tezos#2170: search shifted protocol name/number & adapt
set -e
set -x
# [install prerequisites]
apt-get update
apt-get install sudo
apt-get install -y software-properties-common </dev/null
# [setup repository]
sudo add-apt-repository ppa:serokell/tezos-rc && sudo apt-get update
# [install tezos]
sudo apt-get install -y tezos-client
sudo apt-get install -y tezos-node
sudo apt-get install -y tezos-baker-012-psithaca
sudo apt-get install -y tezos-accuser-012-psithaca
