#!/bin/sh

# TODO tezos/tezos#2170: search shifted protocol name/no; rename script
set -e
set -x
cd
# [install docker]
apt-get update
apt-get install -y docker.io docker-compose kmod wget
dockerd &
# [get testnet]
wget -O ithacanet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
chmod +x ithacanet.sh
# [start testnet]
./ithacanet.sh start
