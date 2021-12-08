# TODO tezos/tezos#2170: search shifted protocol name/no; rename script
trap 'exit $?' ERR
set -x
cd
# [install docker]
apt-get update
apt-get install -y docker.io docker-compose kmod wget
dockerd &
# [get testnet]
wget -O hangzhounet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
chmod +x hangzhounet.sh
# [start testnet]
./hangzhounet.sh start
