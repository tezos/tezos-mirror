# TODO nomadic-labs/tezos#462: search shifted protocol name/no; rename script
trap 'exit $?' ERR
set -x
cd
# [install docker]
apt-get update
apt-get install -y docker.io docker-compose kmod wget
dockerd &
# [get florencenet]
wget -O florencenet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
chmod +x florencenet.sh
# [start florencenet]
./florencenet.sh start
