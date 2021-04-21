trap 'exit $?' ERR
set -x
cd
# [install docker]
apt-get update
apt-get install -y docker.io docker-compose kmod wget
dockerd &
# [get edo2net]
wget -O edo2net.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
chmod +x edo2net.sh
# [start edo2net]
./edo2net.sh start
