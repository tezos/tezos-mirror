trap 'exit $?' ERR
set -x
cd
# [install docker]
apt-get update
apt-get install -y docker.io docker-compose kmod wget
dockerd &
# [get delphinet]
wget -O delphinet.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/tezos-docker-manager.sh
chmod +x delphinet.sh
# [start delphinet]
./delphinet.sh start
