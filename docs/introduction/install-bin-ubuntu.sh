# TODO nomadic-labs/tezos#462: search shifted protocol name/number & adapt
trap 'exit $?' ERR
set -x
# [install prerequisites]
apt-get update
apt-get install sudo
apt-get install -y software-properties-common </dev/null
# [install tezos]
sudo add-apt-repository ppa:serokell/tezos && sudo apt-get update
sudo apt-get install -y tezos-client
sudo apt-get install -y tezos-node
sudo apt-get install -y tezos-baker-009-psfloren
sudo apt-get install -y tezos-endorser-009-psfloren
sudo apt-get install -y tezos-accuser-009-psfloren
