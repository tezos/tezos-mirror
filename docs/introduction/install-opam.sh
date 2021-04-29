trap 'exit $?' ERR
set -x
sudo apt-get update
sudo apt-get install wget
export OPAMYES=true
export OPAMSOLVERTIMEOUT=1200
# [install ocaml compiler]
wget -O latest-release:version.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/version.sh
source latest-release:version.sh
opam switch create for_tezos $ocaml_version
eval $(opam env)
# [get system dependencies]
opam install depext
opam depext tezos
# [install tezos]
opam install -y tezos
