trap 'exit $?' ERR
set -x

apt-get update
apt-get -y install make m4 gcc patch unzip bubblewrap
# apt-get install -y curl
# sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

apt-get -y install software-properties-common
# [install opam]
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get -y install opam
# [opam init]
opam init --bare -y
export OPAMYES=true
export OPAMSOLVERTIMEOUT=1200
# [install ocaml compiler]
opam switch create for_tezos 4.10.2
eval $(opam env)
# [get system dependencies]
opam install depext
opam depext tezos
# [install tezos]
opam install -y tezos
