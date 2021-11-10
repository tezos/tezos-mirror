#! /usr/bin/env bash
# shellcheck source=/dev/null
# shellcheck disable=SC2154
# shellcheck disable=SC2046
# shellcheck disable=SC2086

set -e
set -x

apt-get update
apt-get -y install make m4 gcc patch unzip bubblewrap wget
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
wget -O latest-release:version.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/version.sh
source latest-release:version.sh
opam switch create for_tezos $ocaml_version
eval $(opam env)
# [get system dependencies]
opam install depext
opam depext tezos
# [install tezos]
opam install -y tezos
