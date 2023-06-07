#!/bin/sh
# shellcheck source=/dev/null
# for not checking the existence of sourced file $HOME/.cargo/env
# shellcheck disable=SC2154
# for undefined variable in: opam switch create for_tezos $ocaml_version
# shellcheck disable=SC2086
# for omitting quotes in: opam switch create for_tezos $ocaml_version
# shellcheck disable=SC2046
# for omitting quotes in: eval $(opam env)

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
. ./latest-release:version.sh
opam switch create for_tezos $ocaml_version
eval $(opam env)
# [get system dependencies]
# depext handling is done directly by opam 2.1 and later
opam depext octez
# [install tezos]
opam install -y octez
# [test executables]
octez-client --version
