#!/bin/sh
# shellcheck source=/dev/null
# for not checking the existence of sourced file $HOME/.cargo/env
# shellcheck disable=SC2046
# for omitting quotes in: eval $(opam env)
# shellcheck disable=SC2086
# for omitting quotes in: source $HOME/.cargo/env

set -e
set -x
cd
# [install prerequisites]
sudo apt-get update
sudo apt-get install -y sudo
sudo apt-get install -y cargo # NV: to avoid error on compiling rust-conf
export OPAMYES=true
# [install packages]
sudo apt install -y rsync git m4 build-essential patch unzip wget pkg-config libgmp-dev libev-dev libhidapi-dev opam jq zlib1g-dev bc autoconf
# [install rust]
wget https://sh.rustup.rs/rustup-init.sh
chmod +x rustup-init.sh
./rustup-init.sh --profile minimal --default-toolchain 1.52.1 -y
# [source cargo]
. $HOME/.cargo/env
# [get sources]
git clone https://gitlab.com/tezos/tezos.git
cd tezos
git checkout latest-release
# [install Tezos dependencies]
opam init --bare
make build-deps
# [compile sources]
eval $(opam env)
make
# [optional setup]
export PATH=$HOME/tezos:$PATH
# if using bash: source ./src/bin_client/bash-completion.sh
export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y
# [test executables]
tezos-client --version
tezos-node --version
tezos-baker-alpha --version
tezos-accuser-alpha --version
