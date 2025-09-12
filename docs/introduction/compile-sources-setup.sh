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
apt-get update
apt-get install -y sudo
export OPAMYES=true
# [install packages]
sudo apt-get install -y rsync git m4 build-essential patch unzip wget opam jq bc libev4 postgresql cmake
# [install rust]
wget https://sh.rustup.rs/rustup-init.sh
chmod +x rustup-init.sh
./rustup-init.sh --profile minimal --default-toolchain 1.88.0 -y
# [get sources]
