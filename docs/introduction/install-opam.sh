#!/usr/bin/env bash
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
sudo apt-get update
sudo apt-get install wget
# upgrade opam to v>=2.1, where depext is no longer needed
sudo apt-get -y upgrade opam
export OPAMYES=true
export OPAMSOLVERTIMEOUT=1200
# [make sure opam has the latest repo]
# Note that in the docker image used for the test, the default repo is a local
# copy dating from when the image was generated.
opam repository set-url default https://opam.ocaml.org
opam update
# [install ocaml compiler]
wget -O latest-release:version.sh https://gitlab.com/tezos/tezos/raw/latest-release/scripts/version.sh
source latest-release:version.sh
opam switch create for_tezos $ocaml_version
eval $(opam env)
# [install rust]
wget https://sh.rustup.rs/rustup-init.sh
chmod +x rustup-init.sh
./rustup-init.sh --profile minimal --default-toolchain $recommended_rust_version -y
. "$HOME/.cargo/env"
rustc --version
# [add octez repository]
opam remote remove -a octez
opam remote add octez https://github.com/tezos/opam-repository.git\#octez-latest
opam update
# [install tezos]
opam install octez
# [test executables]
octez-client --version
