#!/bin/sh

set -ex

export BUILDDIR
BUILDDIR="$(pwd)"
export BLST_PORTABLE=true
git fetch -q --tags
# Prepare the building area: copying all files from
# the dependency image a staging area. This is necessary
# to build on arm64 where the BUILDDIR is in ram.
cp -a ./* /root/tezos/
cp -a ./.git /root/tezos/
cd /root/tezos/
export CARGO_NET_OFFLINE=false
# shellcheck disable=SC1091
. "$HOME/.cargo/env"
eval "$(opam env)"
sed -i 's/ocaml_version=[0-9]\+\.[0-9]\+\.[0-9]\+/ocaml_version=4.14.2/' scripts/version.sh
make octez
