#!/bin/sh

set -ex

export BUILDDIR
BUILDDIR="$(pwd)"
export BLST_PORTABLE=true
git fetch -q --tags
export CARGO_NET_OFFLINE=false
# shellcheck disable=SC1091
. "$HOME/.cargo/env"
eval "$(opam env --switch=/root/tezos/ --set-switch)"
sed -i 's/ocaml_version=[0-9]\+\.[0-9]\+\.[0-9]\+/ocaml_version=4.14.2/' scripts/version.sh
make octez
