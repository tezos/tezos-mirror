#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

export OPAMYES="${OPAMYES:=true}"

# install_build_deps.sh calls install_build_deps.rust.sh which checks whether
# Rust is installed with the right version and explains how to install it if
# needed, so here we only make opam acknowledge that we have a rust compiler
# we installed by our own.
# If we use opam depext, it will probably not install the right version.
OPAMASSUMEDEPEXTS=true opam install conf-rust conf-rust-2021

# Opam < 2.1 uses opam-depext as a plugin, later versions provide the option
# `--depext-only`:
case $(opam --version) in
2.0.*)
  opam pin add -n -y octez-deps opam/virtual/ && opam depext octez-deps
  opam pin remove octez-deps
  ;;
*) opam install --depext-only opam/virtual/octez-deps.opam ;;
esac

opam install opam/virtual/octez-deps.opam --deps-only --criteria="-notuptodate,-changed,-removed"

if [ "$1" = "--tps" ]; then
  opam install caqti-driver-postgresql
fi
