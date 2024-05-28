#!/bin/sh

## How to use:
## eval $(scripts/env.sh)

## This script setups the shell environment, to be able to build and
## use the generated executables, including the bytecode version

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"
src_dir="$(dirname "$script_dir")"

## this ensures that opam env hook will not overwrite this script
echo "_opam_env_hook() { :; };"

add_ld_path() {
  echo "CAML_LD_LIBRARY_PATH=\"$src_dir/$1:\$CAML_LD_LIBRARY_PATH\""
}

opam env

## this list should contain all the known shared libraries on Tezos
## to update it the following command may be used
## $ make; find _build/default | grep -e "dll.*\.so\|dylib$"

add_ld_path "_build/default/src/lib_sapling"
