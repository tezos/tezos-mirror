#!/bin/sh
# shellcheck source=/dev/null
# for not checking the existence of sourced file $HOME/.cargo/env
# shellcheck disable=SC2046
# for omitting quotes in: eval $(opam env)
# shellcheck disable=SC2086
# for omitting quotes in: source $HOME/.cargo/env

usage() {
  cat >&2 << !EOF
usage:
  $0 [<repo> <branch>]
!EOF
}

if [ $# -eq 2 ]; then
  REPO=$1
  BRANCH=$2
elif [ $# -eq 0 ]; then # don't remove this branch used for an example in the doc!
  # [select branch]
  REPO="tezos/tezos"
  BRANCH="latest-release"
  # [end]
else
  usage
  exit 1
fi

set -e
set -x

# we clean the directory in the CI before cloning to avoid
# dune getting confused
if [ "$CI" = "true" ]; then rm -Rf ./*; fi

# [source cargo]
. $HOME/.cargo/env
# [get sources]
git clone https://gitlab.com/"$REPO".git tezos
cd tezos
git checkout $BRANCH
# [init opam]
opam init --bare
# [end init opam]

# this script uses a docker image with pre-compiled dependencies.
# we hide this detail here from the script that is copied verbatim in
# the documentation
ln -s /root/tezos/_opam .

# [make build-deps]
make build-deps
# [compile sources]
eval $(opam env)
make release
# [optional setup]
# puts Octez binaries in PATH:
# export PATH=$PWD/_build/install/default/bin/:$PATH
# if using bash, enables autocompletion:
# source ./src/bin_client/bash-completion.sh
# removes Mainnet/testnet disclaimers:
# export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y
# [test executables]
./octez-client --version
./octez-node --version
