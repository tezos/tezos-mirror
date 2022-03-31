#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

#shellcheck source=scripts/opam-remove.sh
. "$script_dir"/opam-remove.sh

# make sure this variable is declared
packages=${packages:?}

echo
echo "## Unpinning tezos packages..."

# here we cannot use double quotes because otherwise the list of packages
# will be intepreted as a string and not as a list of strings leading to
# an error.
# shellcheck disable=SC2086
opam pin remove $packages > /dev/null 2>&1
