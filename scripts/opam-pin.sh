#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$script_dir")"

export OPAMYES=yes

echo
echo "## Pinning tezos packages..."

opams=$(find "$src_dir/vendors" "$src_dir/src" "$src_dir/tezt" -name \*.opam -print)

bin_packages=
lib_packages=
for opam in $opams; do
    dir=$(dirname "$opam")
    file=$(basename "$opam")
    package=${file%.opam}
    if echo "$dir" | grep -q "\/bin_.*" ; then
      bin_packages="$bin_packages $package"
    else
      lib_packages="$lib_packages $package"
    fi
    opam pin add --no-action "$package" "$dir" > /dev/null 2>&1
done


#shellcheck disable=SC2086
lib_packages=$(opam list --short --sort --pinned $lib_packages)
#shellcheck disable=SC2086
bin_packages=$(opam list --short --sort --pinned $bin_packages)

export packages="$bin_packages $lib_packages"
echo
echo "## Pinned lib packages:"
echo
echo "$lib_packages" | sed 's/^/ /'
echo

echo
echo "## Pinned bin packages:"
echo
echo "$bin_packages" | sed 's/^/ /'
echo
