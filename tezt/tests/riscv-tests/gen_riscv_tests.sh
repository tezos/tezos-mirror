#!/bin/sh
# shellcheck disable=SC2320,SC3044

# This script builds the riscv unit tests, located here https://github.com/riscv-collab/riscv-gnu-toolchain.
# It assumes that a GNU riscv toolchin is installed locally.
# To install the GNU riscv toolchain, follow the instructions here: https://github.com/riscv-collab/riscv-gnu-toolchain

set -e

build_dir=$(mktemp -d)

self=$(realpath "$(dirname "$0")")
target_dir=$(realpath "$self"/generated)

clean_up() {
  ARG=$?
  echo "exit($ARG) signal caught, cleaning up"
  popd || exit
  rm -fr "$build_dir"
  exit "$ARG"
}
trap clean_up EXIT

echo "Building riscv unit tests in temporary directory $build_dir"

pushd "$build_dir" || exit

git clone --recursive https://github.com/riscv/riscv-tests . || {
  echo "cloning failed" >&2
  clean_up 1
}

autoconf || {
  echo "autoconf failed" >&2
  clean_up 1
}

git apply "$self/riscv-tests-entry-0.patch"

./configure || {
  echo "configure failed" >&2
  clean_up 1
}

make -j 8 -C isa rv64ui rv64uc rv64um rv64ua rv64uf rv64ud rv64si rv64mi || {
  echo "make isa failed" >&2
  clean_up 1
}

echo "Copying rv64 binaries from $build_dir/isa to $target_dir"

# The `make` command generates the binaries in `isa/` but that directory also contains objdumps and sub-directories.
# We only copy those files which are neither.
for file in isa/rv64*; do
  if [ -f "$file" ]; then
    case "$file" in
    *dump) ;;
    *) cp "$file" "$target_dir"/ ;;
    esac
  fi
done

echo "Build finished"
