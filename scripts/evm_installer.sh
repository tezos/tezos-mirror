#!/bin/sh

set -e

# This file is used to generate the EVM kernel to deploy.
# It will be used to properly call the smart-rollup-installer with a
# configuration adapted to the EVM kernel, e.g. set the dictator key.

print_usage() {
    echo "Usage: ${0}"
    echo "Options:"
    echo "  --evm-kernel <kernel.wasm>"
    echo "  --preimages-dir <dir>           [OPTIONAL]"
    echo "  --output <file>                 [OPTIONAL]"
}

option_error () {
    echo "Incorrect option ${1}"
    print_usage
    exit 1
}

if [ $# -eq 0 ];
then
    print_usage
    exit 1
fi

while [ $# -gt 0 ]; do
  curr="$1"

  case $curr in
      --evm-kernel)
          evm_kernel="$2"
          shift 2
          ;;
      --preimages-dir)
          preimages_dir="$2"
          shift 2;
          ;;
      --output)
          output="$2"
          shift 2;
          ;;
      *)    # unknown option
          option_error "$1"
          ;;
  esac
done

preimages_dir=${preimages_dir:-/tmp}
mkdir -p "$preimages_dir"
output=${output:-evm_installer.wasm}

# Calls the installer
./smart-rollup-installer get-reveal-installer \
                         --upgrade-to "$evm_kernel" \
                         --output "$output" \
                         --preimages-dir "$preimages_dir"

echo "Installer ready at ${output} with preimages in ${preimages_dir}."
