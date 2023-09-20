#!/bin/bash -e

COMMIT="ab335a9a5e5b691ffb919813e7115809584474dd"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
TMP_DIRECTORY=$(mktemp -d)
cd "${TMP_DIRECTORY}" || exit
wget https://github.com/supranational/blst/archive/${COMMIT}.zip
unzip ${COMMIT}.zip
cd blst-${COMMIT} || exit

# remove existing copy of blst
rm -rf "${SCRIPT_DIR}"/libblst

mkdir "${SCRIPT_DIR}"/libblst
mkdir "${SCRIPT_DIR}"/libblst/bindings

# copy the C headers
mv bindings/blst.h "${SCRIPT_DIR}"/libblst/bindings
mv bindings/blst_aux.h "${SCRIPT_DIR}"/libblst/bindings

# copy build script, removing tabs to pass the sanity checks
tr "\t" ' ' < build.sh > "${SCRIPT_DIR}"/libblst/build.sh
# patch script to not print the compilation steps to the terminal
sed -i 's/(set -x;[ tab]\+/(/' "${SCRIPT_DIR}"/libblst/build.sh

# copy source and build directories
mv build "${SCRIPT_DIR}"/libblst
mv src "${SCRIPT_DIR}"/libblst

# include blst_extended source and header files
echo '#include "blst_extended.c"' >> "${SCRIPT_DIR}"/libblst/src/server.c
sed -i '/#include "blst_aux.h"/i \
#include "blst_extended.h"\n' "${SCRIPT_DIR}"/libblst/bindings/blst.h
