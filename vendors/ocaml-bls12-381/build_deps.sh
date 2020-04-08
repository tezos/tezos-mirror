#!/bin/bash

set -e

BUILD_DIR=$(mktemp -dt rustc.bls12.381.XXXXXXXX)

function build_rustc_bls12_381 () {
  #directory where this script lives
  local dir_script=$(cd $(dirname $0) && pwd)

  local commit="4280ea6d13c128d84b384d72f4f0ccaab8143247"
  local repository_name="rustc-bls12-381"
  local repository="https://gitlab.com/dannywillems/rustc-bls12-381"
  local install_dir="${OPAM_SWITCH_PREFIX}/lib"
  local header_dir="${OPAM_SWITCH_PREFIX}/include"
  local full_url_zip="${repository}/-/archive/${commit}/${repository_name}-${commit}.zip"

  echo "Installing rustc-bls12-381 in ${install_dir}"
  command -v cargo > /dev/null 2>&1 || { echo >&2 "Install cargo. Aborting."; exit 1; }
  mkdir -p ${BUILD_DIR}
  mkdir -p ${header_dir}
  cd ${BUILD_DIR}
  echo "Downloading rustc-bls12-381 from ${full_url_zip}"
  curl -L ${full_url_zip} --output ${commit}.zip
  unzip ${commit}.zip >> /dev/null # do not print the resulting file
  cd ${repository_name}-${commit}
  export RUST_BACKTRACE=1
  cargo build --release
  cp ${BUILD_DIR}/${repository_name}-${commit}/target/release/librustc_bls12_381.a ${install_dir}
  cp ${BUILD_DIR}/${repository_name}-${commit}/include/*.h ${header_dir}
  rm -rf ${BUILD_DIR}
}


function cleanup () {
    echo "Cleaning up build directory ${BUILD_DIR}"
    rm -rf ${BUILD_DIR}
}

trap cleanup EXIT INT

build_rustc_bls12_381
