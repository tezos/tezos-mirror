#!/usr/bin/env bash

set -e

script_dir=$(dirname "$0")
etherlink_dir="$script_dir/.."
tezos_dir="$etherlink_dir/.."
commit=$(git rev-parse HEAD)

rust_image=${rust_image:-"registry.gitlab.com/tezos/tezos/rust-toolchain"}
rust_image_tag=${rust_image_tag:-"master"}
platform=${platform:-"linux/amd64"}

# register information about the rust-toolchain image
rust_toolchain_info() {
  _=$(docker pull -q "${rust_image}:${rust_image_tag}")
  docker inspect --format='{{index .RepoDigests 0}}' "${rust_image}:${rust_image_tag}"
}

# build docker image with wasm inside it
build() {
  evm_config=$1
  cp "$evm_config" "$etherlink_dir/config/.evm_config.yaml"
  docker build -t etherlink_kernel:"$commit" --build-arg EVM_CONFIG="etherlink/config/.evm_config.yaml" --build-arg RUST_IMAGE="$rust_image" --build-arg RUST_TAG="$rust_image_tag" --build-arg CI_COMMIT_SHA="$commit" -f "$script_dir"/docker-compose/evm_kernel_builder.Dockerfile --platform "$platform" "$tezos_dir"
  res_code=$?
  rm -f "$etherlink_dir"/config/.evm_config.yaml
  if [[ "${res_code}" -ne 0 ]]; then
    echo "docker build of evm kernel failed"
    exit 1
  fi
}

# copy images in output directory
copy() {
  output_dir=$1
  rust_image_version=$2
  container=$(docker create etherlink_kernel:"$commit")
  docker cp "$container":/kernel "$output_dir"
  {
    echo "rust-toolchain: $rust_image_version"
    echo "tezos: $commit"
  } > "$output_dir/.versions"
  echo "$container"
}

# clean up
cleanup() {
  container=$1
  _=$(docker container rm "$container")
}

arg1=$1

case $arg1 in
--help)
  cat << EOF
Reproducible EVM kernel builder
usage: [env_options] ./build-wasm.sh [[evm_config_file_path [output_directory]]
options:
- evm_config_file_path: input the config for the evm kernel installer (default=etherlink/config/dev.yaml)
- output_dir: directory where the wasm files (kernels, preimages) will be copied (default=etherlink/kernels-${commit})
env_options:
- rust_image (default="registry.gitlab.com/tezos/tezos/rust-toolchain")
- rust_image_tag (default="master")
- platform (default="linux/amd64")
This script builds evm kernels and preimages using the configuration given in <evm_config_file_path>.
For it to be reproducible, they are build in a docker container then copied on the local filesystem.
EOF
  ;;
*)
  evm_config=${1:-"$etherlink_dir/config/dev.yaml"}
  output_dir=${2:-"$etherlink_dir/kernels-$commit"}
  echo "fetching rust-toolchain image info: $rust_image:$rust_image_tag"
  rust_image_version=$(rust_toolchain_info)
  echo "building the docker image with $evm_config"
  build "$evm_config"
  echo "copying kernels and preimages in $output_dir"
  container=$(copy "$output_dir" "$rust_image_version")
  echo "cleaning up"
  cleanup "$container"
  ;;
esac
