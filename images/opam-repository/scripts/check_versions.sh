#!/bin/sh
set -eu

# To run after the final Docker image was built on top of all the
# other ones to display various software versions

# shellcheck source=./scripts/docker.sh
. ./scripts/docker.sh

current_dir=$(cd "$(dirname "${0}")" && pwd)

image_name="${1:-tezos/opam-repository}"
image_tag_suffix="${2:-}"
targetarch="${3:-amd64}"

error=''

die_if_error() {
  if [ -n "${error}" ]; then
    echo 'ERROR: found different version(s) than expected'
    echo 'Checkout Dockerfiles and verify which version is installed'
    echo '⚠️ Alpine packages are often updated and older versions are removed from repositories'
    exit 1
  fi
}

check_version() {
  if [ "${2}" != "${3}" ]; then
    echo "${1}: expected ${3}, got ${2} ❌"
    error='true'
  else
    echo "${1}: ${2} ✅"
  fi
}

check_version_in_test_dependency_image() {
  image_tag="$(docker_tag "runtime-build-test-dependencies" "$targetarch" "$image_tag_suffix")"

  run="docker run --rm ${image_name}:${image_tag}"

  echo "###"
  echo "### Version control for ${image_tag}"
  echo "###"

  # shellcheck source=./scripts/version.sh
  . "${current_dir}/version.sh"

  echo "### Distro info"
  eval "${run} cat /etc/os-release"

  echo "### Important packages version"

  current_cargo_version=$(${run} cargo --version | awk '{print $2}')
  check_version cargo "${current_cargo_version}" "${cargo_version}"

  current_rust_version=$(${run} rustc --version | awk '{print $2}')
  check_version rust "${current_rust_version}" "${rust_version}"

  current_opam_version=$(${run} opam --version)
  check_version opam "${current_opam_version}" "${opam_version}"

  current_ocaml_version=$(${run} ocaml --version | awk 'NF>1{print $NF}')
  check_version ocaml "${current_ocaml_version}" "${ocaml_version}"

  current_python_version=$(${run} python3 --version | awk 'NF>1{print $NF}')
  check_version python "${current_python_version}" "${python_version}"

  current_poetry_version=$(${run} poetry --version | sed -E 's/^Poetry\s+\(version\s+([0-9\.]+)\)$/\1/')
  check_version poetry "${current_poetry_version}" "${poetry_version}"

  die_if_error
}

check_version_in_e2e_test_dependency_image() {
  image_tag="$(docker_tag "runtime-e2etest-dependencies" "$targetarch" "$image_tag_suffix")"

  run="docker run --rm ${image_name}:${image_tag}"

  echo "###"
  echo "### Version control for ${image_tag}"
  echo "###"

  # shellcheck source=./scripts/version.sh
  . "${current_dir}/version.sh"

  echo "### Distro info"
  eval "${run} cat /etc/os-release"

  die_if_error
}

# Launch testing
check_version_in_test_dependency_image
check_version_in_e2e_test_dependency_image
