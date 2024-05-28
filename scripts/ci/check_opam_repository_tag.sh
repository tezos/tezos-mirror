#!/bin/sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$(dirname "$script_dir")")"

#shellcheck source=scripts/version.sh
. "$src_dir"/scripts/version.sh

if [ -z "${build_deps_image_name}" ]; then echo "build_deps_image_name is unset" && exit 3; fi
if [ -z "${build_deps_image_version}" ]; then echo "build_deps_image_version is unset" && exit 3; fi

if [ "${build_deps_image_version}" != "${opam_repository_tag}" ]; then
  echo "Inconsistent dependencies hash between 'scripts/version.sh' and '.gitlab-ci.yml'."
  echo "${build_deps_image_version} != ${opam_repository_tag}"
  exit 1
fi
