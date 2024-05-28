#!/bin/sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$(dirname "$script_dir")")"

#shellcheck source=scripts/version.sh
. "$src_dir"/scripts/version.sh

found_alpine_version="$(cut -d. -f1-2 < /etc/alpine-release)"
if [ "${found_alpine_version}" != "${alpine_version}" ]; then
  echo "Expected Alpine major.minor version ${alpine_version}, got ${found_alpine_version}!"
  echo "Make sure that '.image_template__alpine' in '.gitlab-ci.yml' is coherent w.r.t."
  echo "'alpine_version' in 'scripts/version.sh'."
  exit 1
fi
