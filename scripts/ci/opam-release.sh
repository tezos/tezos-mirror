#!/bin/bash

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"

# shellcheck source=./scripts/ci/release.sh
. "$ci_dir/release.sh"

# call opam-release.sh with the correct arguments
"$script_dir/opam-release.sh" \
  "$opam_release_tag" \
  "https://gitlab.com/tezos/tezos/-/archive/$CI_COMMIT_TAG/$gitlab_package_name.tar.gz"
