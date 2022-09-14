#!/bin/bash

set -e

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"

# shellcheck source=./scripts/ci/release.sh
. "$ci_dir/release.sh"

# set up ssh credentials to access github
mkdir -p "$HOME/.ssh"
cp "$TEZOS_GITHUB_OPAM_REPOSITORY_MACHINE_USER_PRIVATE_SSH_KEY" "$HOME/.ssh/id_rsa"
cat "$GITHUB_SSH_HOST_KEYS" >> "$HOME/.ssh/known_hosts"
chmod 600 "$HOME/known_hosts"
chmod 600 "$HOME/id_rsa"
chmod 700 "$HOME/.ssh"

# call opam-release.sh with the correct arguments
"$script_dir/opam-release.sh" \
  "$opam_release_tag" \
  "https://gitlab.com/tezos/tezos/-/archive/$CI_COMMIT_TAG/$gitlab_package_name.tar.gz"