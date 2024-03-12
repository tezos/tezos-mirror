#!/bin/sh

# Implementation of CI job 'misc_opam_checks'.

# This script expects a copy of scripts/version.sh from tezos/opam-repository
# to be available at: /home/tezos/version.sh
# This is the case if this script runs in one of the Docker images that are
# built by tezos/opam-repository.

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

# shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

echo "## Checking installed dependencies..."
echo

if ! opam install opam/virtual/octez-deps.opam.locked --deps-only --with-test --show-actions | grep "Nothing to do." > /dev/null 2>&1; then
  echo
  echo 'Failure! Missing actions:'
  echo
  opam install opam/virtual/octez-deps.opam.locked --deps-only --with-test --show-actions
  echo
  # We really want literal backticks here, not command substitution.
  # shellcheck disable=SC2016
  echo 'Failed! Please read the doc in `./scripts/update_opam_repo.sh` and act accordingly.'
  echo
  exit 1
fi

# Check that the value of opam_repository_commit_hash in tezos/opam-repository
# matches the value of full_opam_repository_tag in tezos/tezos.
# It happens that tezos/opam-repository keeps its version.sh in the Docker image,
# so we read the value of opam_repository_commit_hash from it.
echo "## Checking opam repository commit hash..."

# Source the copy of `version.sh` in the image produced by tezos/opam-repository
opam_repository_commit_hash=$(
  # '/home/tezos/version.sh' is not in the source repo but in
  # one if the images, so shellcheck cannot analyze it.
  # shellcheck disable=SC1091
  . /home/tezos/version.sh
  echo "$opam_repository_commit_hash"
)

echo "- opam_repository_commit_hash = $opam_repository_commit_hash (from tezos/opam-repository)"
echo "- full_opam_repository_tag    = $full_opam_repository_tag (from tezos/tezos)"

if [ "$opam_repository_commit_hash" != "$full_opam_repository_tag" ]; then
  echo "Error: values do not match."
  echo "See http://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html"
  exit 1
fi

echo "Ok."
