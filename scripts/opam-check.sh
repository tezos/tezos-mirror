#!/bin/sh

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

echo "Ok."
