#!/bin/bash
set -eu

# This script modifies .gitattributes to add an 'export-ignore'
# attribute to all top-level paths that are not included in
# script-inputs/octez-source-content. It expects that paths in
# octez-source-content do not have trailing slashes.

# shellcheck source=./scripts/releases/octez-release.sh
. ./scripts/releases/octez-release.sh

# Adds export-ignore for each part of the repo that is not part of octez
ignore="$(comm -2 -3 <(find . -maxdepth 1 | sed 's|^./||' | sort) <(sort "${octez_source_content}"))"
for e in $ignore; do
  if ! [ "$e" = "." ] && ! [ "$e" = ".." ]; then
    # Note that $e must not have a trailing slash for the attribute to
    # take hold. The path $e must be prefixed by a slash, to anchor
    # the path at the root.
    echo "/$e export-ignore" >> ./.gitattributes
  fi
done
