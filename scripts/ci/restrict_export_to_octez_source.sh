#!/bin/bash
set -eu

# shellcheck source=./scripts/ci/octez-release.sh
. ./scripts/ci/octez-release.sh

# Adds export-ignore for each part of the repo that is not part of octez
ignore="$(comm -2 -3 <(find . -maxdepth 1 | sed 's|^./||' | sort) <(sort "${octez_source_content}"))"
for e in $ignore; do
  if ! [ "$e" = "." ] && ! [ "$e" = ".." ]; then
    echo "$e export-ignore" >> ./.gitattributes
  fi
done
