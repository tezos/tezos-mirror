#!/bin/sh

# Exit on non-zero status
set -e

# Patch the protocol sources:
# - add profiling to the latest protocols (currently alpha and quebec)

for arg in "$@"; do
  case $arg in
  "--dry-run")
    dry_run='--dry-run'
    ;;
  esac
done

#shellcheck disable=SC2086
patch $dry_run -p 1 < scripts/profile_quebec.patch

#shellcheck disable=SC2086
patch $dry_run -p 1 < scripts/profile_riotuma.patch

#shellcheck disable=SC2086
patch $dry_run -p 1 < scripts/profile_alpha.patch
