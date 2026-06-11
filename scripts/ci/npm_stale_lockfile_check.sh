#!/bin/sh

# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Check that every package.json change in an MR is accompanied by a
# package-lock.json change in the same directory.  Only directories
# that already contain a package-lock.json are checked (new projects
# without a lock file are ignored).
#
# Usage:
#   ./scripts/ci/npm_stale_lockfile_check.sh

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

if ! command -v git > /dev/null 2>&1; then
  echo "ERROR: git is not installed." >&2
  exit 1
fi

# shellcheck source=scripts/ci/gitlab_mr_environment.sh
. ./scripts/ci/gitlab_mr_environment.sh

merge_base=$(./scripts/ci/git_merge_base.sh \
  "${TEZOS_CI_MR_TARGET}" "${TEZOS_CI_MR_HEAD}")

# Directories with changed package-lock.json files.
lock_dirs=$(git diff --name-only "${merge_base}" "${TEZOS_CI_MR_HEAD}" \
  -- '**/package-lock.json' |
  while IFS= read -r f; do dirname "$f"; done)

# Directories where package.json changed but package-lock.json did not,
# and a package-lock.json already exists (i.e. the project uses lock files).
has_failure=0
for f in $(git diff --name-only "${merge_base}" "${TEZOS_CI_MR_HEAD}" \
  -- '**/package.json'); do
  d=$(dirname "$f")

  # Skip directories whose lock file was also changed.
  case " ${lock_dirs} " in
  *" ${d} "*) continue ;;
  esac

  # Skip directories that don't have a lock file at all.
  [ -f "${d}/package-lock.json" ] || continue

  echo "FAIL: ${d}/package.json changed without updating package-lock.json"
  has_failure=1
done

if [ "$has_failure" -ne 0 ]; then
  echo ""
  echo "Run 'npm install' in the above directories and commit the updated lock file."
  exit 1
fi

echo "OK: all package.json changes have matching package-lock.json updates."
