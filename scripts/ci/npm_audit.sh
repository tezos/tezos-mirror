#!/bin/sh

# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Run npm audit on package-lock.json files to detect critical
# vulnerabilities.
#
# Usage:
#   ./scripts/ci/npm_audit.sh          # audit only MR-changed directories
#   ./scripts/ci/npm_audit.sh --all    # audit all directories (scheduled pipelines)

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

for cmd in npm jq git; do
  if ! command -v "$cmd" > /dev/null 2>&1; then
    echo "ERROR: ${cmd} is not installed." >&2
    exit 1
  fi
done

# Vendored third-party sources. Their npm packages are neither built nor
# deployed by this repository, so they are not audited; they still appear
# as SKIP entries in the summary to keep the exclusion visible.
is_vendored() {
  case "$1" in
  src/rust_deps/wasmer-3.3.0/*) return 0 ;;
  *) return 1 ;;
  esac
}

# Extract vulnerability counts from npm audit --json output.
# Input:  JSON string from npm audit --json
# Output: "critical=N high=N moderate=N low=N"
parse_counts() {
  echo "$1" | jq -r '
    if .metadata.vulnerabilities then
      .metadata.vulnerabilities
      | "critical=\(.critical) high=\(.high) moderate=\(.moderate) low=\(.low)"
    else
      "critical=? high=? moderate=? low=?"
    end
  '
}

# Determine which directories to audit.
if [ "${1:-}" = "--all" ]; then
  echo "Auditing all directories containing package-lock.json files."
  dirs=$(find . -name package-lock.json \
    -not -path './_build/*' -not -path './_opam/*' \
    -not -path '*/node_modules/*' \
    -exec dirname {} \; | sed 's|^\./||')
  if [ -z "$dirs" ]; then
    echo "No directories with package-lock.json found. Nothing to audit."
    exit 0
  fi
else
  # shellcheck source=scripts/ci/gitlab_mr_environment.sh
  . ./scripts/ci/gitlab_mr_environment.sh

  merge_base=$(./scripts/ci/git_merge_base.sh \
    "${TEZOS_CI_MR_TARGET}" "${TEZOS_CI_MR_HEAD}")

  echo "Auditing directories with changed package-lock.json files."
  dirs=$(git diff --name-only "${merge_base}" "${TEZOS_CI_MR_HEAD}" \
    -- '**/package-lock.json' |
    grep -v '/node_modules/' |
    while IFS= read -r f; do dirname "$f"; done)

  if [ -z "$dirs" ]; then
    echo "No package-lock.json changes detected. Nothing to audit."
    exit 0
  fi
fi

results=""
has_failure=0
for dir in $dirs; do
  if is_vendored "$dir"; then
    echo "--- skipping vendored directory: ${dir} ---"
    results="${results}
SKIP ${dir} (vendored third-party code, not built nor deployed)"
    continue
  fi

  if [ ! -f "${dir}/package-lock.json" ]; then
    echo "WARNING: ${dir} has no package-lock.json — this should not happen."
    results="${results}
WARN ${dir} - (missing package-lock.json)"
    continue
  fi

  echo "--- npm audit: ${dir} ---"
  audit_exit=0
  audit_json=$(npm audit --json --audit-level=critical --prefix "${dir}" 2> /dev/null) || audit_exit=$?

  # Check whether npm returned a JSON error (network, malformed lock file, etc.)
  # rather than an audit report with metadata.
  if echo "$audit_json" | jq -e '.error' > /dev/null 2>&1; then
    error_summary=$(echo "$audit_json" | jq -r '.error.summary')
    echo "ERROR: npm audit failed for ${dir}: ${error_summary}"
    results="${results}
ERROR ${dir} (${error_summary})"
    has_failure=1
  # Critical vulnerabilities detected
  elif [ "$audit_exit" -ne 0 ]; then
    counts=$(parse_counts "$audit_json")
    results="${results}
FAIL ${dir} ${counts}"
    has_failure=1
  else
    counts=$(parse_counts "$audit_json")
    results="${results}
PASS ${dir} ${counts}"
  fi
done

# Print the summary.
# Format: STATUS directory critical=N high=N moderate=N low=N
echo ""
echo "========================================"
echo "  npm audit summary"
echo "========================================"
echo "$results" | while IFS= read -r line; do
  [ -z "$line" ] && continue
  echo "$line"
done
echo "========================================"

if [ "$has_failure" -ne 0 ]; then
  exit 1
fi
