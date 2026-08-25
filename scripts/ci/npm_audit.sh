#!/bin/sh

# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Run npm audit on package-lock.json files to detect known vulnerabilities.
#
# Usage:
#   ./scripts/ci/npm_audit.sh          # audit only MR-changed directories
#   ./scripts/ci/npm_audit.sh --all    # audit all directories (scheduled pipelines)
#
# Results are classified into three tiers and the script exits with a
# distinct code per tier:
#   - critical vulnerability           -> FAIL, exit 1
#   - high / moderate / low only       -> WARN, exit 2
#   - no vulnerability                 -> PASS, exit 0
#
# Critical is set apart from the lower tiers because high and lower
# advisories are frequently unpatched transitive dependencies (e.g. under
# solc or eth-cli's web3@1 tree) with no available fix.

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

# Get the value of "key=N" from a "critical=N high=N ..." counts line.
# Non-numeric (e.g. "?") or missing values yield 0.
count_of() {
  n=$(echo "$2" | grep -oE "$1=[0-9]+" | cut -d= -f2)
  echo "${n:-0}"
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
has_fail=0
has_warn=0
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
    has_warn=1
    continue
  fi

  echo "--- npm audit: ${dir} ---"
  # No --audit-level: npm's exit code is not used for tiering; severity is
  # derived from the parsed counts below (npm always reports full counts in
  # the JSON metadata regardless of --audit-level).
  audit_json=$(npm audit --json --prefix "${dir}" 2> /dev/null) || true

  # Check whether npm returned a JSON error (network, malformed lock file,
  # etc.) rather than an audit report with metadata. Classify it as WARN
  # rather than FAIL so transient tooling errors are not reported as a
  # critical vulnerability; the same convention as cargo_audit.sh.
  if echo "$audit_json" | jq -e '.error' > /dev/null 2>&1; then
    error_summary=$(echo "$audit_json" | jq -r '.error.summary')
    echo "ERROR: npm audit failed for ${dir}: ${error_summary}"
    results="${results}
WARN ${dir} (audit error: ${error_summary})"
    has_warn=1
    continue
  fi

  counts=$(parse_counts "$audit_json")
  critical=$(count_of critical "$counts")
  high=$(count_of high "$counts")
  moderate=$(count_of moderate "$counts")
  low=$(count_of low "$counts")

  # Tier the directory: critical -> FAIL; high/moderate/low -> WARN;
  # otherwise PASS.
  if [ "$critical" -gt 0 ]; then
    results="${results}
FAIL ${dir} ${counts}"
    has_fail=1
  elif [ "$high" -gt 0 ] || [ "$moderate" -gt 0 ] || [ "$low" -gt 0 ]; then
    results="${results}
WARN ${dir} ${counts}"
    has_warn=1
  else
    results="${results}
PASS ${dir} ${counts}"
  fi
done

# Print the summary, sorted: FAIL first, then WARN, then PASS/SKIP.
# Format: STATUS directory critical=N high=N moderate=N low=N
echo ""
echo "========================================"
echo "  npm audit summary"
echo "========================================"
echo "$results" | while IFS= read -r line; do
  [ -z "$line" ] && continue
  case "$line" in
  FAIL*) echo "1 $line" ;;
  WARN*) echo "2 $line" ;;
  PASS*) echo "3 $line" ;;
  SKIP*) echo "4 $line" ;;
  *) echo "5 $line" ;;
  esac
done | sort -n | cut -d' ' -f2-
echo "========================================"

# Exit code encodes the worst tier found:
#   1 = critical present, 2 = high/moderate/low only, 0 = clean.
if [ "$has_fail" -ne 0 ]; then
  echo ""
  echo "FAILED: critical severity vulnerabilities found."
  exit 1
elif [ "$has_warn" -ne 0 ]; then
  echo ""
  echo "WARNING: high, moderate, or low severity vulnerabilities found."
  exit 2
fi
