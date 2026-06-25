#!/bin/sh

# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>

# Run cargo audit on Cargo.lock files to detect known vulnerabilities.
#
# Usage:
#   ./scripts/ci/cargo_audit.sh          # audit only MR-changed directories
#   ./scripts/ci/cargo_audit.sh --all    # audit all directories (scheduled pipelines)
#
# Results are classified into three tiers and the script exits with a
# distinct code per tier so the CI job can gate on severity:
#   - critical or high advisory (CVSS >= 7.0)  -> FAIL, exit 1
#   - lower / unrated advisory only            -> WARN, exit 2
#   - no advisory                              -> PASS, exit 0
#
# The CI job currently sets allow_failure: true, so every non-zero exit
# is reported as a warning (orange) without blocking.  Once the known
# advisories tracked in https://gitlab.com/tezos/tezos/-/issues/8331 are
# resolved, the job can switch to allow_failure: With_exit_codes [2] so
# that exit 1 (critical/high) blocks while exit 2 (lower) stays allowed.

set -eu

if [ -n "${TRACE:-}" ]; then set -x; fi

for cmd in cargo cargo-audit jq git; do
  if ! command -v "$cmd" > /dev/null 2>&1; then
    echo "ERROR: ${cmd} is not installed." >&2
    exit 1
  fi
done

TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT

# Compute CVSS 3.x severity counts from a list of CVSS vector strings.
#
# Why: cargo audit's JSON output includes CVSS vector strings (e.g.
# "CVSS:3.1/AV:N/AC:L/...") but no pre-computed severity label.
# We need to derive severity ourselves so we can fail on criticals only.
#
# The function reimplements the CVSS 3.x base-score algorithm from the
# rustsec crate — the same library that cargo-audit uses internally:
#   https://github.com/rustsec/rustsec/blob/main/cvss/src/v3/base.rs
# This ensures our severity classification matches cargo-audit's own.
# Note: the rustsec implementation intentionally deviates from the FIRST
# spec (exponent 15 instead of 13, no 0.9731 ISS factor for Scope Changed).
#
# The math looks involved because CVSS base scores require:
#   1. Mapping 8 metric letters to numeric weights
#   2. Computing Impact Sub-Score (ISS) from C/I/A weights
#   3. Applying a Scope-dependent impact formula (with exponentiation)
#   4. Combining impact + exploitability into a final score
#   5. "Rounding up" to one decimal and classifying into severity bands
#
# Input:  one CVSS vector string per line on stdin ("null" for missing)
# Output: critical=N high=N medium=N low=N unrated=N
# shellcheck disable=SC2016
cvss_count_severities() {
  awk '
  function parse_metric(vec, key,    parts, n, i, kv) {
    n = split(vec, parts, "/")
    for (i = 1; i <= n; i++) {
      split(parts[i], kv, ":")
      if (kv[1] == key) return kv[2]
    }
    return ""
  }

  function cvss_severity(vec,    av, ac, pr, ui, s, c, iv, a,
                                 av_s, ac_s, pr_s, ui_s, c_s, i_s, a_s,
                                 iss, impact, expl, base, rounded) {
    if (vec == "null" || substr(vec, 1, 6) != "CVSS:3") return "unrated"

    # Parse metric values from the vector string.
    av = parse_metric(vec, "AV")
    ac = parse_metric(vec, "AC")
    pr = parse_metric(vec, "PR")
    ui = parse_metric(vec, "UI")
    s  = parse_metric(vec, "S")
    c  = parse_metric(vec, "C")
    iv = parse_metric(vec, "I")
    a  = parse_metric(vec, "A")

    # Attack Vector
    if      (av == "N") av_s = 0.85
    else if (av == "A") av_s = 0.62
    else if (av == "L") av_s = 0.55
    else                av_s = 0.20

    # Attack Complexity
    ac_s = (ac == "L") ? 0.77 : 0.44

    # Privileges Required (score depends on Scope)
    if      (pr == "N") pr_s = 0.85
    else if (pr == "L") pr_s = (s == "C") ? 0.68 : 0.62
    else                pr_s = (s == "C") ? 0.50 : 0.27

    # User Interaction
    ui_s = (ui == "N") ? 0.85 : 0.62

    # Confidentiality / Integrity / Availability impact weights
    c_s = (c  == "H") ? 0.56 : (c  == "L") ? 0.22 : 0
    i_s = (iv == "H") ? 0.56 : (iv == "L") ? 0.22 : 0
    a_s = (a  == "H") ? 0.56 : (a  == "L") ? 0.22 : 0

    # Impact Sub-Score: ISS = 1 - ((1-C) * (1-I) * (1-A))
    iss = 1 - (1 - c_s) * (1 - i_s) * (1 - a_s)
    if (iss <= 0) return "none"

    # Impact depends on Scope.
    if (s == "C") {
      # Scope Changed — matches rustsec: exponent 15, no 0.9731 factor.
      impact = 7.52 * (iss - 0.029) - 3.25 * ((iss - 0.02) ^ 15)
      if (impact < 0) impact = 0
    } else {
      impact = 6.42 * iss
    }

    # Exploitability = 8.22 * AV * AC * PR * UI
    expl = 8.22 * av_s * ac_s * pr_s * ui_s

    # Base score, capped at 10.
    if (s == "C") {
      base = 1.08 * (impact + expl)
    } else {
      base = impact + expl
    }
    if (base > 10) base = 10

    # Round up to one decimal place (CVSS "roundup" function).
    rounded = int(base * 10)
    if (rounded < base * 10) rounded++
    base = rounded / 10.0

    # Severity classification per CVSS 3.x qualitative scale.
    if      (base >= 9.0) return "critical"
    else if (base >= 7.0) return "high"
    else if (base >= 4.0) return "medium"
    else if (base >  0)   return "low"
    else                   return "none"
  }

  { count[cvss_severity($0)]++ }

  END {
    printf "critical=%d high=%d medium=%d low=%d unrated=%d\n",
      count["critical"]+0, count["high"]+0, count["medium"]+0,
      count["low"]+0, count["unrated"]+0
  }
  '
}

# Extract severity counts from JSON audit output written to $TMPFILE.
# Output: critical=N high=N medium=N low=N unrated=N
parse_json_counts() {
  if ! jq -r '.vulnerabilities.list[] | .advisory.cvss // "null"' "$TMPFILE" \
    2> /dev/null | cvss_count_severities; then
    echo "WARNING: failed to parse JSON from cargo audit" >&2
    echo "critical=0 high=0 medium=0 low=0 unrated=0"
  fi
}

# Determine which directories to audit.
if [ "${1:-}" = "--all" ]; then
  echo "Auditing all directories containing Cargo.lock files."
  # Note: src/riscv and src/lib_riscv/kernels are also covered by
  # audit_riscv_deps (ci/lib_tezos_ci_jobs/kernels.ml) but we
  # intentionally include them here — audit_riscv_deps will be removed
  # once this job is fully rolled out.
  dirs=$(find . -name Cargo.lock \
    -not -path './_build/*' -not -path './_opam/*' \
    -exec dirname {} + | sed 's|^\./||' | sort)
else
  # shellcheck source=scripts/ci/gitlab_mr_environment.sh
  . ./scripts/ci/gitlab_mr_environment.sh

  merge_base=$(./scripts/ci/git_merge_base.sh \
    "${TEZOS_CI_MR_TARGET}" "${TEZOS_CI_MR_HEAD}")

  echo "Auditing directories with changed Cargo.lock files."
  dirs=$(git diff --name-only "${merge_base}" "${TEZOS_CI_MR_HEAD}" \
    -- '**/Cargo.lock' | while IFS= read -r f; do dirname "$f"; done | sort -u)

  if [ -z "$dirs" ]; then
    echo "No Cargo.lock changes detected. Nothing to audit."
    exit 0
  fi
fi

if [ -z "$dirs" ]; then
  echo "No directories with Cargo.lock found. Nothing to audit."
  exit 0
fi

# Get the value of "key=N" from a "critical=N high=N ..." counts line.
count_of() { echo "$2" | grep -oE "$1=[0-9]+" | cut -d= -f2; }

# Keep only the non-zero "key=N" pairs from a counts line, space-joined.
# e.g. "critical=2 high=0 medium=3 low=0 unrated=0" -> "critical=2 medium=3"
nonzero_counts() {
  out=""
  for kv in $1; do
    [ "${kv#*=}" -gt 0 ] && out="${out}${out:+ }${kv}"
  done
  echo "$out"
}

# One result line per directory: "<TIER> <dir> <details>".
results=""
has_fail=0
has_warn=0
while IFS= read -r dir; do
  [ -z "$dir" ] && continue
  if [ ! -f "${dir}/Cargo.lock" ]; then
    results="${results}
SKIP ${dir} no Cargo.lock"
    continue
  fi

  echo "--- cargo audit: ${dir} ---"
  audit_exit=0
  # --quiet leaves only the JSON report in $TMPFILE for the summary below.
  # cd into ${dir} (subshell, so the loop's cwd is untouched): cargo audit
  # reads .cargo/audit.toml from its working dir, not relative to --file.
  (cd "${dir}" && cargo audit --quiet --json --file Cargo.lock) \
    > "$TMPFILE" 2>&1 || audit_exit=$?

  # Print a human-readable line per advisory.
  jq -r '
    .vulnerabilities.list[] |
    "  \(.advisory.id) \(.advisory.package) \(.package.version) - \(.advisory.title)"
  ' "$TMPFILE" 2> /dev/null || true

  counts=$(parse_json_counts)
  critical=$(count_of critical "$counts")
  high=$(count_of high "$counts")
  found=$(nonzero_counts "$counts")

  # Tier the directory: critical or high -> FAIL; any lower/unrated
  # advisory (or an audit error) -> WARN; otherwise PASS.
  if [ "${critical:-0}" -gt 0 ] || [ "${high:-0}" -gt 0 ]; then
    results="${results}
FAIL ${dir} ${found}"
    has_fail=1
  elif [ -n "$found" ] || [ "$audit_exit" -ne 0 ]; then
    results="${results}
WARN ${dir} ${found:-audit error}"
    has_warn=1
  else
    results="${results}
PASS ${dir} no vulnerabilities"
  fi
done << EOF
$dirs
EOF

# Print the summary, sorted: FAIL first, then WARN, then PASS/SKIP.
echo ""
echo "========================================"
echo "  cargo audit summary"
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

# Exit code encodes the worst tier so the CI job can gate on severity:
#   1 = critical/high present, 2 = lower/unrated only, 0 = clean.
if [ "$has_fail" -ne 0 ]; then
  echo ""
  echo "FAILED: critical or high severity vulnerabilities found."
  exit 1
elif [ "$has_warn" -ne 0 ]; then
  echo ""
  echo "WARNING: lower severity or unrated advisories found."
  exit 2
fi
