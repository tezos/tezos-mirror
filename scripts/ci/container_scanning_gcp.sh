#!/bin/sh

# Run GCP on-demand vulnerability scanning on released Docker images.
# Generates a GitLab container-scanning report and an optional
# cosign-vuln predicate for downstream attestation in
# docker:promote_to_latest.
#
# Usage: container_scanning_gcp.sh <report> [vuln_predicate]
#   <report>          — output filename for the GitLab container-scanning
#                       JSON report (written to $CI_PROJECT_DIR/<report>).
#   [vuln_predicate]  — optional output filename for the cosign-vuln
#                       predicate (written to $CI_PROJECT_DIR/<predicate>).
#
# Requires the On-Demand Scanning API (ondemandscanning.googleapis.com)
# enabled on the GCP project.
#
# Security gate: exits non-zero when CRITICAL or HIGH vulnerabilities are
# found.
#
# Expected environment (set by docker_image_names.sh / docker.env):
#   DOCKER_IMAGE_TAG, GCP_RELEASE_REGISTRY,
#   CI_PROJECT_NAMESPACE, CI_PROJECT_NAME, CI_PROJECT_DIR.

set -eu

REPORT="${1:?Usage: container_scanning_gcp.sh <report> [vuln_predicate]}"
VULN_PREDICATE="${2:-}"

scripts_dir="${CI_PROJECT_DIR}/scripts/ci"

# Authenticate to GCP (sets up gcloud credentials).
# The CI service account requires the following GCP roles:
#   - Container Analysis Viewer  (roles/containeranalysis.viewer)
#   - On-Demand Scanning Admin   (roles/ondemandscanning.admin)
# shellcheck source=scripts/ci/gcp_auth.sh
. "${scripts_dir}/gcp_auth.sh"

# Build the GCP Artifact Registry image reference.  We always scan the
# GCP copy even when CI_DOCKER_HUB=true (Docker Hub images are not
# scanned here). Callers can override [GCP_IMAGE] in the environment
# (e.g. when scanning images that live in a different AR repository).
GCP_IMAGE="${GCP_IMAGE:-${GCP_RELEASE_REGISTRY}/${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}:${DOCKER_IMAGE_TAG}}"

echo "==> Scanning image: ${GCP_IMAGE}"

# ---------------------------------------------------------------------------
# Trigger an on-demand vulnerability scan (blocks until complete).
# ---------------------------------------------------------------------------
# TODO: remove this runtime install once the ci-docker base image is rebuilt
# with local-extract (see images/base-images/Dockerfile.alpine-docker-ci).
echo "==> Installing on-demand scanning component..."
gcloud components install local-extract --quiet 2> /dev/null || true

echo "==> Starting on-demand scan..."

SCAN_ERR=$(mktemp)
SCAN_OUTPUT=$(gcloud artifacts docker images scan "${GCP_IMAGE}" \
  --remote --quiet --format=json 2> "${SCAN_ERR}") || {
  echo "ERROR: On-demand scan failed:"
  cat "${SCAN_ERR}"
  rm -f "${SCAN_ERR}"
  exit 1
}
rm -f "${SCAN_ERR}"

SCAN_RESOURCE=$(printf '%s' "${SCAN_OUTPUT}" | jq -r '.response.scan // empty')
if [ -z "${SCAN_RESOURCE}" ]; then
  echo "ERROR: Could not extract scan resource from output:"
  printf '%s' "${SCAN_OUTPUT}" | jq .
  exit 1
fi

echo "==> Scan complete: ${SCAN_RESOURCE}"

# ---------------------------------------------------------------------------
# Fetch vulnerability occurrences.
# ---------------------------------------------------------------------------
echo "==> Fetching vulnerability results..."

VULNS_ERR=$(mktemp)
VULNS_JSON=$(gcloud artifacts docker images list-vulnerabilities \
  "${SCAN_RESOURCE}" --format=json 2> "${VULNS_ERR}") || {
  echo "ERROR: Failed to list vulnerabilities:"
  cat "${VULNS_ERR}"
  rm -f "${VULNS_ERR}"
  exit 1
}
rm -f "${VULNS_ERR}"

# ---------------------------------------------------------------------------
# Tally vulnerabilities by severity.
# ---------------------------------------------------------------------------
CRITICAL=$(printf '%s' "${VULNS_JSON}" | jq '[.[] | select(.vulnerability.effectiveSeverity == "CRITICAL")] | length')
HIGH=$(printf '%s' "${VULNS_JSON}" | jq '[.[] | select(.vulnerability.effectiveSeverity == "HIGH")] | length')
MEDIUM=$(printf '%s' "${VULNS_JSON}" | jq '[.[] | select(.vulnerability.effectiveSeverity == "MEDIUM")] | length')
LOW=$(printf '%s' "${VULNS_JSON}" | jq '[.[] | select(.vulnerability.effectiveSeverity == "LOW")] | length')
TOTAL=$(printf '%s' "${VULNS_JSON}" | jq 'length')

echo ""
echo "==> Vulnerability summary for ${GCP_IMAGE}"
echo "    CRITICAL : ${CRITICAL}"
echo "    HIGH     : ${HIGH}"
echo "    MEDIUM   : ${MEDIUM}"
echo "    LOW      : ${LOW}"
echo "    TOTAL    : ${TOTAL}"

# ---------------------------------------------------------------------------
# GitLab Container Scanning report.
# https://gitlab.com/gitlab-org/security-products/security-report-schemas
# ---------------------------------------------------------------------------
SCAN_TIME=$(date -u +"%Y-%m-%dT%H:%M:%S")

printf '%s' "${VULNS_JSON}" | jq \
  --arg image "${GCP_IMAGE}" \
  --arg scan_time "${SCAN_TIME}" '
(
  {
    id: "gcp-artifact-registry",
    name: "GCP Artifact Registry Vulnerability Scanning",
    version: "latest",
    vendor: { name: "Google Cloud Platform" }
  }
) as $scanner |
{
  version: "15.0.6",
  scan: {
    start_time: $scan_time,
    end_time:   $scan_time,
    status:     "success",
    type:       "container_scanning",
    scanner:    $scanner,
    analyzer:   $scanner
  },
  vulnerabilities: [
    .[]
    | .vulnerability as $v
    | ($v.packageIssue // [{}] | .[0] // {}) as $pkg
    | {
        id:       ([$v.shortDescription // "unknown", $pkg.affectedPackage // ""]
                    | join("_")),
        category: "container_scanning",
        severity: (
          { "CRITICAL": "Critical", "HIGH": "High",
            "MEDIUM": "Medium", "LOW": "Low" }
          [$v.effectiveSeverity // ""] // "Unknown"
        ),
        name:        ($v.shortDescription // "Unknown vulnerability"),
        description: ($v.longDescription  // $v.shortDescription // ""),
        location: {
          image:            $image,
          operating_system: (
            ($pkg.affectedCpeUri // "")
            | split(":")
            | if length > 4 then .[3] + " " + .[4] else "unknown" end
          ),
          dependency: {
            package: { name: ($pkg.affectedPackage // "unknown") },
            version: (($pkg.affectedVersion // {}).fullName // "unknown")
          }
        },
        identifiers: [
          (
            {
              type:  "cve",
              name:  ($v.shortDescription // ""),
              value: ($v.shortDescription // "")
            }
            + (
              ((($v.relatedUrl // [])[0] // {}).url // "")
              | if test("^(https?|ftp)://") then {url: .} else {} end
            )
          )
        ],
        links: [($v.relatedUrl // [])[] | select(. != null) | {url}]
      }
  ]
}' > "${CI_PROJECT_DIR}/${REPORT}"

echo "==> GitLab report written to ${REPORT}"

# ---------------------------------------------------------------------------
# Cosign-vuln predicate (consumed by docker:promote_to_latest attestation).
# ---------------------------------------------------------------------------
if [ -n "${VULN_PREDICATE:-}" ]; then
  echo "==> Generating cosign-vuln predicate"
  SCAN_TS=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

  printf '%s' "${VULNS_JSON}" | jq \
    --arg image "${GCP_IMAGE}" \
    --arg ts "${SCAN_TS}" '
  {
    invocation: {
      parameters: [],
      uri:          "https://cloud.google.com/artifact-registry/docs/analysis",
      event_id:     "",
      "builder.id": "GCP On-Demand Scanning"
    },
    scanner: {
      uri:     "https://cloud.google.com/artifact-registry",
      version: "on-demand-scanning",
      result: {
        image: $image,
        vulnerabilities: [
          .[]
          | .vulnerability as $v
          | ($v.packageIssue // [{}] | .[0] // {}) as $pkg
          | {
              severity:         ($v.effectiveSeverity // "UNKNOWN"),
              id:               ($v.shortDescription // ""),
              package:          ($pkg.affectedPackage // "unknown"),
              installedVersion: (($pkg.affectedVersion // {}).fullName // ""),
              fixedVersion:     (($pkg.fixedVersion    // {}).fullName // ""),
              url:              ((($v.relatedUrl // [])[0] // {}).url  // "")
            }
        ]
      }
    },
    metadata: {
      scanStartedOn:  $ts,
      scanFinishedOn: $ts
    }
  }' > "${CI_PROJECT_DIR}/${VULN_PREDICATE}"

  echo "==> Predicate written to ${VULN_PREDICATE}"
fi

# ---------------------------------------------------------------------------
# Security gate — fail on CRITICAL or HIGH (optional).
#
# Set [SECURITY_GATE_DISABLED=true] to report findings without failing the
# job (e.g. for the scheduled security scans pipeline that should always
# publish results to the vulnerability dashboard).
# ---------------------------------------------------------------------------
if [ "${SECURITY_GATE_DISABLED:-false}" = "true" ]; then
  echo "==> Security gate disabled (SECURITY_GATE_DISABLED=true)"
  exit 0
fi

GATE_COUNT=$((CRITICAL + HIGH))
if [ "${GATE_COUNT}" -gt 0 ]; then
  echo ""
  echo "SECURITY GATE FAILED: ${CRITICAL} critical + ${HIGH} high vulnerabilities found"
  exit 1
fi

echo "==> Security gate passed (0 critical/high vulnerabilities)"
