#!/bin/sh

# Test the octez-archive-keyring package.
#
# This script runs two test suites in sequence:
#   1. User journey — end-user installation flow
#   2. CI verification — infrastructure checks
#
# The sub-scripts live next to this file (resolved via SCRIPT_DIR); the
# systemd Docker harness (scripts/packaging/tests/systemd-docker-test.sh)
# mounts this directory into the container so they are available at runtime.
#
# Usage: test-keyring.sh <DISTRO> <RELEASE>
#
# Local execution: see the "Local execution" note in
# test-keyring-user-journey.sh for how to run these tests via the systemd
# Docker harness against a published APT repository.

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=========================================="
echo "  Running keyring user journey tests      "
echo "=========================================="
"$SCRIPT_DIR/test-keyring-user-journey.sh" "$@"

echo ""
echo "=========================================="
echo "  Running keyring CI verification tests   "
echo "=========================================="
"$SCRIPT_DIR/test-keyring-ci-checks.sh" "$@"

echo ""
echo "All keyring tests passed."
