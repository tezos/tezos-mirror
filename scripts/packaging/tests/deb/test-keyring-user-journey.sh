#!/bin/sh

# Test the octez-archive-keyring package: user journey.
#
# Reproduces what an end-user follows from the documentation
# (docs/introduction/install-bin-deb.sh):
#   1. Bootstrap the repository with a manual key download
#   2. Install the octez-archive-keyring package
#   3. Switch APT to use the managed keyring (sed one-liner)
#   4. Install a package to prove end-to-end authentication works
#
# Usage: test-keyring-user-journey.sh <DISTRO> <RELEASE>
#
# Local execution:
#   The test runs inside a systemd-enabled Docker container against the APT
#   repository published for the current branch, so it requires that the
#   apt-repo build job has already populated the bucket for your branch
#   (CI_COMMIT_REF_NAME). From the repository root, run it via the systemd
#   Docker harness:
#
#     DISTRIBUTION=debian RELEASE=bookworm \
#       scripts/packaging/tests/systemd-docker-test.sh \
#       scripts/packaging/tests/deb/test-keyring-user-journey.sh \
#       images/packages/debian-systemd-tests.Dockerfile
#
#   systemd-docker-test.sh defaults CI_COMMIT_REF_NAME to the current branch
#   and GCP_LINUX_PACKAGES_BUCKET to tezos-linux-repo; override either to point
#   at a different published repository. Pass test-keyring.sh instead of this
#   file to run the user journey and CI verification suites together.

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=scripts/packaging/tests/deb/test-keyring-common.inc.sh
. "$SCRIPT_DIR/test-keyring-common.inc.sh"

#########################################
# PART 1: USER JOURNEY                  #
#########################################

echo ""
echo "=========================================="
echo "  PART 1: User journey (docs flow)        "
echo "=========================================="

# Steps 1-3: Bootstrap, install keyring, switch signed-by
setup_keyring_test

# Verify the managed keyring is now in sources.list
if ! grep -q "signed-by=$KEYRING" /etc/apt/sources.list.d/octez.list; then
  echo "FAIL: sources.list does not use managed keyring after setup" >&2
  cat /etc/apt/sources.list.d/octez.list >&2
  exit 1
fi
echo "PASS: sources.list uses managed keyring"

# Verify apt-get update actually authenticates the repository through the
# managed keyring. apt-get update exits 0 even when signature verification
# fails (it only warns and reuses the stale index), so we must inspect its
# output. This catches keyring-format problems such as a keybox file that
# gpgv tolerates but Sequoia's sqv (APT's verifier on trixie) cannot parse.
echo ""
echo "=== Verify apt-get update authenticates via the managed keyring ==="
keyring_update_log=$(LC_ALL=C apt_get update 2>&1)
echo "$keyring_update_log"
if echo "$keyring_update_log" | grep -Eiq \
  'signature verification|not updated|returned an error|NO_PUBKEY|failed to parse keyring|Failed to fetch.*InRelease'; then
  echo "FAIL: apt-get update could not verify the repository using the managed keyring" >&2
  exit 1
fi
echo "PASS: apt-get update verified the repository using the managed keyring"

# --- Step 4: Install a package using the keyring ---
echo ""
echo "=== Step 4: Install package with keyring ==="
apt_get install -y octez-client
octez-client --version
echo "PASS: octez-client installed successfully using keyring"

#########################################
# CLEANUP                               #
#########################################

echo ""
echo "=== Cleanup: verify purge ==="

apt-get autopurge -y octez-client
apt-get autopurge -y octez-archive-keyring

if dpkg -s octez-archive-keyring > /dev/null 2>&1; then
  echo "WARN: octez-archive-keyring still installed after purge"
else
  echo "PASS: octez-archive-keyring removed successfully"
fi

#########################################
# RECOMMENDS TEST                       #
#########################################
# Verify that installing octez-client pulls in the keyring
# package automatically via Recommends (APT installs Recommends by default).

echo ""
echo "=========================================="
echo "  Recommends auto-install                 "
echo "=========================================="

# Revert sources.list to bootstrap key for a clean Recommends test
sudo sed -i "s|signed-by=$KEYRING|signed-by=/etc/apt/keyrings/octez.gpg|" \
  /etc/apt/sources.list.d/octez.list

apt_get update

echo "=== Installing octez-client (should pull keyring via Recommends) ==="
apt_get install -y octez-client

if dpkg -s octez-archive-keyring > /dev/null 2>&1; then
  echo "PASS: octez-archive-keyring was pulled in via Recommends"
else
  echo "FAIL: octez-archive-keyring was NOT installed via Recommends" >&2
  exit 1
fi

# Switch to managed keyring (as the user would do manually)
sudo sed -i "s|signed-by=/etc/apt/keyrings/octez.gpg|signed-by=$KEYRING|" \
  /etc/apt/sources.list.d/octez.list
apt_get update
echo "PASS: apt-get update works after Recommends-driven keyring install"

# Final cleanup
apt-get autopurge -y octez-client

echo ""
echo "User journey tests passed."
