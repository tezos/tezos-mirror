#!/bin/sh

# Test the octez-archive-keyring package: CI verification.
#
# Infrastructure tests that validate keyring contents, dual-signing,
# and package metadata. These validate the signing pipeline, not the
# user experience.
#
# Expects the keyring package to be available in the CI APT repository.
#
# Usage: test-keyring-ci-checks.sh <DISTRO> <RELEASE>

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=scripts/packaging/tests/deb/test-keyring-common.inc.sh
. "$SCRIPT_DIR/test-keyring-common.inc.sh"

# Bootstrap repository and install keyring
setup_keyring_test

# --- Check 1: Keyring files and contents ---
echo ""
echo "=== Check 1: Verify keyring files and key count ==="

if [ ! -f "$KEYRING" ]; then
  echo "FAIL: $KEYRING not found" >&2
  exit 1
fi
echo "PASS: $KEYRING exists"

key_count=$(gpg --no-default-keyring \
  --keyring "$KEYRING" \
  --list-keys 2> /dev/null | grep -c "^pub" || true)
# The keyring ships one public certificate per generation of the public key
# object in the GCS bucket. With production keys this is a single primary
# certificate (whose subkeys provide the dual-signing), so one key is expected;
# additional certificates only appear after a public key rotation publishes a
# new generation. Dual-signing itself is verified by the signature-count checks
# below, so here we only require the keyring to be non-empty.
if [ "$key_count" -lt 1 ]; then
  echo "FAIL: keyring should contain at least 1 key (found $key_count)" >&2
  exit 1
fi
echo "PASS: keyring contains $key_count key(s)"

# --- Check 2: Dual-signed Release.gpg and InRelease ---
# The APT repository is signed with two keys. Verify both Release.gpg
# (detached) and InRelease (clearsigned) contain valid signatures from
# BOTH keys, proving dual-signing works end-to-end.
echo ""
echo "=== Check 2: Verify dual-signed Release.gpg and InRelease ==="

release_dir=$(mktemp -d)
curl -fsSL "$REPO/$DISTRO/dists/$RELEASE/Release" -o "$release_dir/Release"
curl -fsSL "$REPO/$DISTRO/dists/$RELEASE/Release.gpg" -o "$release_dir/Release.gpg"
curl -fsSL "$REPO/$DISTRO/dists/$RELEASE/InRelease" -o "$release_dir/InRelease"

# Extract key IDs from the installed keyring (avoids hardcoding test key IDs)
keyring_key_ids=$(gpg --no-default-keyring \
  --keyring "$KEYRING" \
  --list-keys --with-colons 2> /dev/null | awk -F: '/^pub/ { print $5 }')

verify_gnupghome=$(mktemp -d)

# Verify Release.gpg (detached signature)
gpg_verify_output=$(GNUPGHOME="$verify_gnupghome" gpg --no-default-keyring \
  --keyring "$KEYRING" \
  --verify "$release_dir/Release.gpg" "$release_dir/Release" 2>&1) || true
echo "$gpg_verify_output"

sig_count=$(echo "$gpg_verify_output" | grep -c "Good signature" || true)
if [ "$sig_count" -lt 2 ]; then
  echo "FAIL: expected at least 2 signatures in Release.gpg (found $sig_count)" >&2
  exit 1
fi
echo "PASS: Release.gpg contains $sig_count valid signatures (dual-signing)"

# Match each keyring key id against the verify output with spaces stripped.
# keyring_key_ids holds primary key ids; when a key signs with a dedicated
# signing subkey, gpg reports the subkey on the "using RSA key" line and the
# primary only on the space-grouped "Primary key fingerprint:" line (e.g.
# "AB2D 2EF4 3BE1 48D3"). Stripping spaces lets the contiguous primary id match.
for key_id in $keyring_key_ids; do
  if ! echo "$gpg_verify_output" | tr -d ' ' | grep -q "$key_id"; then
    echo "FAIL: no signature from key $key_id in Release.gpg" >&2
    exit 1
  fi
  echo "PASS: Release.gpg signature from key $key_id verified"
done

# Verify InRelease (clearsigned)
inrelease_verify_output=$(GNUPGHOME="$verify_gnupghome" gpg --no-default-keyring \
  --keyring "$KEYRING" \
  --verify "$release_dir/InRelease" 2>&1) || true
echo "$inrelease_verify_output"

inrelease_sig_count=$(echo "$inrelease_verify_output" | grep -c "Good signature" || true)
if [ "$inrelease_sig_count" -lt 2 ]; then
  echo "FAIL: expected at least 2 signatures in InRelease (found $inrelease_sig_count)" >&2
  exit 1
fi
echo "PASS: InRelease contains $inrelease_sig_count valid signatures (dual-signing)"

for key_id in $keyring_key_ids; do
  if ! echo "$inrelease_verify_output" | tr -d ' ' | grep -q "$key_id"; then
    echo "FAIL: no signature from key $key_id in InRelease" >&2
    exit 1
  fi
  echo "PASS: InRelease signature from key $key_id verified"
done

rm -rf "$release_dir" "$verify_gnupghome"

# --- Check 3: Package metadata ---
echo ""
echo "=== Check 3: Verify package metadata ==="
dpkg -s octez-archive-keyring | grep -E "^(Package|Version|Architecture|Status):"
echo "PASS: package metadata verified"

# --- Check 4: Keyring reinstall is a no-op ---
# When the keyring is already installed, reinstalling should not break
# anything.
echo ""
echo "=== Check 4: Verify keyring reinstall is a no-op ==="

sources_before=$(cat /etc/apt/sources.list.d/octez.list)
apt-get install -y --reinstall octez-archive-keyring
sources_after=$(cat /etc/apt/sources.list.d/octez.list)

if [ "$sources_before" != "$sources_after" ]; then
  echo "FAIL: reinstall modified sources.list" >&2
  echo "  before: $sources_before" >&2
  echo "  after:  $sources_after" >&2
  exit 1
fi
echo "PASS: keyring reinstall left sources.list unchanged"

apt-get update
echo "PASS: apt-get update still works after reinstall"

# --- Check 5: Key rotation resilience ---
# Simulate a key rotation scenario: one key in the keyring has expired,
# but a valid rotation key is also present. APT should still succeed
# because gpgv accepts any valid signature from the keyring.
echo ""
echo "=== Check 5: Verify key rotation resilience (expired key + valid key) ==="

# Generate an expired GPG key on-the-fly using faketime so the key is
# genuinely expired (created 2 days ago with a 1-day validity).
apt-get install -y faketime

rotation_gnupghome=$(mktemp -d)
chmod 700 "$rotation_gnupghome"

cat > "$rotation_gnupghome/expired-key-params" << 'KEYPARAMS'
%no-protection
Key-Type: RSA
Key-Length: 2048
Name-Real: Expired Test Key
Name-Email: expired@test.local
Expire-Date: 1
%commit
KEYPARAMS
GNUPGHOME="$rotation_gnupghome" faketime -f '-2d' \
  gpg --batch --gen-key "$rotation_gnupghome/expired-key-params"

expired_key_id=$(GNUPGHOME="$rotation_gnupghome" gpg --list-keys --with-colons 2> /dev/null | awk -F: '/^pub/ { print $5 }')

# Extract the key's expiration timestamp (field 7 of the colon-delimited pub
# record) and render it next to the machine's current date, so the log shows
# proof that the key is genuinely expired (expiry < now) rather than just
# trusting gpg's "expired" label.
expired_key_expiry_epoch=$(GNUPGHOME="$rotation_gnupghome" gpg --list-keys --with-colons 2> /dev/null | awk -F: '/^pub/ { print $7; exit }')
expired_key_expiry_date=$(date -u -d "@${expired_key_expiry_epoch}" 2> /dev/null || echo "${expired_key_expiry_epoch}")

# Verify the key is actually expired
if GNUPGHOME="$rotation_gnupghome" gpg --list-keys 2>&1 | grep -q "expired"; then
  echo "Confirmed: test key $expired_key_id is expired"
  echo "  key expiration date (UTC): $expired_key_expiry_date"
  echo "  machine local date  (UTC): $(date -u)"
else
  echo "FAIL: test key $expired_key_id should be expired but is not" >&2
  GNUPGHOME="$rotation_gnupghome" gpg --list-keys 2>&1
  exit 1
fi

# Build a test keyring with: the expired key + one valid signing key
test_keyring_dir=$(mktemp -d)
GNUPGHOME="$rotation_gnupghome" gpg --batch --export "$expired_key_id" > "$test_keyring_dir/expired.gpg"

# Extract just the first valid key from the installed keyring
first_valid_key_id=$(echo "$keyring_key_ids" | head -1)
gpg --no-default-keyring --keyring "$KEYRING" --export "$first_valid_key_id" > "$test_keyring_dir/valid.gpg"

# Combine into a test keyring
combined_keyring="$test_keyring_dir/rotation-test.gpg"
combined_gnupghome=$(mktemp -d)
GNUPGHOME="$combined_gnupghome" gpg --batch --no-default-keyring \
  --keyring "$combined_keyring" --import "$test_keyring_dir/expired.gpg" "$test_keyring_dir/valid.gpg"

# Download Release and Release.gpg for verification
rotation_release_dir=$(mktemp -d)
curl -fsSL "$REPO/$DISTRO/dists/$RELEASE/Release" -o "$rotation_release_dir/Release"
curl -fsSL "$REPO/$DISTRO/dists/$RELEASE/Release.gpg" -o "$rotation_release_dir/Release.gpg"

# Verify: the combined keyring should accept the signature from the valid key
# even though the expired key is also present
rotation_verify=$(GNUPGHOME="$combined_gnupghome" gpg --no-default-keyring \
  --keyring "$combined_keyring" \
  --verify "$rotation_release_dir/Release.gpg" "$rotation_release_dir/Release" 2>&1) || true
echo "$rotation_verify"

if ! echo "$rotation_verify" | grep -q "Good signature"; then
  echo "FAIL: keyring with expired + valid key could not verify Release.gpg" >&2
  exit 1
fi
echo "PASS: APT verification succeeds with expired key + valid rotation key in keyring"

rm -rf "$rotation_gnupghome" "$test_keyring_dir" "$combined_gnupghome" "$rotation_release_dir"

echo ""
echo "CI verification tests passed."
