#!/bin/sh

# Build the octez-archive-keyring Debian package.
#
# On protected branches, the production public key(s) are injected from
# GPG_PUBLIC_KEY_1/GPG_PUBLIC_KEY_2 (set by repository-keys.sh) into the
# active-keys directory before building. On non-protected branches, the
# test signing keys from scripts/packaging/ are copied into active-keys/
# so that the keyring contains the same keys used to sign the test APT
# repository (otherwise apt-get update would reject the signatures).
#
# This script is designed to be called from the CI pipeline after
# repository-keys.sh has been sourced.
#
# Environment variables:
#   GPG_PUBLIC_KEY_1 - ASCII-armored public key 1 (set by repository-keys.sh)
#   GPG_PUBLIC_KEY_2 - ASCII-armored public key 2, optional (set by repository-keys.sh)
#   KEYRING_VERSION  - Version for the keyring package. If unset, it is
#                      derived from the creation date of the newest active
#                      key (YYYY.MMDD) so that APT sees a strictly newer
#                      package whenever the key set changes.
#
# Usage: build-keyring-deb.sh

set -eu

KEYRING_DIR="scripts/packaging/octez-archive-keyring"

if [ -n "${GPG_PUBLIC_KEY_1:-}" ]; then
  echo "Injecting production public key(s) into keyring package..."

  # Clear any test keys - production keys take precedence
  rm -f "${KEYRING_DIR}/active-keys/"*.asc

  echo "${GPG_PUBLIC_KEY_1}" > "${KEYRING_DIR}/active-keys/octez-signing-key-1.asc"
  if [ -n "${GPG_PUBLIC_KEY_2:-}" ]; then
    echo "${GPG_PUBLIC_KEY_2}" > "${KEYRING_DIR}/active-keys/octez-signing-key-2.asc"
  fi

  key_count=$(find "${KEYRING_DIR}/active-keys/" -name '*.asc' | wc -l)
  echo "Injected ${key_count} public key(s) into active-keys/"
else
  # No production keys: copy the test signing keys so the keyring matches
  # the keys used to sign the test APT repository. In production, the
  # production keys from GCP are different from these test keys.
  echo "No GPG_PUBLIC_KEY_1 set, copying test signing keys into active-keys/"
  rm -f "${KEYRING_DIR}/active-keys/"*.asc
  cp scripts/packaging/package-signing-key.asc "${KEYRING_DIR}/active-keys/"
  cp scripts/packaging/package-signing-key-2.asc "${KEYRING_DIR}/active-keys/"
  key_count=$(find "${KEYRING_DIR}/active-keys/" -name '*.asc' | wc -l)
  echo "Copied ${key_count} test key(s) into active-keys/"
fi

if [ "${key_count}" -eq 0 ]; then
  echo "ERROR: No public keys found in ${KEYRING_DIR}/active-keys/" >&2
  exit 1
fi

# Derive the package version from the creation date of the newest active key
# (formatted YYYY.MMDD) unless KEYRING_VERSION is set explicitly. This makes
# the version increase monotonically whenever a key is rotated in, so that
# `apt upgrade` actually picks up the new keyring; a build with an unchanged
# key set keeps the same version and produces no spurious upgrade. The version
# committed in debian/changelog is only a placeholder: it is overwritten here
# at build time.
if [ -z "${KEYRING_VERSION:-}" ]; then
  newest=0
  for key in "${KEYRING_DIR}/active-keys/"*.asc; do
    [ -e "${key}" ] || continue
    created=$(gpg --with-colons --show-keys "${key}" 2> /dev/null |
      awk -F: '$1 == "pub" { print $6 }' | sort -nr | head -n 1)
    if [ -n "${created}" ] && [ "${created}" -gt "${newest}" ]; then
      newest="${created}"
    fi
  done
  if [ "${newest}" -eq 0 ]; then
    echo "ERROR: could not determine a key creation date for versioning" >&2
    exit 1
  fi
  KEYRING_VERSION=$(date -u -d "@${newest}" +%Y.%m%d)
fi
echo "Keyring package version: ${KEYRING_VERSION}"

# If SOURCE_DATE_EPOCH is undefined, set it to the UNIX timestamp of HEAD
# (committer date). It is thus an optional parameter of the script:
# build-deb-local.sh exports it before invoking this script; standalone runs
# fall back to HEAD here. Anchoring embedded timestamps to it makes the build
# reproducible.
: "${SOURCE_DATE_EPOCH:=$(git show -s --format=%ct HEAD)}"
export SOURCE_DATE_EPOCH
DEB_DATE=$(date -u -R -d "@${SOURCE_DATE_EPOCH}")

# Update the changelog version
export DEBEMAIL="${DEBEMAIL:-contact@nomadic-labs.com}"
debchange --changelog "${KEYRING_DIR}/debian/changelog" \
  --newversion "${KEYRING_VERSION}" "Octez archive keyring update"
# Pin the changelog date for reproducibility: debchange stamps it with the
# current local time. We do not use its --date option because that was only
# added in devscripts 2.24.2 and bookworm ships 2.23.4; once we drop bookworm
# this can become `debchange --date "$DEB_DATE" ...`.
#
# Example changelog signature:
#  -- Albert Dupont <albert@dupont.com>  Wed, 24 Jul 2026 17:31:42 +0200
# This sed command looks for the first line starting with " -- " and replaces the date
# which is after the '>' character.
sed -i "0,/^ -- /s|\(^ -- .*>  \).*|\1${DEB_DATE}|" "${KEYRING_DIR}/debian/changelog"

# Build the package
cd "${KEYRING_DIR}"
DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
cd -

echo "Keyring package built successfully."
ls -la scripts/packaging/octez-archive-keyring*.deb 2> /dev/null || true
