#!/bin/sh

# Verify a GPG signature against a data file using an isolated keyring.
# Handles both detached signatures and clearsigned files automatically.
#
# Usage: verify_gpg_signature.sh <sig_file> <data_file> <public_key_file> [<label>] [<expected_key_count>]
#   sig_file:            path to the signature file (detached .gpg/.asc, or clearsigned InRelease)
#   data_file:            path to the signed file; pass "" for clearsigned files (not needed)
#   public_key_file:      path to the ASCII-armored public key file
#   label:                optional label for messages (default: "Signature")
#   expected_key_count:   optional minimum number of distinct signing (sub)keys
#                         required (default: 1). Pass 2 when dual-signing, so a
#                         Release signed once, or twice by the same (sub)key,
#                         fails instead of publishing silently.

set -eu

echo "##############################" >&2
echo "# GPG Signature Verification #" >&2
echo "##############################" >&2

sig_file="${1:?Usage: $0 <sig_file> <data_file> <public_key_file> [<label>] [<expected_key_count>]}"
data_file="${2:-}"
public_key_file="${3:?Usage: $0 <sig_file> <data_file> <public_key_file> [<label>] [<expected_key_count>]}"
label="${4:-Signature}"
expected_key_count="${5:-1}"

tmp_gnupghome=$(mktemp -d)
trap 'rm -rf "${tmp_gnupghome}"' EXIT INT TERM

GNUPGHOME="${tmp_gnupghome}" gpg --import "${public_key_file}" 2> /dev/null

# --status-fd 1 emits GnuPG's machine-readable status protocol on stdout
# (stable across locales/gpg versions), while the human-readable diagnostics
# (localized "Good signature" text etc.) keep going to stderr and stream to
# the log as before. Capturing only stdout lets us count signatures reliably
# without depending on locale-specific message text.
# Clearsigned files (e.g. InRelease) embed both data and signature: gpg --verify
# takes only the signed file as argument.
# Detached signatures (e.g. Release.gpg) require both the sig file and the data file.
if grep -q "^-----BEGIN PGP SIGNED MESSAGE-----" "${sig_file}"; then
  gpg_verify() { GNUPGHOME="${tmp_gnupghome}" gpg --status-fd 1 --verify "${sig_file}"; }
else
  if [ -z "${data_file}" ]; then
    echo "ERROR: data_file is required for detached signatures" >&2
    exit 1
  fi
  gpg_verify() { GNUPGHOME="${tmp_gnupghome}" gpg --status-fd 1 --verify "${sig_file}" "${data_file}"; }
fi

if gpg_status_output=$(gpg_verify); then
  echo "${label} verified successfully." >&2
else
  echo "ERROR: ${label} verification failed" >&2
  exit 1
fi

# gpg --verify exiting 0 only proves every signature present is valid, not
# how many there are: a Release signed once, or signed twice by the same
# (sub)key (e.g. a dual-signing "!" pin silently dropped), still exits 0.
# Each signature emits one VALIDSIG status line carrying the fingerprint of
# the (sub)key that produced it; dedupe those to count distinct signers.
# This is the only guard on protected/production publish pipelines, which
# have no keyring-test job.
signing_key_count=$(echo "${gpg_status_output}" |
  awk '/^\[GNUPG:\] VALIDSIG/ {print $3}' | sort -u | grep -c . || true)
if [ "${signing_key_count}" -lt "${expected_key_count}" ]; then
  echo "ERROR: ${label} expected >= ${expected_key_count} distinct signing (sub)keys, found ${signing_key_count}" >&2
  exit 1
fi
echo "${label}: confirmed ${signing_key_count} distinct signing (sub)key(s) (expected >= ${expected_key_count})." >&2
