#!/bin/sh

# Verify a GPG signature against a data file using an isolated keyring.
# Handles both detached signatures and clearsigned files automatically.
#
# Usage: verify_gpg_signature.sh <sig_file> <data_file> <public_key_file> [<label>]
#   sig_file:        path to the signature file (detached .gpg/.asc, or clearsigned InRelease)
#   data_file:       path to the signed file; pass "" for clearsigned files (not needed)
#   public_key_file: path to the ASCII-armored public key file
#   label:           optional label for messages (default: "Signature")

set -eu

echo "##############################" >&2
echo "# GPG Signature Verification #" >&2
echo "##############################" >&2

sig_file="${1:?Usage: $0 <sig_file> <data_file> <public_key_file> [<label>]}"
data_file="${2:-}"
public_key_file="${3:?Usage: $0 <sig_file> <data_file> <public_key_file> [<label>]}"
label="${4:-Signature}"

tmp_gnupghome=$(mktemp -d)
trap 'rm -rf "${tmp_gnupghome}"' EXIT INT TERM

GNUPGHOME="${tmp_gnupghome}" gpg --import "${public_key_file}" 2> /dev/null

# Clearsigned files (e.g. InRelease) embed both data and signature: gpg --verify
# takes only the signed file as argument.
# Detached signatures (e.g. Release.gpg) require both the sig file and the data file.
if grep -q "^-----BEGIN PGP SIGNED MESSAGE-----" "${sig_file}"; then
  gpg_verify() { GNUPGHOME="${tmp_gnupghome}" gpg --verify "${sig_file}"; }
else
  if [ -z "${data_file}" ]; then
    echo "ERROR: data_file is required for detached signatures" >&2
    exit 1
  fi
  gpg_verify() { GNUPGHOME="${tmp_gnupghome}" gpg --verify "${sig_file}" "${data_file}"; }
fi

if gpg_verify; then
  echo "${label} verified successfully." >&2
else
  echo "ERROR: ${label} verification failed" >&2
  exit 1
fi
