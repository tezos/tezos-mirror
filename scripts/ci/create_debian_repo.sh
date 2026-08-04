#!/bin/sh

# Create the APT repository for debian packages and sign it using
# the private key available as ENV variable for production repository
# and using the files test_repo_private_key.key and test_repo_private_key2.key
# for test repositories.

# uses :
# - scripts/packaging/Release.conf for release metadata

# expected env vars
# - ARCHITECTURES
# - GCP_LINUX_PACKAGES_BUCKET

# Env vars set by scripts/ci/repository-keys.sh
# - GPG_DUAL_SIGNING
# - GPG_KEY_ID
# - GPG_KEY_ID_BIS
# - GPG_PASSPHRASE
# - GPG_PASSPHRASE_BIS
# - GPG_PUBLIC_KEY

set -eu

if [ $# -lt 2 ]; then
  cat << EOF
Usage: $0 <DISTRIBUTION> <RELEASES..>

<DISTRIBUTION>: The linux distribution, eg. debian or ubuntu

<RELEASES>: The release of the Linux distribution, e.g. '22_04', '24_04', 'bookworm'.
This argument can be repeated to build for multiple releases.

Set the ARCHITECTURES env variable of packages built for
multiple architectures are available. Eg 'amd64 arm64'
EOF
  exit 1
fi

ARCHITECTURES=${ARCHITECTURES:-"amd64"}

#The prefix used for these packages in the repository. E.g. 'next'
PREFIX=${PREFIX:-""}
# The linux distribution for which we are creating the apt repository
# E.g. 'ubuntu' or 'debian'
DISTRIBUTION=${1}
shift
# The release of the linux distribution for which
# we are creating the apt repository
# E.g. '22_04 24_04', 'bookworm'
RELEASES=$*

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

oldPWD=$PWD

. ./scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  TARGETDIR="public/$PREFIX/RC/$DISTRIBUTION"
  ;;
Beta | TestBeta)
  TARGETDIR="public/$PREFIX/BETA/$DISTRIBUTION"
  ;;
Release | TestRelease | Rebuild | TestRebuild)
  TARGETDIR="public/$PREFIX/$DISTRIBUTION"
  ;;
Master)
  TARGETDIR="public/$PREFIX/master/$DISTRIBUTION"
  ;;
SoftRelease)
  TARGETDIR="public/$PREFIX/${CI_COMMIT_TAG}/$DISTRIBUTION"
  ;;
TestBranch | TestProtectedBranch)
  TARGETDIR="public/$PREFIX/$CI_COMMIT_REF_NAME/$DISTRIBUTION"
  ;;
*)
  echo "Cannot create a repository for this branch"
  exit 1
  ;;
esac

# Retrieve and import GPG keys (keyring is ready after this)
. ./scripts/ci/repository-keys.sh

# Preset a key's passphrase into gpg-agent so that batch signing with
# multiple -u flags works without --passphrase-fd (which only reads once).
# Usage: gpg_preset_passphrase <key_id> <passphrase>
gpg_preset_passphrase() {
  _key_id="$1"
  _passphrase="$2"
  _preset_cmd=$(command -v gpg-preset-passphrase || echo /usr/lib/gnupg/gpg-preset-passphrase)
  # Preset the passphrase for every keygrip of the key (primary and subkeys),
  # not just the first one. The signing key may be a dedicated signing subkey,
  # in which case gpg signs with the subkey's keygrip rather than the primary's.
  # Presetting only the primary keygrip would leave gpg-agent to prompt through
  # pinentry, which is absent in CI and fails with "signing failed: No such file
  # or directory". The whole key is protected by a single passphrase, so
  # presetting it for each keygrip is safe.
  gpg --list-keys --with-keygrip --with-colons "$_key_id" 2> /dev/null |
    grep ^grp | cut -d: -f10 | while read -r _keygrip; do
    "$_preset_cmd" --preset --passphrase "$_passphrase" "$_keygrip"
  done
}

# Set up gpg-agent with preset passphrases so all signing operations
# work in batch mode without --passphrase-fd.
echo "allow-preset-passphrase" >> "${GNUPGHOME:-$HOME/.gnupg}/gpg-agent.conf"
gpgconf --kill gpg-agent
gpgconf --launch gpg-agent
gpg_preset_passphrase "$GPG_KEY_ID" "$GPG_PASSPHRASE"
if [ "$GPG_DUAL_SIGNING" = "true" ]; then
  gpg_preset_passphrase "$GPG_KEY_ID_BIS" "$GPG_PASSPHRASE_BIS"
fi

# Signs a release directory's Release file with GPG clearsign and detached
# signature. When GPG_DUAL_SIGNING is true, both InRelease and Release.gpg
# carry signatures from both keys.
# Usage: gpg_sign_release <release_dir>
gpg_sign_release() {
  release_dir="$1"
  release_file="${release_dir}/Release"

  if [ "$GPG_DUAL_SIGNING" != "true" ]; then
    echo "Signing with key: $GPG_KEY_ID"

    # InRelease: clearsigned
    gpg --batch -u "${GPG_KEY_ID}!" --clearsign \
      -o "${release_dir}/InRelease" \
      "$release_file"

    # Release.gpg: detached signature
    gpg --batch -u "${GPG_KEY_ID}!" --detach-sign \
      -o "${release_dir}/Release.gpg" \
      "$release_file"
    return
  fi

  echo "Dual-signing with keys: $GPG_KEY_ID, $GPG_KEY_ID_BIS"

  # GPG_KEY_ID and GPG_KEY_ID_BIS are often two signing subkeys of the same
  # master key (key rotation keeps the master, rotates the signing subkey).
  # In that case a single "gpg -u KEY1 -u KEY2 --clearsign/--detach-sign"
  # call fails with "gpg: skipped: secret key already present": GnuPG's
  # multi-signer path dedupes signers by primary certificate and refuses
  # the second -u. So each key signs independently, and the two signatures
  # are combined afterwards.

  # Release.gpg: detached signatures are self-framed OpenPGP packets, so two
  # independently generated (binary, non-armored) signatures can simply be
  # concatenated into one multi-signature file.
  gpg --batch -u "${GPG_KEY_ID}!" --detach-sign \
    -o "${release_dir}/Release.gpg.1" "$release_file"
  gpg --batch -u "${GPG_KEY_ID_BIS}!" --detach-sign \
    -o "${release_dir}/Release.gpg.2" "$release_file"
  cat "${release_dir}/Release.gpg.1" "${release_dir}/Release.gpg.2" \
    > "${release_dir}/Release.gpg"

  # InRelease: clearsign with each key separately, then splice the two
  # signature packets into a single clearsign armor block. This is the
  # technique recommended by GnuPG's maintainer for combining independently
  # produced clearsign signatures:
  # https://lists.gnupg.org/pipermail/gnupg-users/2013-July/047118.html
  gpg --batch -u "${GPG_KEY_ID}!" --clearsign \
    -o "${release_dir}/InRelease.1" "$release_file"
  gpg --batch -u "${GPG_KEY_ID_BIS}!" --clearsign \
    -o "${release_dir}/InRelease.2" "$release_file"

  # Extract each signature's binary OpenPGP packet, concatenate them, and
  # re-armor. The filter keeps only the base64 body and the "=CRC24" line,
  # dropping the BEGIN/END ARMORED FILE markers, any Comment:/Version:
  # header lines, and the blank separator line (content-based, so it does
  # not depend on the exact number of header lines gpg's --enarmor emits).
  combined_sig_body=$(
    {
      sed -n '/^-----BEGIN PGP SIGNATURE-----$/,$ p' "${release_dir}/InRelease.1" | gpg --dearmor
      sed -n '/^-----BEGIN PGP SIGNATURE-----$/,$ p' "${release_dir}/InRelease.2" | gpg --dearmor
    } | gpg --enarmor | sed -e '/^-----/d' -e '/^[A-Za-z-]*:/d' -e '/^$/d'
  )
  [ -n "$combined_sig_body" ] || {
    echo "error: empty combined signature body" >&2
    exit 1
  }

  # The message header (Hash: line) and dash-escaped body are identical
  # between InRelease.1 and InRelease.2 as long as both keys use the same
  # digest algorithm; reuse InRelease.1's copy up to and including the
  # "-----BEGIN PGP SIGNATURE-----" line, then append the combined signature
  # body. Assert the assumption holds, since a future rotation to a
  # differently-typed key (e.g. RSA -> Ed25519) could pick a different
  # digest algorithm and silently produce an unverifiable signature.
  pre_sig_1=$(sed -n '1,/^-----BEGIN PGP SIGNATURE-----$/ p' "${release_dir}/InRelease.1")
  pre_sig_2=$(sed -n '1,/^-----BEGIN PGP SIGNATURE-----$/ p' "${release_dir}/InRelease.2")
  [ "$pre_sig_1" = "$pre_sig_2" ] || {
    echo "error: InRelease.1 and InRelease.2 headers differ (mismatched digest algorithm?)" >&2
    exit 1
  }

  {
    echo "$pre_sig_1"
    echo
    echo "$combined_sig_body"
    echo '-----END PGP SIGNATURE-----'
  } > "${release_dir}/InRelease"

  rm -f "${release_dir}/Release.gpg.1" "${release_dir}/Release.gpg.2" \
    "${release_dir}/InRelease.1" "${release_dir}/InRelease.2"
}

mkdir -p "$TARGETDIR/dists"

# Copying files
for release in $RELEASES; do             # unstable, 22_04, 24_04 ...
  for architecture in $ARCHITECTURES; do # amd64, arm64 ...
    echo "Setting up APT repository for $DISTRIBUTION / $release / $architecture"
    echo "targetdir: $TARGETDIR"

    # Create the apt repository root directory and copy the public key
    echo "$GPG_PUBLIC_KEY" > "$TARGETDIR/octez.asc"

    target="dists/${release}/main/binary-${architecture}"

    mkdir -p "$TARGETDIR/${target}/"

    for file in packages/"${DISTRIBUTION}/${release}"/*_"$architecture".deb; do
      cp "$file" "$TARGETDIR/${target}/"
      echo "Adding package $file to $TARGETDIR/${target}/"
    done

    # we also add the data packages that we built for
    # trixie, that are distribution independent.
    if [ -z "$PREFIX" ]; then
      for file in packages/debian/trixie/*_all.deb; do
        cp "$file" "$TARGETDIR/${target}/"
        echo "Adding data package $file to $TARGETDIR/${target}/"
      done
    fi

    cd "$TARGETDIR"
    echo "Create the Packages file $TARGETDIR/${target}/Packages"
    apt-ftparchive packages "dists/${release}" > \
      "${target}/Packages"
    gzip -k -f "${target}/Packages"
    cd -

  done

  echo "Create the Release files using a static configuration file: \
    $TARGETDIR/dists/${release}/Release"
  apt-ftparchive \
    -o APT::FTPArchive::Release::Codename="$release" \
    -o APT::FTPArchive::Release::Architectures="noarch $ARCHITECTURES" \
    -c "$oldPWD/scripts/packaging/Release.conf" release \
    "$TARGETDIR/dists/${release}/" > "$TARGETDIR/dists/${release}/Release"

  gpg_sign_release "$TARGETDIR/dists/${release}"

  # Verify GPG signatures against the exported public key. When dual-signing,
  # require 2 distinct signing (sub)keys so a Release signed once, or twice by
  # the same (sub)key, fails here instead of publishing silently: this is the
  # only guard on protected/production pipelines, which have no keyring-test job.
  if [ "$GPG_DUAL_SIGNING" = "true" ]; then
    expected_sig_count=2
  else
    expected_sig_count=1
  fi
  # Clearsigned files (e.g. InRelease) embed both data and signature, data is not needed for verification
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/InRelease" "" "$TARGETDIR/octez.asc" "InRelease signature" "$expected_sig_count"
  # Detached signatures (e.g. Release.gpg) require both the sig file and the data file.
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/Release.gpg" "$TARGETDIR/dists/${release}/Release" "$TARGETDIR/octez.asc" "Release.gpg signature" "$expected_sig_count"

  # back to base
  cd "$oldPWD"
done

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"
