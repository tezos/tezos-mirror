#!/bin/sh

# Create the APT repository for debian packages and sign it using
# the private key available as ENV variable for production repository
# and using the file test_repo_private.key for test repositories.

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

  if [ "$GPG_DUAL_SIGNING" = "true" ]; then
    sign_flags="-u $GPG_KEY_ID -u $GPG_KEY_ID_BIS"
    echo "Dual-signing with keys: $GPG_KEY_ID, $GPG_KEY_ID_BIS"
  else
    sign_flags="-u $GPG_KEY_ID"
    echo "Signing with key: $GPG_KEY_ID"
  fi

  # InRelease: clearsigned
  # shellcheck disable=SC2086
  gpg --batch $sign_flags --clearsign \
    -o "${release_dir}/InRelease" \
    "${release_dir}/Release"

  # Release.gpg: detached signature
  # shellcheck disable=SC2086
  gpg --batch $sign_flags --detach-sign \
    -o "${release_dir}/Release.gpg" \
    "${release_dir}/Release"
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

    # We also add arch-independent packages (data packages and keyring)
    # that are built once for trixie but are distribution independent.
    if [ -z "$PREFIX" ]; then
      for file in packages/debian/trixie/*_all.deb; do
        cp "$file" "$TARGETDIR/${target}/"
        echo "Adding arch-independent package $file to $TARGETDIR/${target}/"
      done
    fi

    cd "$TARGETDIR"
    echo "Create the Packages file $TARGETDIR/${target}/Packages"
    apt-ftparchive packages "dists/${release}" > \
      "${target}/Packages"
    # -n: omit the mtime/name from the gzip header so an unchanged Packages
    # produces a byte-identical Packages.gz across publishes. This keeps the
    # index hash (and thus its by-hash path) stable when the content is
    # unchanged, avoiding spurious in-place rewrites and extra by-hash blobs.
    gzip -n -k -f "${target}/Packages"
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

  # Verify GPG signatures against the exported public key
  # Clearsigned files (e.g. InRelease) embed both data and signature, data is not needed for verification
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/InRelease" "" "$TARGETDIR/octez.asc" "InRelease signature"
  # Detached signatures (e.g. Release.gpg) require both the sig file and the data file.
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/Release.gpg" "$TARGETDIR/dists/${release}/Release" "$TARGETDIR/octez.asc" "Release.gpg signature"

  # Publish indices under by-hash/ (content-addressed) so clients fetch them
  # via immutable paths. Done after Release so by-hash is not listed in it.
  for architecture in $ARCHITECTURES; do
    byhash_target="dists/${release}/main/binary-${architecture}"
    for index in Packages Packages.gz; do
      index_path="$TARGETDIR/${byhash_target}/${index}"
      # Skip if this index was not produced (e.g. no packages for this arch).
      [ -f "$index_path" ] || continue
      # Each "spec" packs two values "<DIR>:<command>"; apt's by-hash layout
      # uses an upper-case dir name (SHA256/SHA512) while the matching tool is
      # lower-case (sha256sum/sha512sum). "${spec%%:*}" keeps the part before
      # the ":" (the dir), "${spec#*:}" keeps the part after it (the command).
      for spec in SHA256:sha256sum SHA512:sha512sum; do
        field="${spec%%:*}"
        # Run the checksum tool and keep only the hash (cut drops the filename
        # that *sum prints as the second space-separated column).
        sum=$("${spec#*:}" "$index_path" | cut -d' ' -f1)
        # The blob's name *is* its hash, so its path never changes once written.
        mkdir -p "$TARGETDIR/${byhash_target}/by-hash/${field}"
        cp "$index_path" "$TARGETDIR/${byhash_target}/by-hash/${field}/${sum}"
      done
    done
  done

  # back to base
  cd "$oldPWD"
done

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

# Upload content first, then the signed Release files last, so a client never
# sees a new Release before the indices it references. No -d: keep old blobs.
#
# Phase 1: mirror everything EXCEPT the signed Release files. "-x" excludes
# paths matching the regex (the indices and packages are uploaded here).
gsutil -m rsync -r -x '.*/(InRelease|Release|Release\.gpg)$' public "gs://${BUCKET}"

# Phase 2: upload the signed Release files that were excluded above.
# "find ... -exec sh -c '...' sh <args> {} +" runs one shell with the matched
# files appended as arguments; inside it, $1 is "$BUCKET" (saved to "bucket",
# then dropped with "shift") and the remaining "$@" are the files. For each
# file, "${f#public/}" strips the local "public/" prefix to get its path
# inside the bucket.
find public -type f \( -name InRelease -o -name Release -o -name Release.gpg \) \
  -exec sh -c 'bucket=$1; shift; for f do gsutil cp "$f" "gs://${bucket}/${f#public/}"; done' \
  sh "$BUCKET" {} +
