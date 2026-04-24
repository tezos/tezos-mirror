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

# Signs a release directory's Release file with GPG clearsign and detached
# signature. If GPG_DUAL_SIGNING is true, appends a second detached signature.
# Verifies both resulting signature files.
# Usage: gpg_sign_release <release_dir>
gpg_sign_release() {
  release_dir="$1"

  # InRelease: clearsigned (embedded signature + content), primary key only
  echo "Signing release file: ${release_dir}/InRelease"
  echo "Using key: $GPG_KEY_ID"
  echo "Output: ${release_dir}/InRelease"
  echo "$GPG_PASSPHRASE" |
    gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
      -u "$GPG_KEY_ID" --clearsign \
      -o "${release_dir}/InRelease" \
      "${release_dir}/Release"

  # Release.gpg: detached signature; BIS key is concatenated if dual-signing.
  # InRelease is not enough for dual signing as it contains only the result from
  # the newest GPG key. Detached signatures can be concatenated.
  echo "Signing release file: ${release_dir}/Release"
  echo "Using key: $GPG_KEY_ID"
  echo "Output: ${release_dir}/Release.gpg"
  echo "$GPG_PASSPHRASE" |
    gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
      -u "$GPG_KEY_ID" --detach-sign \
      -o "${release_dir}/Release.gpg" \
      "${release_dir}/Release"

  echo "GPG dual signing: $GPG_DUAL_SIGNING"
  if [ "$GPG_DUAL_SIGNING" = "true" ]; then
    echo "Signing release file: ${release_dir}/Release"
    echo "Using key: $GPG_KEY_ID_BIS"
    echo "Output: ${release_dir}/Release.gpg.bis"
    echo "$GPG_PASSPHRASE_BIS" |
      gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
        -u "$GPG_KEY_ID_BIS" --detach-sign \
        -o "${release_dir}/Release.gpg.bis" \
        "${release_dir}/Release"

    echo "Appending ${release_dir}/Release.gpg.bis to ${release_dir}/Release.gpg"
    cat "${release_dir}/Release.gpg.bis" >> "${release_dir}/Release.gpg"
    rm "${release_dir}/Release.gpg.bis"
  fi
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

  # Verify GPG signatures against the exported public key
  # Clearsigned files (e.g. InRelease) embed both data and signature, data is not needed for verification
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/InRelease" "" "$TARGETDIR/octez.asc" "InRelease signature"
  # Detached signatures (e.g. Release.gpg) require both the sig file and the data file.
  ./scripts/ci/verify_gpg_signature.sh "$TARGETDIR/dists/${release}/Release.gpg" "$TARGETDIR/dists/${release}/Release" "$TARGETDIR/octez.asc" "Release.gpg signature"

  # back to base
  cd "$oldPWD"
done

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"
