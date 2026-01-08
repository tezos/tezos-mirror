#!/bin/sh

set -eu

# Create the APT repository for debian packages and sign it using
# the private key available as ENV variable for production repository
# and using the file test_repo_private.key for test repositories.

# uses :
# - scripts/packaging/Release.conf for release metadata
# - scripts/packaging/key.asc as the repository pub key

# expected env vars
# - ARCHITECTURES
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_KEY_ID
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PUBLIC_KEY
# - GCP_LINUX_PACKAGES_BUCKET

if [ $# -lt 2 ]; then
  cat << EOF
Usage: $0 <DISTRIBUTION> <RELEASES..>

<DISTRIBUTION>: The linux distribution, eg. debian or ubuntu

<RELEASES>: The release of the Linux distribution, e.g. 'jammy', 'noble', 'bookworm'.
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
# E.g. 'jammy noble', 'bookworm'
RELEASES=$*

# If it's a protected branch the value of $bucket will
# be set accordingly but the CI.
BUCKET="$GCP_LINUX_PACKAGES_BUCKET"

oldPWD=$PWD

. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate)
  TARGETDIR="public/$PREFIX/RC/$DISTRIBUTION"
  ;;
Beta | TestBeta)
  TARGETDIR="public/$PREFIX/BETA/$DISTRIBUTION"
  ;;
Release | TestRelease)
  TARGETDIR="public/$PREFIX/$DISTRIBUTION"
  ;;
Master)
  TARGETDIR="public/$PREFIX/master/$DISTRIBUTION"
  ;;
SoftRelease)
  TARGETDIR="public/$PREFIX/${CI_COMMIT_TAG}/$DISTRIBUTION"
  ;;
TestBranch)
  TARGETDIR="public/$PREFIX/$CI_COMMIT_REF_NAME/$DISTRIBUTION"
  ;;
*)
  echo "Cannot create a repository for this branch"
  exit 1
  ;;
esac

# Retrieve GPG keys for repository
. scripts/ci/repository-keys.sh

echo "$GPG_PRIVATE_KEY" | base64 --decode | gpg --batch --import --

mkdir -p "$TARGETDIR/dists"

# Copying files
for release in $RELEASES; do             # unstable, jammy, noble ...
  for architecture in $ARCHITECTURES; do # amd64, arm64 ...
    echo "Setting up APT repository for $DISTRIBUTION / $release / $architecture"
    echo "targetdir: $TARGETDIR"

    # create the apt repository root directory and copy the public key
    cp "$GPG_PUBLIC_KEY" "$TARGETDIR/octez.asc"

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

  # sign the release file using GPG. Since gpg is run in a script we need to set
  # some variables and extra options to make it work. The InRelease file contains
  # both the gpg signature and the content of the Release file.
  echo "Sign the release file: $TARGETDIR/dists/${release}/InRelease"
  echo "$GPG_PASSPHRASE" |
    gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
      -u "$GPG_KEY_ID" --clearsign \
      -o "$TARGETDIR/dists/${release}/InRelease" \
      "$TARGETDIR/dists/${release}/Release"

  # back to base
  cd "$oldPWD"
done

if [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then
  echo "### Logging into protected repo ..."
  echo "${GCP_PROTECTED_SERVICE_ACCOUNT}" | base64 -d > protected_sa.json
  gcloud auth activate-service-account --key-file=protected_sa.json
else
  echo "### Logging into standard repo ..."
  # Nothing to do
fi

GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"
