#!/bin/sh

set -eu

# Create the DNF repository for RPM packages and sign it using
# the private key available as ENV variable for production repository
# and using the file test_repo_private.key for test repositories.

# uses :
# - scripts/packaging/key.asc as the repository pub key

# expected env vars
# - ARCHITECTURES
# - GPG_PASSPHRASE
# - GPG_KEY_ID
# - GPG_PRIVATE_KEY
# - GCP_LINUX_PACKAGES_BUCKET

if [ $# -lt 2 ]; then
  cat << EOF
Usage: $0 <DISTRIBUTION> <RELEASES..>

<DISTRIBUTION>: The linux distribution, eg. rockylinux or fedora

<RELEASES>: The release of the Linux distribution, e.g. '9.3', '39'.
This argument can be repeated to build for multiple releases.

Set the ARCHITECTURES env variable of packages built for
multiple architectures are available. Eg 'amd64 arm64'
EOF
  exit 1
fi

ARCHITECTURES=${ARCHITECTURES:-"amd64"}

# The prefix used for these packages in the repository. E.g. 'next'
PREFIX=${PREFIX:-""}

# The linux distribution for which we are creating the rpm repository
# E.g. 'fedora' or 'rockylinux'
DISTRIBUTION=${1}
shift
# The release of the linux distribution for which
# we are creating the rpm repository
# E.g. '39', '9.3'
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

if [ "$CI_PROJECT_NAMESPACE" = "tezos" ] && [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then
  # the keys used for official releases only
  # These env vars are only available on tezos/tezos
  # and in protected branches
  #  GPG_KEY_ID="5DC80C4ED0B7C4FE"
  GPG_PRIVATE_KEY="$GPG_LINUX_PACKAGES_PRIVATE_KEY"
  GPG_PASSPHRASE="$GPG_LINUX_PACKAGES_PASSPHRASE"
  echo "$GPG_LINUX_PACKAGES_PUBLIC_KEY" > \
    ./scripts/packaging/package-signing-key-release.asc
  GPG_PUBLIC_KEY="./scripts/packaging/package-signing-key-release.asc"
else
  # This is strictly for testing
  # We embed these keys here for testing only.
  GPG_KEY_ID="CFC482F3CD08D36D"
  GPG_PASSPHRASE="07cde771b39a4ed394864baa46126b"
  GPG_PRIVATE_KEY=$(cat ./scripts/packaging/test_repo_private.key)
  GPG_PUBLIC_KEY="./scripts/packaging/package-signing-key.asc"
fi

export GPG_TTY=/dev/console
echo "$GPG_PRIVATE_KEY" | base64 --decode | gpg --batch --import --
GPG_EMAIL=$(gpg --list-keys --with-colons "$GPG_KEY_ID" | grep uid | cut -d':' -f10)
rpm -v --import "$GPG_PUBLIC_KEY"

mkdir -p "$TARGETDIR/dists"

# Copying files
for release in $RELEASES; do
  for architecture in $ARCHITECTURES; do # amd64, arm64 ...
    case "$architecture" in
    arm64)
      # in rpm world these are called aarch64
      architecture="aarch64"
      ;;
    amd64)
      architecture="x86_64"
      ;;
    esac

    echo "Setting up RPM repository for $DISTRIBUTION / $release / $architecture"
    echo "targetdir: $TARGETDIR"

    # Create the rpm repository root directory and copy the public key
    cp "$GPG_PUBLIC_KEY" "$TARGETDIR/octez.asc"

    target="dists/${release}"

    mkdir -p "$TARGETDIR/${target}/"

    for file in packages/"${DISTRIBUTION}/${release}/$architecture/"*.rpm; do
      rpm --query --qf '%{NAME}-%{VERSION}-%{RELEASE}.rpm: %{SIGMD5}\n' "$file"
      printf "Sha256: %s\n" "$(sha256sum "$file")\n"

      echo "$GPG_PASSPHRASE" |
        gpg --batch --passphrase-fd 0 --pinentry-mode loopback \
          -u "$GPG_KEY_ID" --detach-sign --armor \
          "$file" || {
        echo "Error signing $file"
        exit 1
      }
      rpm --define "_gpg_name $GPG_EMAIL" --resign "$file" || {
        echo "Error resigning $file"
        exit 1
      }

      rpm --query --qf '%{NAME}-%{VERSION}-%{RELEASE}.rpm: %{SIGMD5}\n' "$file"
      printf "Sha256: %s\n" "$(sha256sum "$file")\n"

      rpm --checksig "$file" || {
        echo "Signature check failed for $file"
        exit 1
      }
      cp "$file" "$TARGETDIR/${target}/"
      echo "Signing and adding package $file to $TARGETDIR/${target}/"
      echo "---------------------"
    done

  done

  echo "Create the Repository $TARGETDIR/${target}"
  createrepo_c -v --update --retain-old-md 1 "$TARGETDIR/${target}"

done

cd "$oldPWD"

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
