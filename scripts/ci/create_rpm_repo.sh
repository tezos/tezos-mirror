#!/bin/sh

# Create the DNF repository for RPM packages and sign it using
# the private key available as ENV variable for production repository
# and using the file test_repo_private.key for test repositories.

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

<DISTRIBUTION>: The linux distribution, eg. rockylinux or fedora

<RELEASES>: The release of the Linux distribution, e.g. '9.3', '39'.
This argument can be repeated to build for multiple releases.

Set the ARCHITECTURES env variable of packages built for
multiple architectures are available. Eg 'amd64 arm64'
EOF
  exit 1
fi

ARCHITECTURES=${ARCHITECTURES:-"amd64"}

# The prefix used for these packages in the repository.
PREFIX=${PREFIX:-""}

# The linux distribution for which we are creating the rpm repository
# E.g. 'fedora' or 'rockylinux'
DISTRIBUTION=${1}
shift
# The release of the linux distribution for which
# we are creating the rpm repository
# E.g. '39', '9'
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
TestBranch | TestProtectedBranch)
  TARGETDIR="public/$PREFIX/$CI_COMMIT_REF_NAME/$DISTRIBUTION"
  ;;
*)
  echo "Cannot create a repository for this branch"
  exit 1
  ;;
esac

# Retrieve and import GPG keys (keyring is ready after this)
. scripts/ci/repository-keys.sh

mkdir -p "$TARGETDIR/dists"

# Import public key into RPM's own keyring (RPM-specific)
echo "$GPG_PUBLIC_KEY" > "$TARGETDIR/octez.asc"
rpm -v --import "$TARGETDIR/octez.asc"

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

    target="dists/${release}"

    mkdir -p "$TARGETDIR/${target}/"

    for file in packages/"${DISTRIBUTION}/${release}/$architecture/"*.rpm; do
      rpm --query --qf '%{NAME}-%{VERSION}-%{RELEASE}.rpm: %{SIGMD5}\n' "$file"
      printf "Sha256: %s\n" "$(sha256sum "$file")\n"

      # GPG Signature
      echo "Signing binary file: $file"
      echo "Using key: $GPG_KEY_ID"
      echo "Output: $file"
      echo "$GPG_PASSPHRASE" | rpm --define "_gpg_name $GPG_KEY_ID" \
        --define="__gpg_sign_cmd gpg --batch --passphrase-fd 0 --pinentry-mode loopback -u %{_gpg_name} --detach-sign" \
        --addsign "$file" || {
        echo "Error signing $file with key $GPG_KEY_ID"
        exit 1
      }

      # GPG dual signing
      echo "GPG dual signing: $GPG_DUAL_SIGNING"
      if [ "$GPG_DUAL_SIGNING" = "true" ]; then
        echo "Signing binary file: $file"
        echo "Using key: $GPG_KEY_ID_BIS"
        echo "Output: $file"
        echo "$GPG_PASSPHRASE_BIS" | rpm --define "_gpg_name $GPG_KEY_ID_BIS" \
          --define="__gpg_sign_cmd gpg --batch --passphrase-fd 0 --pinentry-mode loopback -u %{_gpg_name} --detach-sign" \
          --addsign "$file" || {
          echo "Error signing $file with key $GPG_KEY_ID_BIS"
          exit 1
        }
      fi
      # Signing verification
      echo "Verification:"
      rpm -Kv "$file"

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

./scripts/ci/gcp_auth.sh
GOOGLE_OAUTH_ACCESS_TOKEN=$(gcloud auth print-access-token)
export GOOGLE_OAUTH_ACCESS_TOKEN

echo "Push to $BUCKET"

gsutil -m cp -r public/* gs://"${BUCKET}"
