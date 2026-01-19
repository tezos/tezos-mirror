#!/bin/sh

set -eu

# Retrieve and set the keys for package releases
#
# we export the GPG key related env vars GPG_ with the following values:
# - Keys got from GCP Secret Manager : branch is protected
# - Keys got from ./scripts/packaging/ : branch is not protected

# expected env vars
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_KEY_ID
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY
# - GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PUBLIC_KEY

export GPG_KEY_ID=
export GPG_PASSPHRASE=
export GPG_PRIVATE_KEY=
export GPG_PUBLIC_KEY=

if [ "$CI_PROJECT_NAMESPACE" = "tezos" ] && [ "$CI_COMMIT_REF_PROTECTED" = "true" ]; then
  # the keys used for official releases only
  # These env vars are only available on tezos/tezos
  # and in protected branches

  # Enable OIDC Auth
  ./scripts/ci/gcp_get_access_token.sh

  # Get secrets from GCP Secret manager if exist
  ./scripts/ci/gcp_sm_get_secret.sh GPG_LINUX_PACKAGES_KEY_ID "$GCP_SECRET_PATH_GPG_LINUX_PACKAGES_KEY_ID"
  if [ -f "GPG_LINUX_PACKAGES_KEY_ID.secret" ]; then
    GPG_LINUX_PACKAGES_KEY_ID=$(cat GPG_LINUX_PACKAGES_KEY_ID.secret)
  fi
  ./scripts/ci/gcp_sm_get_secret.sh GPG_LINUX_PACKAGES_PASSPHRASE "$GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PASSPHRASE"
  if [ -f "GPG_LINUX_PACKAGES_PASSPHRASE.secret" ]; then
    GPG_LINUX_PACKAGES_PASSPHRASE=$(cat GPG_LINUX_PACKAGES_PASSPHRASE.secret)
  fi
  ./scripts/ci/gcp_sm_get_secret.sh GPG_LINUX_PACKAGES_PRIVATE_KEY "$GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PRIVATE_KEY"
  if [ -f "GPG_LINUX_PACKAGES_PRIVATE_KEY.secret" ]; then
    GPG_LINUX_PACKAGES_PRIVATE_KEY=$(cat GPG_LINUX_PACKAGES_PRIVATE_KEY.secret)
  fi
  ./scripts/ci/gcp_sm_get_secret.sh GPG_LINUX_PACKAGES_PUBLIC_KEY "$GCP_SECRET_PATH_GPG_LINUX_PACKAGES_PUBLIC_KEY"
  if [ -f "GPG_LINUX_PACKAGES_PUBLIC_KEY.secret" ]; then
    GPG_LINUX_PACKAGES_PUBLIC_KEY=$(cat GPG_LINUX_PACKAGES_PUBLIC_KEY.secret)
  fi
  # by default pre-configured GitLab vars are used
  export GPG_KEY_ID="${GPG_LINUX_PACKAGES_KEY_ID:-5DC80C4ED0B7C4FE}"
  export GPG_PASSPHRASE="$GPG_LINUX_PACKAGES_PASSPHRASE"
  export GPG_PRIVATE_KEY="$GPG_LINUX_PACKAGES_PRIVATE_KEY"
  echo "$GPG_LINUX_PACKAGES_PUBLIC_KEY" > \
    ./scripts/packaging/package-signing-key-release.asc
  export GPG_PUBLIC_KEY="./scripts/packaging/package-signing-key-release.asc"
else
  # ! protected
  # This is strictly for testing
  # We embed these keys here for testing only.
  export GPG_KEY_ID="24EA481996EB8138"
  export GPG_PASSPHRASE="07cde771b39a4ed394864baa46126b"
  GPG_PRIVATE_KEY=$(cat ./scripts/packaging/test_repo_private.key)
  export GPG_PRIVATE_KEY
  export GPG_PUBLIC_KEY="./scripts/packaging/package-signing-key.asc"
fi
