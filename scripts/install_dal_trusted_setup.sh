#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# Install DAL trusted setup.

# When executing this script within the Docker image, this variable
# might not be initialized because the binaries have already been
# compiled. This approach ensures compatibility and functionality with
# respect to an environment compiling sources with opam.
if [ -z "$OPAM_SWITCH_PREFIX" ]; then
  OPAM_SWITCH_PREFIX="./_opam"
fi

DAL_TRUSTED_SETUP="${OPAM_SWITCH_PREFIX}/share/dal-trusted-setup"
URL="https://assets.nomadic-labs.cloud/dal_trusted_setup"

echo "Installing DAL trusted setup in ${DAL_TRUSTED_SETUP}"
rm -rf "${DAL_TRUSTED_SETUP}"
mkdir -p "${DAL_TRUSTED_SETUP}"

curl -s -o "${DAL_TRUSTED_SETUP}"/srs_zcash_g1 "${URL}"/srs_filecoin_g1_21
curl -s -o "${DAL_TRUSTED_SETUP}"/srs_zcash_g2 "${URL}"/srs_filecoin_g2_21
