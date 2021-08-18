#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=version.sh
. "$script_dir"/version.sh

# Install Sapling parameters.
ZCASH_PARAMS="${OPAM_SWITCH_PREFIX}/share/zcash-params"
OUTPUT_PARAMETERS="${opam_repository_url}/-/raw/${opam_repository_tag}/zcash-params/sapling-output.params"
SPEND_PARAMETERS="${opam_repository_url}/-/raw/${opam_repository_tag}/zcash-params/sapling-spend.params"

echo "Installing Sapling parameters in ${ZCASH_PARAMS}"
rm -rf "${ZCASH_PARAMS}"
mkdir -p "${ZCASH_PARAMS}"
curl -s -o ${ZCASH_PARAMS}/sapling-output.params ${OUTPUT_PARAMETERS}
curl -s -o ${ZCASH_PARAMS}/sapling-spend.params ${SPEND_PARAMETERS}
