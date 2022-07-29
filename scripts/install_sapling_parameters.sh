#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# Install Sapling parameters.
ZCASH_PARAMS="${OPAM_SWITCH_PREFIX}/share/zcash-params"
OUTPUT_PARAMETERS_URL="${opam_repository_url}/-/raw/${opam_repository_tag}/zcash-params/sapling-output.params"
OUTPUT_PARAMETERS_FILE=${ZCASH_PARAMS}/sapling-output.params
SPEND_PARAMETERS_URL="${opam_repository_url}/-/raw/${opam_repository_tag}/zcash-params/sapling-spend.params"
SPEND_PARAMETERS_FILE=${ZCASH_PARAMS}/sapling-spend.params

echo "Installing Sapling parameters in ${ZCASH_PARAMS}"
mkdir -p "${ZCASH_PARAMS}"

if ! echo "${sapling_output_parameters_sha256}  ${OUTPUT_PARAMETERS_FILE}" | sha256sum --check  > /dev/null 2>&1 ; then
    echo "Downloading ${OUTPUT_PARAMETERS_URL}..."
    rm -f "${OUTPUT_PARAMETERS_FILE}"
    curl -s -o "${OUTPUT_PARAMETERS_FILE}" "${OUTPUT_PARAMETERS_URL}"
    if ! echo "${sapling_output_parameters_sha256}  ${OUTPUT_PARAMETERS_FILE}" | sha256sum --check  > /dev/null 2>&1 ; then
        echo "Unexpected sha256 for ${OUTPUT_PARAMETERS_FILE}."
        exit 1
    fi
else
    echo "File 'sapling-output.params' is already installed."
fi

if ! echo "${sapling_spend_parameters_sha256}  ${SPEND_PARAMETERS_FILE}" | sha256sum --check  > /dev/null 2>&1 ; then
    echo "Downloading ${SPEND_PARAMETERS_URL}..."
    rm -f "${SPEND_PARAMETERS_FILE}"
    curl -s -o "${SPEND_PARAMETERS_FILE}" "${SPEND_PARAMETERS_URL}"
    if ! echo "${sapling_spend_parameters_sha256}  ${SPEND_PARAMETERS_FILE}" | sha256sum --check  > /dev/null 2>&1 ; then
        echo "Unexpected sha256 for ${SPEND_PARAMETERS_FILE}."
        exit 1
    fi
else
    echo "File 'sapling-spend.params' is already installed."
fi
