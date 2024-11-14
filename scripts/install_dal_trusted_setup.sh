#!/bin/sh

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

case $1 in
--legacy)
  legacy=true
  shift
  ;;
*)
  legacy=false
  ;;
esac

if [ "$legacy" = false ]; then
  echo "If you use Octez v20.x, you should run the script with '--legacy' option"
fi

# Install DAL trusted setup.

# When executing this script within the Docker image, this variable
# might not be initialized because the binaries have already been
# compiled. This approach ensures compatibility and functionality with
# respect to an environment compiling sources with opam.
if [ -z "$OPAM_SWITCH_PREFIX" ]; then
  OPAM_SWITCH_PREFIX="./_opam"
fi

if [ -z "$DAL_TRUSTED_SETUP" ]; then
  DAL_TRUSTED_SETUP="${OPAM_SWITCH_PREFIX}/share/dal-trusted-setup"
fi

URL="https://assets.nomadic-labs.cloud/dal_trusted_setup"

echo "Installing DAL trusted setup in ${DAL_TRUSTED_SETUP}"
mkdir -p "${DAL_TRUSTED_SETUP}"

correct_sha() {
  filename="$1"
  expected="$2"

  sha_output=$(sha256sum "${filename}")
  exp_output="${expected}  ${filename}"

  if [ "${sha_output}" = "${exp_output}" ]; then
    return 0
  else
    return 1
  fi
}

download_if_necessary() {
  filename="$1"
  expected_sha="$2"
  url="$3"

  echo "Checking ${filename}"
  if [ -f "$filename" ]; then
    if ! correct_sha "${filename}" "${expected_sha}"; then
      echo "File exists, but incorrect sha, redownloading from ${url}..."
      curl -s -o "${filename}" "${url}"
      echo "done!"
      correct_sha "${filename}" "${expected_sha}"
    else
      echo "File exists, sha256 correct. All done!"
    fi
  else
    echo "File ${filename} not found. Downloading from ${url}..."
    curl -s -o "${filename}" "${url}"
    echo "done!"
    correct_sha "${filename}" "${expected_sha}"
  fi
}

# Download uncompressed g1 SRS
if [ "$legacy" = true ]; then
  download_if_necessary "${DAL_TRUSTED_SETUP}/srs_zcash_g1" "${dal_srs_g1_sha}" "${URL}/srs_filecoin_g1_21"
else
  download_if_necessary "${DAL_TRUSTED_SETUP}/srsu_zcash_g1" "${dal_srsu_g1_sha}" "${URL}/srsu_g1"
fi

# Download uncompressed g2 SRS
if [ "$legacy" = true ]; then
  download_if_necessary "${DAL_TRUSTED_SETUP}/srs_zcash_g2" "${dal_srs_g2_sha}" "${URL}/srs_filecoin_g2_21"
else
  download_if_necessary "${DAL_TRUSTED_SETUP}/srsu_zcash_g2" "${dal_srsu_g2_sha}" "${URL}/srsu_g2"
fi
