#!/usr/bin/env bash

set -e

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to create a release page."
  exit 1
fi

versions_list_filename="${1:-}"

if [ -z "${versions_list_filename}" ]; then
  echo "$0 takes a Versions list as argument."
  exit 1
fi

sudo apk add pandoc jq

echo "# Octez Releases" >> index.md

# [versions] is a 2D array representation of the [$versions_list_filename] JSON:
# - one line per release or release candidate
# - each line has two elements: "[major].[minor] [rc]", [rc] is "null" if it is not a release candidate
# shellcheck disable=SC2162
mapfile -t versions < <(jq -r '[.[] | "\(.major).\(.minor) \(.rc // "null")"] | reverse | .[]' "${versions_list_filename}")

# Get the version of the latest release:
# - [release_versions] is an array representation of the [$versions_list_filename] JSON of the versions of all releases (we ignore release candidates).
# - each element of the array is a release version with the format "[major].[minor]"
# - elements of the array (i.e. versions) are separated by a whitespace
# - the latest release appears first.
# shellcheck disable=SC2162
read -a releases_versions <<< "$(jq -r '[.[] | select(has("rc") | not) | "\(.major).\(.minor)"] | reverse | join(" ")' "${versions_list_filename}")"
latest=${releases_versions[0]}

# Define the content of the release page.
# We iterate on the 2d array [$versions], distinguishing between three cases:
# - latest release (computed beforehand)
# - non-latest release
# - release candidate
while read -r version rc; do
  if [[ ${rc} != null ]]; then
    echo "## Octez Release Candidate ${version}~rc${rc}" >> index.md
    version="${version}-rc${rc}"
  else
    if [[ "$version" == "$latest" ]]; then
      echo "## Octez $version (latest)" >> index.md
    else
      echo "## Octez $version" >> index.md
    fi
  fi
  echo "### Static Binaries" >> index.md
  for arch in x86_64 arm64; do
    echo "#### $arch" >> index.md

    aws s3 cp "s3://${S3_BUCKET}/octez-v${version}/binaries/${arch}/sha256sums.txt" "./sha256sums.txt"

    for binary in $(aws s3 ls "s3://${S3_BUCKET}/octez-v${version}/binaries/${arch}/" --recursive | awk '{print $NF}'); do
      binary_name=$(basename "$binary")
      # Write sha256sum only if it's an actual binary (and not a checksums file)
      if [[ "$binary_name" != "sha256sums.txt" ]]; then
        checksum=$(grep " ${binary_name}$" sha256sums.txt | awk '{print $1}')
        echo "- [${binary_name}](https://${S3_BUCKET}/${binary}) <span class=\"sha256\">(**sha256:** \`$checksum\`)</span>" >> index.md
      else
        echo "- [${binary_name}](https://${S3_BUCKET}/${binary})" >> index.md
      fi
    done
    echo -e "\n" >> index.md
  done

  {
    echo -e "### Debian Packages\n"
    echo -e "For installation instructions, refer to the [Octez Debian Packages Guide](https://tezos.gitlab.io/introduction/howtoget.html#new-set-of-debian-packages)\n"
  } >> index.md

  {
    echo -e "### RPM Packages\n"
    echo -e "For installation instructions, refer to the [Octez RPM Packages Guide](https://tezos.gitlab.io/introduction/howtoget.html#fedora-octez-packages)\n"
  } >> index.md
done <<< "$(printf "%s\n" "${versions[@]}")"

echo "Generating html file."
pandoc index.md -s --template="./docs/release_page/template.html" --metadata title="Octez Releases" -o index.html
