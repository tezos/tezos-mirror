#!/usr/bin/env bash

set -e

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to create a release page."
  exit 1
fi

Releases_list="${1:-}"

if [ -z "${Releases_list}" ]; then
  echo "$0 takes a Releases list as argument."
  exit 1
fi

sudo apk add pandoc

echo "# Octez Releases" >> index.md

mapfile -t releases < <(tac "$Releases_list")
latest=${releases[0]}

# Define the content of the release page
for release in "${releases[@]}"; do
  if [[ "$release" == "$latest" ]]; then
    echo "## $release (latest)" >> index.md
  else
    echo "## $release" >> index.md
  fi
  echo "### Static binaries" >> index.md
  for arch in x86_64 arm64; do
    echo "#### $arch" >> index.md

    aws s3 cp "s3://${S3_BUCKET}/${release}/binaries/${arch}/sha256sums.txt" "./sha256sums.txt"

    for binary in $(aws s3 ls "s3://${S3_BUCKET}/${release}/binaries/${arch}/" --recursive | awk '{print $NF}'); do
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

done

echo "Generating html file."
pandoc index.md -s --template="./docs/release_page/template.html" --metadata title="Octez Releases" -o index.html
