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

# Define the content of the release page
tac "$Releases_list" | while IFS= read -r release; do
  echo "## $release" >> index.md
  echo "### Static binaries" >> index.md
  for arch in x86_64 arm64; do
    echo "#### $arch" >> index.md

    for binary in $(aws s3 ls "s3://${S3_BUCKET}/${release}/binaries/${arch}/" --recursive | awk '{print $NF}'); do
      echo "- [$(basename "$binary")](https://${S3_BUCKET}/${binary})" >> index.md
    done
    echo -e "\n" >> index.md
  done

  {
    echo -e "### Debian Packages\n"
    echo -e "For installation instructions, refer to the [Octez Debian Packages Guide](https://tezos.gitlab.io/introduction/howtoget.html#new-set-of-debian-packages)\n"
  } >> index.md

  echo -e "### RPM packages\n" >> index.md

  for distribution in fedora:39 rockylinux:9.3; do

    echo "#### $distribution" >> index.md

    for package in $(aws s3 ls "s3://${S3_BUCKET}/${release}/rpm/${distribution}/" --recursive | awk '{print $NF}'); do
      echo "- [$(basename "$package")](https://${S3_BUCKET}/${package})" >> index.md
    done
    echo -e "\n" >> index.md
  done
done

echo "Generating html file."
pandoc index.md -s --template="./docs/release_page/template.html" --metadata title="Octez Releases" -o index.html
