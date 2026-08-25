#!/bin/bash

set -e

# This mechanism allows us to install fixed versions of Rust and Cargo which are not shipped with
# mainline Alpine releases.
#
# It effectively goes through 3 steps:
#   1. Download the Alpine packages (.apk files)
#   2. Verify the integrity of these files
#   3. Install the packages using Alpine's package manager

# We have uploaded the packages to Nomadic Labs' S3 bucket. This is not strictly necessary, you
# could upload new packages anywhere.
packages_base_url="https://nl-public-area.s3.us-east-1.amazonaws.com/rust"

# These are the packages that we want to install
packages=("llvm22-libs-22.1.1-r0.apk" "rust-1.94.0-r0.apk" "cargo-1.94.0-r0.apk")

# Configuration
download_dest=$(mktemp -d)
arch=$(uname -m)

# Collect checksums
declare -A checksums
case "$arch" in
aarch64)
  checksums["cargo-1.94.0-r0.apk"]=9852cb6a81c337e60173406d06b22fa4f0a9f6637dd43e1f4e1eb6669570d535
  checksums["llvm22-libs-22.1.1-r0.apk"]=c23cf327daecb34ddbaa4b351109a1320b0b41d0bf79c06a0d28d6cb41fb4e37
  checksums["rust-1.94.0-r0.apk"]=611cf4d7c7e6d42be0962ce0f615739aecc90f55dac8da7aa1674838f811e3cb
  ;;
x86_64)
  checksums["cargo-1.94.0-r0.apk"]=80b6b449436c792fe9ff9ccb30f3145392aa5f2202ed7f73f450d0ed6f8ea87c
  checksums["llvm22-libs-22.1.1-r0.apk"]=e218643f457216011c7e4df5d1512293ea9045879fe3a84f1f7743b0b44f68d2
  checksums["rust-1.94.0-r0.apk"]=7e285531649611abdf80e8710937ffe3ce9eb49ab887b1ef32d9276608fa60a2
  ;;
*)
  echo "Unsupported architecture: $arch"
  exit 1
  ;;
esac

KISSCURL=${OPAMFETCH:-curl}

# Download the artefacts
mkdir -p "$download_dest"
for package in "${packages[@]}"; do
  url="$packages_base_url/$arch/$package"

  # Download
  $KISSCURL -S -s --output-dir "$download_dest" -o "$package" "$url"

  # Verify
  echo "${checksums[$package]} $download_dest/$package" | sha256sum -c
done

# Install the packages
apk add "${packages[@]/#/$download_dest/}"

# Clean up the download folder
rm -rf "$download_dest"
