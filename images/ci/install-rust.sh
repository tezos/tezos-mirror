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
packages=("llvm19-libs-19.1.7-r5.apk" "rust-1.86.0-r0.apk" "cargo-1.86.0-r0.apk")

# Configuration
download_dest=$(mktemp -d)
arch=$(uname -m)

# Collect checksums
declare -A checksums
case "$arch" in
aarch64)
  checksums["cargo-1.86.0-r0.apk"]=420af4b02cd11cfb47687ec3315ccf792fece7897c51fdced77a1a52d12e02ec
  checksums["llvm19-libs-19.1.7-r5.apk"]=d42f719bf749cb82655208208fc92944d9268805caeee56c2c4eb0902acc0a98
  checksums["rust-1.86.0-r0.apk"]=6ece8234db203ab00895268b59cf87e4161b816549413dae3f218214f226e5d8
  ;;
x86_64)
  checksums["cargo-1.86.0-r0.apk"]=d41897fad495388f28fa83b547aae14738a3b905ae21a2d799abe79099a8cf04
  checksums["llvm19-libs-19.1.7-r5.apk"]=91f6fbb008b5bcbab0cac9f5493a12ca79a19a15d9f925c1fbf8ce51a1a4aa52
  checksums["rust-1.86.0-r0.apk"]=2e563eb81aaf2f9590ee466c6e0549ef7309e534c0d448e140afe1e835680e60
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
