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
packages=("llvm20-libs-20.1.8-r0.apk" "rust-1.88.0-r0.apk" "cargo-1.88.0-r0.apk")

# Configuration
download_dest=$(mktemp -d)
arch=$(uname -m)

# Collect checksums
declare -A checksums
case "$arch" in
aarch64)
  checksums["cargo-1.88.0-r0.apk"]=d54fa563e6d20aa922384006ee8fb62d058659b5c14331c9ea9417823dc945b0
  checksums["llvm20-libs-20.1.8-r0.apk"]=f0ffae0f10ae7f93b4a0da8951a4ba036dec28cb4952a5b6f23d54c6ee0e0977
  checksums["rust-1.88.0-r0.apk"]=95391e610a4ea9f15889d65d13eaa9440c881fd1df3fc2f3cf88e0601ed327c4
  ;;
x86_64)
  checksums["cargo-1.88.0-r0.apk"]=1096f0eb49707d1955bee5b70c1c05a0a6eb35e1b766c09b03cf2befab0656d1
  checksums["llvm20-libs-20.1.8-r0.apk"]=14628e2e8650001e5f6d139218a67ee5e02001b8984c995d300240d7b4393c65
  checksums["rust-1.88.0-r0.apk"]=268f2dd4c98d4c250ecf8daf7b6f9efdb96e059bd354fbe84694a995bc8bd82f
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
