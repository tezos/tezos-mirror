#!/usr/bin/env bash

set -ex

if [ -z "${S3_BUCKET:-}" ]; then
  echo "S3_BUCKET variable is not set, impossible to publish assets and release page."
  exit 1
fi

if [ -z "${AWS_ACCESS_KEY_ID}" ] || [ -z "${AWS_SECRET_ACCESS_KEY}" ]; then
  echo "The AWS credentials are not found. Make sure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are set."
  exit 1
fi

# Set version 23.3 as active using the version manager
dune exec ci/bin_release_page/version_manager.exe --path "site-prod.octez.tezos.com/releases" set-active --major 23 --minor 3

aws s3 cp "s3://site-prod.octez.tezos.com/releases/versions.json" "./"
cat "./versions.json"
