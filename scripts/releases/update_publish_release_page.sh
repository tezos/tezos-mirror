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

echo "[]" > versions.json

aws s3 cp "./versions.json" "s3://site-prod.octez.tezos.com/releases/grafazos/"
aws s3 cp "./versions.json" "s3://site-prod.octez.tezos.com/releases/teztale/"
aws s3 cp "./versions.json" "s3://site-prod.octez.tezos.com/releases/octez-smart-rollup-node/"
