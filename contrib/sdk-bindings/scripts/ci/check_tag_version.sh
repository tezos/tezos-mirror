#!/bin/sh
#
# Check that the version of the commit tag matches the version of the
# 'tezos-bindings' Rust package.
#
# Reads the following environment variables:
#  - 'CI_COMMIT_TAG': set by GitLab CI

set -eu

if [ -z "$CI_COMMIT_TAG" ]; then
  echo "No commit tag found. CI_COMMIT_TAG is null."
  exit 1
fi

if [ -z "$CI_PROJECT_DIR" ]; then
  echo "No project root in CI. CI_PROJECT_DIR is null."
  exit 1
fi

# This does not fail because the tag format has already been checked
# to launch the `publish_sdk_bindings_releases` pipeline.
TAG_VERSION=$(echo "$CI_COMMIT_TAG" | sed -nE 's/^tezos-sdk-v([0-9]+\.[0-9]+\.[0-9]+)$/\1/p')

CARGO_TOML=$CI_PROJECT_DIR/rust/Cargo.toml
PACKAGE_VERSION=$(grep '^version *= *"' "$CARGO_TOML" | head -n1 | sed 's/.*version *= *"\([^"]*\)".*/\1/')

if [ "$TAG_VERSION" != "$PACKAGE_VERSION" ]; then
  echo "Commit tag version ($TAG_VERSION) does not match 'tezos-bindings' Rust package version ($PACKAGE_VERSION)."
  exit 1
fi
