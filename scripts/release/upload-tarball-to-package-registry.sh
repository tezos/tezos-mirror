#!/bin/sh

# This script upload the git archive used by opam packages to
# the package registry

set -eu

TARBALL="tezos-$CI_COMMIT_SHORT_SHA.tgz"

# Create .tag.gz archive and upload it
git archive "$CI_COMMIT_SHORT_SHA" -o "$TARBALL"
curl --header "JOB-TOKEN: $CI_JOB_TOKEN" \
     --upload-file "$TARBALL" \
     "$PACKAGE_REGISTRY_URL/$TARBALL"

release-cli create \
  --tag-name "tarball-$CI_COMMIT_SHORT_SHA" \
  --name "Release source $CI_COMMIT_SHORT_SHA" "$TARBALL"
