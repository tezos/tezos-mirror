#!/bin/sh

set -e

if [ -z "${TEZCAPITAL_GITHUB_TOKEN}" ]; then
  echo "TEZCAPITAL_GITHUB_TOKEN is not set"
  exit 1
fi

if [ -z "${CI_COMMIT_TAG}" ]; then
  echo "CI_COMMIT_TAG is not set"
  exit 1
fi

curl -L -X POST \
  -H "Accept: application/vnd.github+json" \
  -H "Authorization: Bearer ${TEZCAPITAL_GITHUB_TOKEN}" \
  -H "X-GitHub-Api-Version: 2022-11-28" \
  https://api.github.com/repos/tez-capital/tezos-macos-pipeline/actions/workflows/macos.yml/dispatches \
  -d "{\"ref\":\"main\",\"inputs\":{\"version\": \"${CI_COMMIT_TAG}\"}}"
