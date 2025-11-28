#!/usr/bin/env bash

usage() {
  cat >&2 << EOF
usage: $0

Get the latest Octez EVM Node release version.

This script queries GitLab API to find the latest released version
of octez-evm-node and returns just the version number.
EOF
}

PROJECT="${PROJECT:-tezos%2Ftezos}"

function gitlab_url {
  local route="$1"

  echo -n "https://gitlab.com/api/v4/projects/$PROJECT/$route"
}

function gitlab_get {
  local route="$1"

  curl -s "$(gitlab_url "$route")"
}

function json_select {
  local obj="$1"
  local field="$2"

  shift 2

  echo -n "$obj" | jq "$@" ".$field"
}

function get_latest_version {
  local latest_release
  latest_release="$(gitlab_get "releases" | jq '[.[] | select(.tag_name | capture("^octez-evm-node-"))][0]')"
  local latest_tag
  latest_tag="$(json_select "$latest_release" "tag_name" -r)"
  local latest_version="${latest_tag#octez-evm-node-v}"

  echo "$latest_version"
}

get_latest_version
