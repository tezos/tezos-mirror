#!/usr/bin/env bash

usage() {
  cat >&2 << EOF
usage: $0 [ARCHITECTURE] <VERSION>

Build Docker image for Octez EVM Node VERSION release.

This script downloads the specified VERSION of octez-evm-node binaries
from GitLab releases and builds a Docker image for the given architecture.
EOF
}

PROJECT="${PROJECT:-tezos%2Ftezos}"
ARCH="${1:-x86_64}"
VERSION="${2}"
WORKDIR="$(mktemp -d -p .)"
BASE_IMAGE="${BASE_IMAGE:-alpine:3.22}"
trap 'rm -rf $WORKDIR' EXIT
DOCKER_PLATFORM=""

if [ -z "$VERSION" ]; then
  usage
  exit 1
fi

case "$ARCH" in
"x86_64")
  DOCKER_PLATFORM="amd64"
  ;;
"arm64")
  DOCKER_PLATFORM="arm64"
  ;;
*)
  echo "Unsupported architecture $ARCH"
  exit 2
  ;;
esac

function gitlab_url {
  local route="$1"

  echo -n "https://gitlab.com/api/v4/projects/$PROJECT/$route"
}

function gitlab_get {
  local route="$1"

  curl -s "$(gitlab_url "$route")"
}

function docker_tag {
  local repo="$1"
  local image="$2"
  local tag="$3"

  curl -s "https://hub.docker.com/v2/repositories/$repo/$image/tags/$tag"
}

function next_docker_tag {
  local repo="$1"
  local image="$2"
  local version="$3"
  local first_release
  first_release="$(docker_tag "$repo" "$image" "$version" | jq -e '.id')"
  echo "first_release: $?"

  if [ "$first_release" = "null" ]; then
    echo ""
  else
    local rev="1"

    while true; do
      local release
      release="$(docker_tag "$repo" "$image" "$version-$rev" | jq -e '.id')"
      echo "rev $rev: $?"

      if [ "$release" = "null" ]; then
        echo "-$rev"
        break
      fi

      rev="$rev + 1"
    done
  fi
}

function json_select {
  local obj="$1"
  local field="$2"

  shift 2

  echo -n "$obj" | jq "$@" ".$field"
}

function fetch {
  local package_files="$1"
  local file_name="$2"
  local dst="${3:-.}"
  local file
  file="$(echo -n "$package_files" | jq -r '[.[] | select(.file_name == "'"$file_name"'")][0]')"

  curl -s "https://gitlab.com/tezos/tezos/-/package_files/$(json_select "$file" "id")/download" --output "$dst/$file_name"
}

function fetch_version_release {
  local workdir="${1}"
  local version="${2}"
  local target_tag="octez-evm-node-v${version}"
  local release
  release="$(gitlab_get "releases" | jq '.[] | select(.tag_name == "'"$target_tag"'")')"

  if [ -z "$release" ] || [ "$release" = "null" ]; then
    echo "Error: Release $target_tag not found"
    exit 1
  fi

  static_binaries_package_id="$(json_select "$release" "assets.links" | jq -r '[.[] | select(.name == "Static binaries")][0] | .direct_asset_url' | awk -F/ '{print $NF}')"

  static_binaries_package_files="$(gitlab_get "packages/$static_binaries_package_id/package_files")"

  binary="linux-$ARCH-octez-evm-node"

  fetch "$static_binaries_package_files" "$binary" "$workdir"
  fetch "$static_binaries_package_files" "$binary.sha512" "$workdir"

  echo "$workdir"
  echo "$binary"

  ls "$workdir"

  if ! echo "$(cat "$workdir/$binary.sha512") $workdir/$binary" | xargs | sha512sum -cs; then
    exit 1
  fi

  rm "$workdir/$binary.sha512"
  mv "$workdir/$binary" "$workdir/octez-evm-node"
}

fetch_version_release "$WORKDIR" "$VERSION"

docker pull -q \
  "$BASE_IMAGE" \
  --platform "linux/$DOCKER_PLATFORM" \
  > /dev/null

docker build -q \
  -f etherlink/scripts/Dockerfile.release . \
  -t "tezos/octez-evm-node:${VERSION}" \
  --platform "linux/$DOCKER_PLATFORM" \
  --build-arg "TMPDIR=$WORKDIR" \
  --build-arg "BASE=$BASE_IMAGE" \
  > /dev/null

echo -n "tezos/octez-evm-node:${VERSION}"
