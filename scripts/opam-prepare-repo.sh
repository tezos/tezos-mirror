#!/bin/sh

set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"

usage="Usage: $0 <VERSION_NUMBER> <TARBALL_URL_OR_PATH> [OPAM_REPOSITORY_CLONE_DIR]

Example: $0 11.0 https://gitlab.com/tezos/tezos/-/archive/v11.0/tezos-v11.0.tar.bz2

This script creates an opam-repository with all tezos opam packages.

Default value for OPAM_REPOSITORY_CLONE_DIR is 'opam-repository'.

TARBALL_URL_OR_PATH is the URL (OR PATH) to put in opam files.  The
script downloads the tarball (if necessary) and computes sha256 and
sha512 checksums for you."

version="${1:-}"
tarball="${2:-}"
opam_dir="${3:-opam-repository}"

if [ -z "$version" ]; then
  echo "$usage"
  exit 1
fi

if [ -z "$tarball" ]; then
  echo "$usage"
  exit 1
fi

log() {
  printf '\e[1m%s\e[0m' "$1"
}

if [ -d "$opam_dir" ]; then
  log "$opam_dir exists"
else
  log "Error: $opam_dir does not exists or is not a directory"
  exit 1
fi

tarball_copy=$(mktemp tezos_tarball.XXXXXXXX --tmpdir)

clean_tarball() {
  log "Cleaning up..."
  rm -f "$tarball_copy"
}
trap clean_tarball EXIT

if [ -f "$tarball" ]; then
  cp "$tarball" "$tarball_copy"
elif [ -d "$tarball" ]; then
  tarball=$(realpath "$tarball")
  log "using directory '$tarball' as url"
  rm -f "$tarball_copy"
else
  log "Downloading tarball from $tarball..."
  curl "$tarball" --output "$tarball_copy"
fi

if [ -f "$tarball_copy" ]; then
  log "Hashing tarball..."
  sha256=$(sha256sum "$tarball_copy" | cut -d ' ' -f 1)
  log "SHA256: $sha256"
  sha512=$(sha512sum "$tarball_copy" | cut -d ' ' -f 1)
  log "SHA512: $sha512"
else
  sha256=""
  sha512=""
fi

log "Generating opam files for $version..."
cd "$script_dir"/..
make -C manifest manifest
./manifest/manifest \
  --packages-dir "$opam_dir/packages" \
  --url "$tarball" \
  --sha256 "$sha256" \
  --sha512 "$sha512" \
  --release "$version"
