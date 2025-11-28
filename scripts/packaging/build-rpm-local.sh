#!/bin/bash

set -eu

if [ -z "${CI:-}" ]; then
  TIMESTAMP=$(date '+%Y%m%d%H%M')
  CI_COMMIT_SHORT_SHA=$(git rev-parse --short HEAD)
  CI_COMMIT_REF_NAME=$(git rev-parse --abbrev-ref HEAD)
  CI_COMMIT_TAG=$(git describe --exact-match --tags 2> /dev/null || git rev-parse --short HEAD)
else
  TIMESTAMP="$(date -d "$CI_PIPELINE_CREATED_AT" '+%Y%m%d%H%M')"
fi

gitlab_release_no_v=
. scripts/ci/octez-packages-version.sh

case "$RELEASETYPE" in
ReleaseCandidate | TestReleaseCandidate | Release | TestRelease | Beta | TestBeta)
  # rpm versions are more strict than debian versions
  _VERSION=$(echo "${VERSION}" | tr -d '~' | tr '-' '_')
  _EPOCH="%{nil}"
  _CHANGELOG="New Release $VERSION / $CI_COMMIT_SHORT_SHA"
  ;;
Master)
  _VERSION=$TIMESTAMP+$CI_COMMIT_SHORT_SHA
  _EPOCH=1
  _CHANGELOG="Packages for master $CI_COMMIT_SHORT_SHA"
  ;;
SoftRelease)
  _VERSION=$TIMESTAMP+${CI_COMMIT_TAG:-}
  _EPOCH=1
  _CHANGELOG="Packages for tag ${CI_COMMIT_TAG:-}"
  ;;
TestBranch)
  _VERSION=$TIMESTAMP+$CI_COMMIT_SHORT_SHA
  _EPOCH=1
  _CHANGELOG="Test package commit ${CI_COMMIT_REF_NAME:-}"
  ;;
*)
  echo "Cannot create package for this branch"
  exit 1
  ;;
esac

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
packaging_dir="$(dirname "$(dirname "$script_dir")")"

BUILD_DIR="$HOME/rpmbuild"
SOURCES_DIR="$BUILD_DIR/SOURCES"
SPECS_DIR="$BUILD_DIR/SPECS"
RPMS_DIR="$BUILD_DIR/RPMS"
BINARIES="$SPECS_DIR/binaries"

# Create RPM build directories
mkdir -p "$SOURCES_DIR" "$SPECS_DIR" "$RPMS_DIR" "$BINARIES"

# shellcheck disable=SC1091
. "$HOME/.cargo/env"
eval "$(opam env)"

packages() {
  # Build Octez as usual
  make release

  rm -f "${BINARIES:?}/*"
  EXECUTABLES=$(cat script-inputs/*-executables)
  for ex in $EXECUTABLES; do
    if [ -f "$ex" ]; then
      mv -f "$ex" "$BINARIES"
    fi
  done

  # on the CI cleanup everything before building the packages
  if [ -z "${CI:-}" ]; then
    make clean
  fi

  cp -a "$packaging_dir/scripts/packaging/octez/manpages/" "$SPECS_DIR/"
  cp -a "$packaging_dir/scripts/packaging/octez/scripts/" "$SPECS_DIR/"
  "$SPECS_DIR/"binaries/octez-node --help=groff > \
    "$SPECS_DIR/"manpages/octez-node.1
  "$SPECS_DIR/"binaries/octez-dal-node --help=groff > \
    "$SPECS_DIR/"manpages/octez-dal-node.1
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.service "$SPECS_DIR/"
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.default "$SPECS_DIR/"
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.manpages "$SPECS_DIR/"
  {
    echo "DATADIR=/var/tezos/.tezos-node"
    echo "NETWORK=mainnet"
    echo "HISTORY_MODE=rolling"
    echo "SNAPSHOT_IMPORT=true"
    echo "SNAPSHOT_NO_CHECK=false"
  } >> "$SPECS_DIR/octez-node.default"

  grep "Package:" "$packaging_dir/scripts/packaging/octez/debian/control" | cut -d' ' -f2 |
    while read -r pkg; do
      cp "$packaging_dir/scripts/packaging/octez/rpm/SPECS/$pkg.spec" "$SPECS_DIR/"
      cd "$SPECS_DIR" || exit
      rpmbuild -ba \
        --define "version $_VERSION" \
        --define "epoch $_EPOCH" \
        --define '_source_filedir %{nil}' \
        --define "_binary_payload w2T16.xzdio" \
        "$pkg.spec"
      cd - || exit 1
    done
  cd - || exit 1
}

zcash() {
  # Build data packages

  cp -a "$OPAM_SWITCH_PREFIX/share/zcash-params/" "$SPECS_DIR/"

  grep "Package:" "$packaging_dir/scripts/packaging/octez-data/debian/control" | cut -d' ' -f2 |
    while read -r pkg; do
      cp "$packaging_dir/scripts/packaging/octez/rpm/SPECS/$pkg.spec" "$SPECS_DIR/"
      cd "$SPECS_DIR" || exit
      rpmbuild -ba --quiet \
        --define "version 1.0.0" \
        --define '_source_filedir %{nil}' \
        --define "_binary_payload w2T16.xzdio" \
        "$pkg.spec"
      cd - || exit 1
    done
  cd - || exit 1
}

usage() {
  echo "Usage: $0 (binaries|zcash) [--dev]"
  exit 2
}

TARGET=all

if [ -z "${CI:-}" ]; then
  echo "Warning: You are compiling the rpm packages locally."
  echo
  echo "    This script should be only used for development."
  echo "    The version of the packages is set to be $_VERSION"
  echo "    The version of the octez binaries depends on the git branch / tag"
  echo "    git tag ${CI_COMMIT_TAG:-}"
  echo "    git branch $CI_COMMIT_REF_NAME"
  echo "    timestamp: $TIMESTAMP"
  echo
else
  echo "CI compilation"
  echo "    version $_VERSION"
  echo "    git tag ${CI_COMMIT_TAG:-}"
  echo "    git branch $CI_COMMIT_REF_NAME"
  echo "    timestamp: $TIMESTAMP"
  echo "    ci timestamp: $CI_PIPELINE_CREATED_AT"
fi

while [ $# -gt 0 ]; do
  case ${1} in
  "binaries")
    TARGET=binaries
    shift
    ;;
  "zcash")
    TARGET=zcash
    shift
    ;;
  "help")
    usage
    ;;
  *)
    echo "Unknown command or option: $1"
    usage
    ;;
  esac
done

case ${TARGET} in
"binaries")
  echo "Building binary packages only"
  packages
  ;;
"zcash")
  echo "Building data packages only"
  zcash
  ;;
"all")
  echo "Building binary and data packages"
  packages
  zcash
  ;;
esac

cd "$packaging_dir" || exit
# Show the resulting RPM package
echo "Built RPM packages are located in $RPMS_DIR"
