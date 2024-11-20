#!/bin/bash

set -eu

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
  make octez

  rm -f "${BINARIES:?}/*"
  EXECUTABLES=$(cat script-inputs/*-executables)
  for ex in $EXECUTABLES; do
    if [ -f "$ex" ]; then
      cp -f "$ex" "$BINARIES"
    fi
  done

  cp -a "$packaging_dir/scripts/packaging/octez/manpages/" "$SPECS_DIR/"
  cp -a "$packaging_dir/scripts/packaging/octez/scripts/" "$SPECS_DIR/"
  "$SPECS_DIR/"binaries/octez-node --help=groff > \
    "$SPECS_DIR/"manpages/octez-node.1
  "$SPECS_DIR/"binaries/octez-dal-node --help=groff > \
    "$SPECS_DIR/"manpages/octez-dal-node.1
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.service "$SPECS_DIR/"
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.default "$SPECS_DIR/"
  cp "$packaging_dir/scripts/packaging/octez/debian/"*.manpages "$SPECS_DIR/"
  grep "Package:" "$packaging_dir/scripts/packaging/octez/debian/control" | cut -d' ' -f2 |
    while read -r pkg; do
      cp "$packaging_dir/scripts/packaging/octez/rpm/SPECS/$pkg.spec" "$SPECS_DIR/"
      cd "$SPECS_DIR" || exit
      rpmbuild -ba \
        --define '_source_filedir %{nil}' \
        --define "_binary_payload w2T16.xzdio" \
        "$pkg.spec"
      cd - || exit 1
    done
  cd - || exit 1
}

zcash() {
  # Build data packages

  cp -a "$OPAM_SWITCH_PREFIX/share/zcash-params/" \
    "$OPAM_SWITCH_PREFIX/share/dal-trusted-setup/" "$SPECS_DIR/"

  grep "Package:" "$packaging_dir/scripts/packaging/octez-data/debian/control" | cut -d' ' -f2 |
    while read -r pkg; do
      cp "$packaging_dir/scripts/packaging/octez/rpm/SPECS/$pkg.spec" "$SPECS_DIR/"
      cd "$SPECS_DIR" || exit
      rpmbuild -ba \
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
  echo "    The version of the rpm packages is set to be '0.0.1-1'"
  echo "    The version of the octez binaries depends on the git branch / tag"
  echo
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
