#!/bin/sh

set -eu

DEVEL=0

packages() {
  set -x
  # Build tezos as usual
  # shellcheck disable=SC1091
  . "$HOME/.cargo/env"
  eval "$(opam env)"
  make octez

  # Prepare the packaging by copying all the freshly compiled binaries
  mkdir -p scripts/packaging/octez/binaries
  mkdir -p scripts/packaging/octez/zcash-params
  rm -Rf scripts/packaging/octez/binaries/*

  EXECUTABLES=$(cat script-inputs/*-executables)
  for ex in $EXECUTABLES; do
    if [ -f "$ex" ]; then
      cp "$ex" scripts/packaging/octez/binaries/
    fi
  done

  # Build the debian packages
  cd scripts/packaging/octez/
  DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
  cd -
}

zcash() {
  # Link the zcash DAL parameters to be packaged
  if [ ! -e scripts/packaging/octez-data/zcash-params ]; then
    ln -s ../../../_opam/share/zcash-params scripts/packaging/octez-data/
  fi

  if [ ! -e scripts/packaging/octez-data/dal-trusted-setup ]; then
    scripts/install_dal_trusted_setup.sh
    ln -s ../../../_opam/share/dal-trusted-setup scripts/packaging/octez-data/
  fi

  cd scripts/packaging/octez-data
  DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
  cd -

}

usage() {
  echo "Usage: $0 (binaries|zcash) [--dev]"
  exit 2
}

# Cleanup old leftover packages
rm -f scripts/packaging/*.deb

TARGET=all

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
  "--dev")
    DEVEL=1
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

echo "All packages are available in ./scripts/packaging"

# Run lintian only if building packages locally.
# On the CI we have a specific job for it
if [ -z "${CI:-}" ] && [ ${DEVEL} != "1" ]; then
  echo "Running lintian scripts/packaging/octez-*.deb"
  lintian scripts/packaging/octez-*.deb --tag-display-limit 0 --verbose
fi
