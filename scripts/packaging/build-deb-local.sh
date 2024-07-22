#!/bin/sh

TYPE=${1}

set -eu

packages() {
  # Build tezos as usual
  # shellcheck disable=SC1091
  . "$HOME/.cargo/env"
  eval "$(opam env)"
  make all

  # Prepare the packaging by copying all the freshly compiled binaries
  mkdir -p scripts/packaging/octez/binaries
  mkdir -p scripts/packaging/octez/zcash-params
  rm -Rf scripts/packaging/octez/binaries/*

  EXECUTABLES=$(cat script-inputs/*-executables)
  for ex in $EXECUTABLES; do
    cp "$ex" scripts/packaging/octez/binaries/
  done

  # Build the debian packages
  cd scripts/packaging/octez/
  DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
  cd -
}

zcash() {
  # Link the zcash dans DAL parametes to be packaged
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

# Cleanup old leftover packages
rm -f scripts/packaging/*.deb

case $TYPE in
"binaries")
  echo "Building binary packages only"
  packages
  ;;
"zcash")
  echo "Building DATA packages only"
  zcash
  ;;
*)
  zcash
  packages
  ;;
esac

echo "All packages are available in ./scripts/packaging"

# Run lintian only if building packages locally.
# On the CI we have a specific job for it
if [ -z "${CI:-}" ]; then
  lintian scripts/packaging/octez-*.deb --tag-display-limit 0 --verbose
fi
