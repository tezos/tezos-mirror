#!/bin/sh

set -eu

DEVEL=0

packages() {
  # Build tezos as usual
  # shellcheck disable=SC1091
  . "$HOME/.cargo/env"
  eval "$(opam env)"
  make release

  # Prepare the packaging by copying all the freshly compiled binaries
  mkdir -p scripts/packaging/octez/binaries
  mkdir -p scripts/packaging/octez/zcash-params
  rm -Rf scripts/packaging/octez/binaries/*

  EXECUTABLES=$(cat script-inputs/*-executables)
  for ex in $EXECUTABLES; do
    if [ -f "$ex" ]; then
      mv -f "$ex" scripts/packaging/octez/binaries/
    fi
  done

  # on the CI cleanup everything before building the packages
  if [ -z "${CI:-}" ]; then
    make clean
  fi

  # Build the debian packages
  cd scripts/packaging/octez/
  DEB_BUILD_OPTIONS=parallel=6 DEB_BUILD_OPTIONS=noautodbgsym \
    dpkg-buildpackage -tc -b --no-sign -sa
  cd -
}

zcash() {
  eval "$(opam env)"
  # Link the zcash DAL parameters to be packaged
  if [ ! -e scripts/packaging/octez-data/zcash-params ]; then
    md5sum --status -c script-inputs/sapling-params.md5
    if $? != 0; then
      echo "Sapling params md5 mismatch. You can fix this problem running :"
      echo "md5sum _opam/share/zcash-params/* > script-inputs/sapling-params.md5"
      md5sum -c script-inputs/sapling-params.md5
      exit 1
    fi
    ln -s "$OPAM_SWITCH_PREFIX/share/zcash-params" scripts/packaging/octez-data/
  fi

  cd scripts/packaging/octez-data
  DEB_BUILD_OPTIONS=noautodbgsym dpkg-buildpackage -tc -b --no-sign -sa
  cd -

}

usage() {
  echo "Usage: $0 (binaries|zcash) [--dev]"
  exit 2
}

check_version() {
  OCTEZ_VERSION=$(dune exec src/lib_version/exe/octez_print_version.exe)
  echo "Version used for the package/octez: $DEBVERSION / $OCTEZ_VERSION"
  if [ -n "${EXPECTED_VERSION:-}" ] && [ "$OCTEZ_VERSION" != "$EXPECTED_VERSION" ]; then
    echo "Executables version does not match the expected version of the packages:"
    echo "Executables version: $OCTEZ_VERSION"
    echo "Expected version: $EXPECTED_VERSION"
    exit 1
  fi
}

# Cleanup old leftover packages
rm -f scripts/packaging/*.deb

export DEBEMAIL="contact@nomadic-labs.com"

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
  DEBVERSION=$VERSION
  DEBCHANGELOG="New Release $VERSION / $CI_COMMIT_SHORT_SHA"
  EXPECTED_VERSION="Octez $(echo "$gitlab_release_no_v" | tr '-' '~') (build: 0)"
  ;;
Rebuild | TestRebuild)
  DEBVERSION=$VERSION
  DEBCHANGELOG="New Release $VERSION / $CI_COMMIT_SHORT_SHA"
  EXPECTED_VERSION="Octez ${gitlab_release_major_version}.${gitlab_release_minor_version} (build: ${gitlab_packaging_revision_version})"
  ;;
Master)
  DEBVERSION="1:$TIMESTAMP+$CI_COMMIT_SHORT_SHA"
  DEBCHANGELOG="Packages for master $CI_COMMIT_SHORT_SHA"
  ;;
SoftRelease)
  DEBVERSION="1:$TIMESTAMP+${CI_COMMIT_TAG:-}"
  DEBCHANGELOG="Packages for tag ${CI_COMMIT_TAG:-}"
  ;;
TestBranch)
  DEBVERSION="1:$TIMESTAMP+$CI_COMMIT_SHORT_SHA"
  DEBCHANGELOG="Test package commit ${CI_COMMIT_REF_NAME:-}"
  ;;
*)
  echo "Cannot create package for this branch"
  exit 1
  ;;
esac

if [ -z "${CI:-}" ]; then
  echo "Warning: You are compiling the debian packages locally."
  echo
  echo "    This script should be only used for development."
  echo "    The version of the debian packages is set to be $DEBVERSION"
  echo "    The version of the octez binaries depends on the git branch / tag"
  echo "    git tag $CI_COMMIT_TAG"
  echo "    git branch $CI_COMMIT_REF_NAME"
  echo "    timestamp: $TIMESTAMP"
  echo
else
  echo "CI compilation"
  echo "    debian version $DEBVERSION"
  echo "    git tag ${CI_COMMIT_TAG:-}"
  echo "    git branch $CI_COMMIT_REF_NAME"
  echo "    timestamp: $TIMESTAMP"
  echo "    ci timestamp: $CI_PIPELINE_CREATED_AT"
fi

cat << EOF > scripts/packaging/octez/debian/changelog
octez (0.0.1-1) stable; urgency=medium

  * Initial release

 -- Nomadic Labs <pkg@nomadic-labs.com>  Tue, 04 Mar 2025 15:23:20 +0100

EOF

# Set a version for the debian package we are building.
debchange --changelog scripts/packaging/octez/debian/changelog \
  --newversion "$DEBVERSION" "$DEBCHANGELOG"
debchange --changelog scripts/packaging/octez-data/debian/changelog \
  --newversion "1.0.0" "Sapling params"

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
  check_version
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
  for p in scripts/packaging/octez-*.deb; do
    lintian "$p" --tag-display-limit 0 --verbose --fail-on warning &
  done
  wait
fi
