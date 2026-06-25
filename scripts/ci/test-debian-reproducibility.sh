#!/bin/sh

# Verify that the Debian packages build reproducibly: rebuild the binary, data
# and keyring packages from the current commit and check, with diffoscope, that
# they are byte-for-byte identical to the packages already produced by the
# [oc.build-debian], [oc.build-data_packages] and [oc.build-keyring_package]
# jobs (pulled in as artifacts under packages/$DISTRIBUTION/$RELEASE). Reusing
# the first build as the reference means we only pay for a single extra build.

set -eu

DISTRIBUTION="${DISTRIBUTION:-debian}"
RELEASE="${RELEASE:-trixie}"
PKG_DIR="packages/${DISTRIBUTION}/${RELEASE}"

# Move the reference packages built by the upstream build jobs aside: the
# rebuild below overwrites ${PKG_DIR}, so we relocate them rather than copy.
if ! ls "${PKG_DIR}"/*.deb > /dev/null 2>&1; then
  echo "ERROR: no reference packages found in ${PKG_DIR}; expected the" \
    "${DISTRIBUTION} ${RELEASE} artifacts from the package build jobs." >&2
  exit 1
fi
REFERENCE=$(mktemp -d)
mv "${PKG_DIR}"/*.deb "${REFERENCE}/"

# Install diffoscope for the byte-level comparison. --no-install-recommends is
# important: diffoscope Recommends a large set of optional comparators
# (GUI/Java/document tools) we do not need for .deb files, and pulling them in
# is slow and fails against the CI apt proxy.
# TODO: https://gitlab.com/tezos/tezos/-/issues/8345
# bake diffoscope into the debian-build base image so this runtime install
# can be dropped.
export DEBIAN_FRONTEND=noninteractive
apt-get update -qq
apt-get install -y -qq --no-install-recommends diffoscope

# Rebuild the binary, data and keyring packages from the same commit. Each
# upstream build job ran build-debian-packages.sh once from a pristine checkout;
# here we run several targets in one checkout, but build-deb-local.sh appends to
# the octez-data / keyring changelogs in place (only the octez changelog is
# regenerated from scratch each run). Restore the tracked changelogs before each
# target so every rebuild starts from the same state the reference builds saw.
for target in binaries zcash keyring; do
  git checkout -- \
    scripts/packaging/octez-data/debian/changelog \
    scripts/packaging/octez-archive-keyring/debian/changelog
  ./scripts/ci/build-debian-packages.sh "${target}"
done

# Compare each rebuilt package against the reference build. The reference set
# also contains the build matrix's other-architecture .deb files (oc.build-debian
# builds amd64 and arm64), but we can only rebuild for this runner's
# architecture, so skip packages built for a different arch. Architecture-
# independent packages (*_all.deb: zcash, keyring) are always checked.
host_arch=$(dpkg --print-architecture)
status=0
for ref in "${REFERENCE}"/*.deb; do
  name=$(basename "${ref}")
  case "${name}" in
  *_all.deb | *_"${host_arch}".deb) ;;
  *)
    echo "=== ${name} === SKIP (built for another architecture)"
    continue
    ;;
  esac
  rebuilt="${PKG_DIR}/${name}"
  echo "=== ${name} ==="
  if [ ! -f "${rebuilt}" ]; then
    echo "MISSING: ${name} was not produced by the rebuild" >&2
    status=1
    continue
  fi
  if diffoscope "${ref}" "${rebuilt}"; then
    echo "OK: ${name} is byte-for-byte reproducible"
  else
    echo "NON-REPRODUCIBLE: ${name} differs between builds" >&2
    status=1
  fi
done

if [ "${status}" -eq 0 ]; then
  echo "All ${DISTRIBUTION} ${RELEASE} packages are byte-for-byte reproducible."
fi
exit "${status}"
