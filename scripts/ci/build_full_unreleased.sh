#!/bin/sh

set -eu

eval "$(opam env)"

if [ -z "${build_deps_image_name}" ]; then echo "build_deps_image_name is unset" && exit 3; fi
if [ -z "${build_deps_image_version}" ]; then echo "build_deps_image_version is unset" && exit 3; fi

# We remove protocols not needed for tests in order to speed up the CI.
old_protocol_store=$(mktemp -d)
./scripts/remove-old-protocols.sh "$old_protocol_store"
. ./scripts/version.sh

# 1. Some basic, fast sanity checks
if [ "${build_deps_image_version}" != "${opam_repository_tag}" ] ; then
    echo "Inconsistent dependencies hash between 'scripts/version.sh' and '.gitlab/ci/templates.yml'." ;
    echo "${build_deps_image_version} != ${opam_repository_tag}" ;
    exit 1 ;
fi

# 2. Actually build.
#    NOTE: This makes one call to `dune build`, as calling `dune`
#    several time would otherwise need to reconstruct its rules. Do
#    not split this invocation.
#    NOTE: This ensure that $COVERAGE_OPTIONS is used consistently.

# EXECUTABLE_FILES may contain multiple paths and so must be split.
# shellcheck disable=SC2086
OCTEZ_EXECUTABLES="$(cat $EXECUTABLE_FILES)"
make build OCTEZ_EXECUTABLES="$OCTEZ_EXECUTABLES"

# 3. Strip the built binaries in OCTEZ_EXECUTABLES and in BUILD_EXTRA.
# shellcheck disable=SC2086
chmod +w ${OCTEZ_EXECUTABLES}
# shellcheck disable=SC2086
strip -s ${OCTEZ_EXECUTABLES}

if [ -n "${BUILD_EXTRA:-}" ]; then
    paths=$(for executable in ${BUILD_EXTRA}; do echo _build/default/"$executable"; done)
    # Paths must be split so disable shellcheck's SC2086 here.
    # shellcheck disable=SC2086
    chmod +w $paths
    # shellcheck disable=SC2086
    strip -s $paths
fi
