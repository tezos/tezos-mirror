#!/bin/sh

# Common packaging routines for Octez
#

# A better way to do this would be to build the package from source
# but given the various hurdles of Rust and OPAM during the build
# we construct packages afterwards. Which is not best practice :-)
#
# A better strategy would be to extract the version number, build a
# master spec file, build Octez and then make the packages from the
# master spec file.
#
# Place files in the dpkg directory to declare a package. e.g.
#
# baker-control.in      - a template for the Debian control file
#
# Place files in the rpm directory to declare packages similarly:
#
# baker-spec.in         - a template for the RPM SPEC file
# cf. https://rpm-packaging-guide.github.io/#binary-rpms
#
# These files are shared with the Debian package build in pkg-common
#
# baker.conf            - an example configuration file (optional)
# baker-binaries.in        - the list of binaries to include
# baker.initd.in           - System V init script (optional)
#
# Where Protocol variations are needed use @PROTOCOL@ and @PROTOCOLS@
#
# You can set OCTEZ_PKGMAINTAINER and OCTEZ_PKGNAME in the environment
# to change from the defaults.
#

# Initialise from active protocol versions
#
proto_file="script-inputs/active_protocol_versions_without_number"

if [ ! -f $proto_file ]; then
  echo "Cannot find active protocol list"
  exit 2
fi
protocols=$(tr '\n' ' ' < $proto_file | sed -e 's/ $//g')

# Variables
#
# Package maintainer
OCTEZ_PKGMAINTAINER=${OCTEZ_PKGMAINTAINER:-package@nomadic-labs.com}
#
# Package name used in dpkg or rpm name
OCTEZ_PKGNAME=${OCTEZ_PKGNAME:-octez}
#
# Real name used in scripts (usually octez)
OCTEZ_REALNAME=${OCTEZ_REALNAME:-octez}
#
# Revision
OCTEZ_PKGREV=${OCTEZ_PKGREV:-1}

export OCTEZ_PKGMAINTAINER
export OCTEZ_PKGNAME
export OCTEZ_REALNAME
export OCTEZ_PKGREV

# Expand protocols in configuration and init files
#
expand_PROTOCOL() {
  file="$1"
  protocols_formatted=""
  protocols_list=""

  for i in $protocols; do

    if [ "$protocols_list" = "" ]; then
      protocols_list="$i"
    else
      protocols_list="$protocols_list $i"
    fi

    if [ "$i" != "alpha" ]; then
      # Alpha is handled in an experimental package
      #shellcheck disable=SC1003
      protocols_formatted=$protocols_formatted'\'"1${i}"'\'"2\n"
    fi

  done

  sed -e "s/@PROTOCOLS@/$protocols_list/g" \
    -e "/@PROTOCOL@/ { s/^\(.*\)@PROTOCOL@\(.*\)$/$protocols_formatted/; s/\\n$//; }" \
    "$file"

}

# Issue Warnings
#

warnings() {
  # BETA WARNING
  echo "WARNING: This build script should be considered beta for now"

  # Generic warning about BLST_PORTABLE=yes
  #
  BLST_PORTABLE=${BLST_PORTABLE:-notused}
  if [ "$BLST_PORTABLE" != "yes" ]; then
    echo "WARNING: BLST_PORTABLE is not set to yes in your environment"
    echo "If the binaries were not made with BLST_PORTABLE=yes then they"
    echo "might not run on some platforms."
  fi
}

# Get Octez version from the build
#

getOctezVersion() {

  . scripts/ci/octez-packages-version.sh

  # provide defaults for local compilation
  COMMIT_SHORT_SHA=${CI_COMMIT_SHORT_SHA:-$(git rev-parse --short HEAD)}

  case "$RELEASETYPE" in
  ReleaseCandidate | TestReleaseCandidate | Release | TestRelease)
    # rpm version do not accept '-' as a character
    # https://rpm-software-management.github.io/rpm/manual/spec.html
    RET=$(echo "$VERSION" | tr '-' '~')
    ;;
  SoftRelease)
    RET=$TIMESTAMP+$(echo "${CI_COMMIT_TAG:-}" | tr '-' '_')
    ;;
  Master | TestBranch | TestProtectedBranch)
    RET=$TIMESTAMP+$COMMIT_SHORT_SHA
    ;;
  *)
    RET=$TIMESTAMP+$COMMIT_SHORT_SHA
    ;;
  esac

  echo "$RET"

}

# Build init.d scripts
#

initdScripts() {
  _initin=$1     # Init script
  _inittarget=$2 # The target (e.g. octez-node)
  _stagedir=$3   # The staging area
  _initd="${_stagedir}/etc/init.d"

  if [ -f "${_initin}" ]; then
    mkdir -p "${_initd}"
    expand_PROTOCOL "${_initin}" \
      > "${_initd}/${_inittarget}"
    chmod +x "${_initd}/${_inittarget}"
  fi

}

# Fix up the binary lists
#
fixBinaryList() {
  _binlist=$1
  _binaries=""
  if [ -f "${_binlist}.in" ]; then
    expand_PROTOCOL "${_binlist}.in" > "${_binlist}"
  fi

  if [ -f "${_binlist}" ]; then
    _binaries=$(cat "${_binlist}" 2> /dev/null)
  fi
  echo "$_binaries"
}

# Deal with Zcash parameters
#
zcashParams() {
  _pkgzcash=$1
  _zcashtgt=$2
  # Where the zcash files are
  _zcashdir=${3:-"_opam/share/zcash-params"}

  if [ -f "${_pkgzcash}" ]; then
    zcashstuff=$(cat "${_pkgzcash}" 2> /dev/null)
    echo "=> Zcash"
    mkdir -p "${_zcashtgt}"
    for shr in ${zcashstuff}; do
      cp "${_zcashdir}/${shr}" \
        "${_zcashtgt}"
    done
  fi
}
