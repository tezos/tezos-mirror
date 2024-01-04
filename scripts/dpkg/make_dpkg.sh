#!/bin/sh

# Debian/Ubuntu package build for Octez
#
# (c) Chris Pinnock 2022-3, Supplied under a MIT license.
# see ../pkg-common/utils.sh for more detail

set -eu

# Setup
#
myhome=scripts/dpkg
common=scripts/pkg-common
dieonwarn=${dieonwarn:-1}

#shellcheck disable=SC1091
. ${common}/utils.sh
protocols=${protocols:?protocols not specified} # Not used?

warnings
pkg_vers=$(getOctezVersion)
staging_root=_dpkgstage

# Checking prerequisites
#
if ! which dpkg-deb > /dev/null 2>&1; then
  echo "Needs to run on a system with dpkg-deb in path" >&2
  exit 2
fi

# Get the local architecture
#
eval "$(dpkg-architecture)"
dpkg_arch=$DEB_BUILD_ARCH

# For each control file in the directory, build a package
#
for control_file in "$myhome"/*control.in; do
  pg=$(basename "$control_file" | sed -e 's/-control.in$//g')
  echo "===> Building package $pg v$pkg_vers rev $OCTEZ_PKGREV"

  # Derivative variables
  #
  dpkg_name=${OCTEZ_PKGNAME}-${pg}
  init_name=${OCTEZ_REALNAME}-${pg}
  dpkg_dir="${dpkg_name}_${pkg_vers}-${OCTEZ_PKGREV}_${dpkg_arch}"
  dpkg_fullname="${dpkg_dir}.deb"

  binaries=$(fixBinaryList "${common}/${pg}-binaries")

  if [ -f "$dpkg_fullname" ]; then
    echo "built already - skipping"
    continue
  fi

  # Populate the staging directory with control scripts
  # binaries and configuration as appropriate
  #
  staging_dir="$staging_root/$dpkg_dir"

  rm -rf "${staging_dir}"
  mkdir -p "${staging_dir}/DEBIAN"

  if [ -n "$binaries" ]; then
    echo "=> Populating directory with binaries"
    mkdir -p "${staging_dir}/usr/bin"
    for bin in ${binaries}; do
      if [ -f "${bin}" ]; then
        echo "Installing ${bin}"
        install -s -t "${staging_dir}/usr/bin" "${bin}"
      else
        echo "WARN: ${bin} not found"
        [ "$dieonwarn" = "1" ] && exit 1
      fi
    done

    # Shared libraries
    #
    mkdir -p "${staging_dir}/debian"
    touch "${staging_dir}/debian/control"

    echo "=> Finding shared library dependencies"

    deps=$(cd "${staging_dir}" && dpkg-shlibdeps -O usr/bin/* | sed -e 's/^shlibs://g' -e 's/^Depends=//g')
    rm "${staging_dir}/debian/control"
    rmdir "${staging_dir}/debian"
  fi

  # Edit the control file to contain real values
  #
  sed -e "s/@ARCH@/${dpkg_arch}/g" -e "s/@VERSION@/$pkg_vers/g" \
    -e "s/@MAINT@/${OCTEZ_PKGMAINTAINER}/g" \
    -e "s/@PKG@/${dpkg_name}/g" \
    -e "s/@DPKG@/${OCTEZ_PKGNAME}/g" \
    -e "s/@DEPENDS@/${deps}/g" < "$control_file" \
    > "${staging_dir}/DEBIAN/control"

  # Install hook scripts (not used initially)
  #
  for src in postinst preinst postrm prerm; do
    if [ -f "${myhome}/${pg}.$src" ]; then
      cp "${myhome}/${pg}.$src" "${staging_dir}/DEBIAN/$src"
      chmod +x "${staging_dir}/DEBIAN/$src"
    fi
  done

  # init.d scripts
  #
  initdScripts "${common}/${pg}.initd.in" "${init_name}" "${staging_dir}"
  if [ "$pg" = "baker" ]; then
    initdScripts "${common}/vdf.initd.in" octez-vdf "${staging_dir}"
  fi

  # Configuration files
  #
  if [ -f "${common}/${pg}.conf" ]; then
    mkdir -p "${staging_dir}/etc/octez"
    expand_PROTOCOL "${common}/${pg}.conf" > "${staging_dir}/etc/octez/${pg}.conf"
    echo "/etc/octez/${pg}.conf" > "${staging_dir}/DEBIAN/conffiles"
  fi

  # Zcash parameters ships with some packages
  #
  zcashParams "${common}/${pg}-zcash" \
    "${staging_dir}/usr/share/zcash-params"

  # Build the package
  #
  echo "=> Constructing package ${dpkg_fullname}"
  dpkg-deb -v --build --root-owner-group "${staging_dir}"
  mv "${staging_root}/${dpkg_fullname}" .
done
