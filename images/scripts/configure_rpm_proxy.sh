#!/bin/sh

set -eu

if [ "$APT_PROXY" != false ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  ARCH=$(uname -m)
  OS_ID=$ID
  OS_VERSION=$VERSION_ID

  case "$OS_ID" in
  "fedora")

    echo "zchunk=false" >> /etc/dnf/dnf.conf
    for file in /etc/yum.repos.d/fedora*.repo; do
      sed -i \
        -e 's|^metalink=|#metalink=|' \
        -e "s|^#baseurl=http://download.example.*|baseurl=http://dl.fedoraproject.org/pub/fedora/linux/updates/$OS_VERSION/Everything/$ARCH/|" \
        -e "s|^baseurl=.*|baseurl=http://dl.fedoraproject.org/pub/fedora/linux/updates/$OS_VERSION/Everything/$ARCH/|" \
        -e "/^baseurl=/a proxy=$APT_PROXY" \
        -e '/^\[.*\]/a metadata_types=rpm-md' \
        "$file"
    done
    ;;

  "rockylinux" | "rocky")

    for file in /etc/yum.repos.d/rocky-*.repo; do
      sed -i \
        -e 's|^mirrorlist=|#mirrorlist=|' \
        -e 's|^#baseurl=http://|baseurl=http://|' \
        -e "/^baseurl=/a proxy=$APT_PROXY" \
        "$file"
    done
    ;;

  *)
    echo "rpm distribution not supported"
    exit 1
    ;;
  esac

  dnf makecache

else
  echo "APT_PROXY not set"
  exit 1
fi
