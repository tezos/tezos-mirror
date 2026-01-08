#!/bin/sh

set -eu

if [ "$APT_PROXY" != false ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  OS_ID=$ID

  case "$OS_ID" in
  "fedora")

    : noop
    # since fedora uses already a systeme based on mirrors
    # we do nor configure the local proxy
    # Also because of the use of the keyword mirrorlist
    # the use of a local proxy is not easy
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
