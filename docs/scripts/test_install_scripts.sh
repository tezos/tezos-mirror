#!/usr/bin/env bash
# Test the different Octez installation scenarios documented in howtoget.rst.
#
# The scenarios are implemented by separate scripts, which are both:
# - called by this script within the appropriate Docker containers, and
# - included in howtoget.rst, wholly or in smaller chunks.
#
# The scenarios tested by this script are called both:
# - by the Makefile so that you can test them locally before committing
# - by the CI to check them on a regular basis (e.g. every day)
#
# Therefore, mind keeping in sync the list of scenarios with both the Makefile
# and the CI (file .gitlab/ci/jobs/test/install_octez.yml).

# Ubuntu Focal Fossa 20.04 LTS:
UBUNTU_FOCAL=public.ecr.aws/lts/ubuntu:20.04_stable

# Ubuntu Jammy Jellyfish 22.04 LTS:
UBUNTU_JAMMY=public.ecr.aws/lts/ubuntu:22.04_stable

# Debian stable
DEBIAN_BOOKWORM=debian/bookworm

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
DOCS_DIR="$(dirname "$SCRIPT_DIR")"

usage() {
  cat >&2 << !EOF
usage:
  $0 <test-name>
where <test-name> can be:
* install-bin-focal
* install-bin-jammy
* install-bin-rc-focal
* install-bin-rc-jammy
* install-bin-bookworm
* install-bin-rc-bookworm
* install-opam-scratch
* install-opam-jammy
* compile-release-sources-bullseye
* compile-sources-bullseye
* compile-sources-mantic
* install-python-focal
* install-python-jammy
* install-python-bullseye
!EOF
}

set -e

if [ -z "$1" ]; then
  usage
  exit 1
fi

for test_case in "$@"; do
  case "$test_case" in
  "install-bin-focal")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$UBUNTU_FOCAL" /Scripts/install-bin-deb.sh ubuntu focal
    ;;
  "install-bin-jammy")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$UBUNTU_JAMMY" /Scripts/install-bin-deb.sh ubuntu jammy
    ;;
  "install-bin-rc-focal")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$UBUNTU_FOCAL" /Scripts/install-bin-deb.sh ubuntu focal rc
    ;;
  "install-bin-rc-jammy")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$UBUNTU_JAMMY" /Scripts/install-bin-deb.sh ubuntu jammy rc
    ;;
  "install-bin-bookworm")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$DEBIAN_BOOKWORM" /Scripts/install-bin-deb.sh debian bookworm
    ;;
  "install-bin-rc-bookworm")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts "$DEBIAN_BOOKWORM" /Scripts/install-bin-deb.sh debian bookworm rc
    ;;
  "install-opam-scratch")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts --privileged "$UBUNTU_FOCAL" /Scripts/install-opam-scratch.sh
    ;;
  "install-opam-jammy")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-22.04 /Scripts/install-opam.sh
    ;;
  "compile-release-sources-bullseye")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-11 /Scripts/compile-sources.sh tezos/tezos latest-release
    ;;
  "compile-sources-bullseye")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-11 /Scripts/compile-sources.sh tezos/tezos master
    ;;
  "compile-sources-mantic")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-23.10 /Scripts/compile-sources.sh tezos/tezos master
    ;;
  "install-python-focal")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts "$UBUNTU_FOCAL" /Scripts/install-python-debian-ubuntu.sh
    ;;
  "install-python-jammy")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts "$UBUNTU_JAMMY" /Scripts/install-python-debian-ubuntu.sh
    ;;
  "install-python-bullseye")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts debian:11 /Scripts/install-python-debian-ubuntu.sh
    ;;
  *)
    echo "unknown test name: '$test_case'"
    usage
    exit 1
    ;;

  esac

  echo "test $test_case returned $?"
  echo "*******************************************************************"
done

exit 0
