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

# Ubuntu Noble 24.04 LTS:
UBUNTU_24_04=public.ecr.aws/lts/ubuntu:24.04_stable

# Ubuntu Jammy 22.04 LTS:
UBUNTU_22_04=public.ecr.aws/lts/ubuntu:22.04_stable

# Debian oldstable
DEBIAN_BOOKWORM=debian:bookworm

# Debian stable
DEBIAN_TRIXIE=debian:trixie

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
DOCS_DIR="$(dirname "$SCRIPT_DIR")"

usage() {
  cat >&2 << !EOF
usage:
  $0 <test-name>
where <test-name> can be:
* install-bin-24.04
* install-bin-22.04
* install-bin-rc-24.04
* install-bin-rc-22.04
* install-bin-bookworm
* install-bin-trixie
* install-bin-rc-bookworm
* install-opam-scratch
* install-opam-22.04
* install-opam-24.04
* compile-release-sources-bookworm
* compile-sources-bookworm
* compile-sources-oracular
* install-python-24.04
* install-python-22.04
* install-python-bookworm
!EOF
}

set -e

if [ -z "$1" ]; then
  usage
  exit 1
fi

for test_case in "$@"; do
  case "$test_case" in
  "install-bin-24.04")
    docker run --rm -i -e RELEASETYPE=Master -v "$DOCS_DIR/..":/Tezos "$UBUNTU_24_04" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh ubuntu 24.04"
    ;;
  "install-bin-22.04")
    docker run --rm -i -e RELEASETYPE=Master -v "$DOCS_DIR/..":/Tezos "$UBUNTU_22_04" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh ubuntu 22.04"
    ;;
  "install-bin-rc-24.04")
    docker run --rm -i -e RELEASETYPE=ReleaseCandidate -e GCP_LINUX_PACKAGES_BUCKET=tezos-linux-repo -v "$DOCS_DIR/..":/Tezos "$UBUNTU_24_04" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh ubuntu 24.04 rc"
    ;;
  "install-bin-rc-22.04")
    docker run --rm -i -e RELEASETYPE=ReleaseCandidate -e GCP_LINUX_PACKAGES_BUCKET=tezos-linux-repo -v "$DOCS_DIR/..":/Tezos "$UBUNTU_22_04" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh ubuntu 22.04 rc"
    ;;
  "install-bin-bookworm")
    docker run --rm -i -e RELEASETYPE=Master -v "$DOCS_DIR/..":/Tezos "$DEBIAN_BOOKWORM" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh debian bookworm"
    ;;
  "install-bin-trixie")
    docker run --rm -i -e RELEASETYPE=Master -v "$DOCS_DIR/..":/Tezos "$DEBIAN_TRIXIE" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh debian trixie"
    ;;
  "install-bin-rc-bookworm")
    docker run --rm -i -e RELEASETYPE=ReleaseCandidate -e GCP_LINUX_PACKAGES_BUCKET=tezos-linux-repo -v "$DOCS_DIR/..":/Tezos "$DEBIAN_BOOKWORM" /bin/sh -c "cd Tezos; ./docs/introduction/install-bin-deb.sh debian bookworm rc"
    ;;
  "install-opam-scratch")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts --privileged "$UBUNTU_24_04" /Scripts/install-opam-scratch.sh
    ;;
  "install-opam-22.04")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-22.04 /Scripts/install-opam.sh
    ;;
  "install-opam-24.04")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-24.04 /Scripts/install-opam.sh
    ;;
  "compile-release-sources-bookworm")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-12 /Scripts/compile-sources.sh tezos/tezos latest-release
    ;;
  "compile-sources-bookworm")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-12 /Scripts/compile-sources.sh tezos/tezos master
    ;;
  "compile-sources-oracular")
    docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-24.10 /Scripts/compile-sources.sh tezos/tezos master
    ;;
  "install-python-24.04")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts "$UBUNTU_24_04" /Scripts/install-python-debian-ubuntu.sh
    ;;
  "install-python-22.04")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts "$UBUNTU_22_04" /Scripts/install-python-debian-ubuntu.sh
    ;;
  "install-python-bookworm")
    docker run --rm -i -v "$DOCS_DIR/developer":/Scripts debian:12 /Scripts/install-python-debian-ubuntu.sh
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
