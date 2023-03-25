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
# and the CI (file .gitlab/ci/doc/doc-scripts.yml).

# Ubuntu Bionic Beaver 18.04 LTS:
UBUNTU_BIONIC=public.ecr.aws/lts/ubuntu:18.04_stable
# Ubuntu Focal Fossa 20.04 LTS:
UBUNTU_FOCAL=public.ecr.aws/lts/ubuntu:20.04_stable

SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
DOCS_DIR="$(dirname "$SCRIPT_DIR")"

usage () {
    cat >&2 <<!EOF
usage:
  $0 <test-name>
where <test-name> can be:
* install-bin-bionic
* install-bin-focal
* install-bin-fedora36
* install-bin-rc-bionic
* install-bin-rc-focal
* install-bin-rc-fedora36
* install-opam-scratch
* install-opam-bionic
* install-opam-focal
* compile-release-sources-buster
* compile-sources-buster
* install-python-bionic
* install-python-focal
* install-python-buster
!EOF
}

set -e

if [ -z "$1" ]; then
    usage
    exit 1
fi

for test_case in "$@"; do
    case "$test_case" in
        "install-bin-bionic" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_BIONIC /Scripts/install-bin-ubuntu.sh
            ;;
        "install-bin-focal" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_FOCAL /Scripts/install-bin-ubuntu.sh
            ;;
        "install-bin-fedora36" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts fedora:36 /Scripts/install-bin-fedora.sh
            ;;
        "install-bin-rc-bionic" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_BIONIC /Scripts/install-bin-ubuntu.sh rc
            ;;
        "install-bin-rc-focal" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_FOCAL /Scripts/install-bin-ubuntu.sh rc
            ;;
        "install-bin-rc-fedora36" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts fedora:36 /Scripts/install-bin-fedora.sh rc
            ;;
        "install-opam-scratch" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts --privileged $UBUNTU_BIONIC /Scripts/install-opam-scratch.sh
            ;;
        "install-opam-bionic" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-18.04 /Scripts/install-opam.sh
            ;;
        "install-opam-focal" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:ubuntu-20.04 /Scripts/install-opam.sh
            ;;
        "compile-release-sources-buster" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-10 /Scripts/compile-sources.sh tezos/tezos latest-release
            ;;
        "compile-sources-buster" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-10 /Scripts/compile-sources.sh tezos/tezos master
            ;;
        "install-python-bionic" )
            docker run --rm -i -v "$DOCS_DIR/developer":/Scripts $UBUNTU_BIONIC /Scripts/install-python-debian-ubuntu.sh
            ;;
        "install-python-focal" )
            docker run --rm -i -v "$DOCS_DIR/developer":/Scripts $UBUNTU_FOCAL /Scripts/install-python-debian-ubuntu.sh
            ;;
        "install-python-buster" )
            docker run --rm -i -v "$DOCS_DIR/developer":/Scripts debian:buster /Scripts/install-python-debian-ubuntu.sh
            ;;
        * )
            echo "unknown test name: '$test_case'"
            usage
            exit 1 ;;

    esac

    echo "test $test_case returned $?"
    echo "*******************************************************************"
done

exit 0
