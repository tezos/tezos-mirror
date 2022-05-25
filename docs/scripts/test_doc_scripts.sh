#!/usr/bin/env bash
# Test the different Tezos installation scenarios documented in howtoget.rst.
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
* use-docker
* install-bin-bionic
* install-bin-focal
* install-bin-fedora34
* install-bin-rc-bionic
* install-bin-rc-focal
* install-bin-rc-fedora34
* install-opam-scratch
* install-opam-bionic
* install-opam-focal
* compile-release-sources-buster
* compile-master-sources-buster
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
        "use-docker" )
            # turn off variable expansion in here-script to protect $?:
            docker run --rm -i --privileged debian:buster <"$DOCS_DIR"/introduction/use-docker-ithacanet.sh
            ;;
        "install-bin-bionic" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_BIONIC /Scripts/install-bin-ubuntu.sh
            ;;
        "install-bin-focal" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_FOCAL /Scripts/install-bin-ubuntu.sh
            ;;
        "install-bin-fedora34" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts fedora:34 /Scripts/install-bin-fedora.sh
            ;;
        "install-bin-rc-bionic" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_BIONIC /Scripts/install-bin-ubuntu.sh rc
            ;;
        "install-bin-rc-focal" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts $UBUNTU_FOCAL /Scripts/install-bin-ubuntu.sh rc
            ;;
        "install-bin-rc-fedora34" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts fedora:34 /Scripts/install-bin-fedora.sh rc
            ;;
        "install-opam-scratch" )
            docker run --rm -i --privileged $UBUNTU_BIONIC <"$DOCS_DIR"/introduction/install-opam-scratch.sh
            ;;
        "install-opam-bionic" )
            docker run --rm -i ocaml/opam:ubuntu-18.04 <"$DOCS_DIR"/introduction/install-opam.sh
            ;;
        "install-opam-focal" )
            docker run --rm -i ocaml/opam:ubuntu-20.04 <"$DOCS_DIR"/introduction/install-opam.sh
            ;;
        "compile-release-sources-buster" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-10 /Scripts/compile-sources.sh latest-release
            ;;
        "compile-master-sources-buster" )
            docker run --rm -i -v "$DOCS_DIR/introduction":/Scripts ocaml/opam:debian-10 /Scripts/compile-sources.sh master
            ;;
        "install-python-bionic" )
            docker run --rm -i $UBUNTU_BIONIC <"$DOCS_DIR"/developer/install-python-debian-ubuntu.sh
            ;;
        "install-python-focal" )
            docker run --rm -i $UBUNTU_FOCAL <"$DOCS_DIR"/developer/install-python-debian-ubuntu.sh
            ;;
        "install-python-buster" )
            docker run --rm -i debian:buster <"$DOCS_DIR"/developer/install-python-debian-ubuntu.sh
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
