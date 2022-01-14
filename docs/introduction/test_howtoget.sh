#! /usr/bin/env bash
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
MYDIR=$(dirname "$0")

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
* compile-sources-buster
!EOF
}

set -e

case "$1" in

  "use-docker" )
    # turn off variable expansion in here-script to protect $?:
    docker run --rm -i --privileged debian:buster <"$MYDIR"/use-docker-delphinet.sh
    ;;
  "install-bin-bionic" )
    docker run --rm -i $UBUNTU_BIONIC <"$MYDIR"/install-bin-ubuntu.sh
    ;;
  "install-bin-focal" )
    docker run --rm -i $UBUNTU_FOCAL <"$MYDIR"/install-bin-ubuntu.sh
    ;;
  "install-bin-fedora34" )
    docker run --rm -i fedora:34 <"$MYDIR"/install-bin-fedora.sh
    ;;
  "install-bin-rc-bionic" )
    docker run --rm -i $UBUNTU_BIONIC <"$MYDIR"/install-bin-rc-ubuntu.sh
    ;;
  "install-bin-rc-focal" )
    docker run --rm -i $UBUNTU_FOCAL <"$MYDIR"/install-bin-rc-ubuntu.sh
    ;;
  "install-bin-rc-fedora34" )
    docker run --rm -i fedora:34 <"$MYDIR"/install-bin-rc-fedora.sh
    ;;
  "install-opam-scratch" )
    docker run --rm -i --privileged $UBUNTU_BIONIC <"$MYDIR"/install-opam-scratch.sh
    ;;
  "install-opam-bionic" )
    docker run --rm -i ocaml/opam:ubuntu-18.04 <"$MYDIR"/install-opam.sh
    ;;
  "install-opam-focal" )
    docker run --rm -i ocaml/opam:ubuntu-20.04 <"$MYDIR"/install-opam.sh
    ;;
  "compile-sources-buster" )
    docker run --rm -i ocaml/opam:debian-10 <"$MYDIR"/compile-sources.sh
    ;;
  * )
    echo "unknown test name: '$1'"
    usage
    exit 1 ;;

esac
echo "test $1 returned $?"
echo "*******************************************************************"
exit 0
