#!/bin/sh

set -e

if [ -z "$1" ]; then
  echo "Usage: $0 [x86_64|arm64] <static-binary> [version|node_run ...]"
  echo
  echo " - version: this test simply verifies that the Octez binaries"
  echo "   execute correctly when passed '--version' and that the"
  echo "   reported version is as expected"
  echo
  echo " - node_run: this test verifies that the node reaches the"
  echo "   running state"
  exit 1
fi

ARCH=${1}
shift
TESTS=${*:-version node_run}

SHA=$(git rev-parse --short=8 HEAD)

ci_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$ci_dir")"
src_dir="$(dirname "$script_dir")"
bin_dir="$src_dir/octez-binaries/$ARCH"

test_version() {
  echo "Testing version of static binaries"

  for binary in octez-client octez-signer; do
    printf "Checking version of %s..." "$binary"

    cmd="$bin_dir/$binary --version"
    ressha=$($cmd | cut -d' ' -f 1)

    if [ "$ressha" != "$SHA" ]; then
      echo "NOK"
      echo
      echo "Expected the version reference commit $SHA, got $ressha"
      echo "when executing '$cmd'"
      exit 1
    fi

    echo " OK!"
  done
}

test_node_run() {
  echo "Testing running a node"

  "$script_dir/install_sapling_parameters.sh"

  if ! timeout 20 "$bin_dir/octez-node" run --expected-pow=-0.0 | grep 'Tezos node is now running'; then
    echo "Could not verify that node is running."
    exit 1
  fi

  echo "OK"
}

echo "Smoke testing static binaries"

for test_case in $TESTS; do
  case $test_case in
  version)
    test_version
    ;;
  node_run)
    test_node_run
    ;;
  *)
    echo "Unknown test '$test_case'. Should be one of: version, node_run"
    ;;
  esac
done
