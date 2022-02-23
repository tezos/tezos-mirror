#!/bin/sh

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <docker-image> [expected-version-sha] [version|node_run ...]"
    echo
    echo 'This script is used to test Octez Docker images. It can run two test cases:'
    echo
    echo " - version: this test simply verifies that the Octez binaries"
    echo "   contained in the image execute when passed '--version' and that the"
    echo "   reported version is as expected"
    echo
    echo " - node_run: this test verifies that 'tezos-docker-manager.sh' is able"
    echo "   to launch with the image, and that the contained node reaches the"
    echo "   running state"
    exit 1
fi

IMAGE=$1
shift
SHA=${1:-$(git rev-parse --short=8 HEAD)}
shift
TESTS=${*:-version node_run}

test_version() {
    echo "Testing version of binaries in Docker image"

    binaries=$(docker run --entrypoint ls "$IMAGE" /usr/local/bin/)
    for bin in tezos-client tezos-node; do
        if ! echo "$binaries" | grep -q "^${bin}$" ; then
            echo "Could not find binary $bin in Docker image."
            exit 1
        else
            echo "Found binary $bin in Docker image"
        fi
    done

    for binary in $(echo "$binaries" | grep tezos-); do
        printf "Checking version of %s..." "$binary"

        cmd="docker run --entrypoint $binary $IMAGE --version"
        node_sha_in_docker=$($cmd | cut -d' ' -f 1)

        if [ "$node_sha_in_docker" != "$SHA" ]; then
            echo "NOK"
            echo
            echo "Expected the version of Docker tezos-node to reference commit $SHA, got $node_sha_in_docker"
            echo "when executing '$cmd'"
            exit 1
        fi

        echo " OK!"
    done
}


test_node_run_clean_up() {
    set +e

    echo "## Clean up"
    if [ -f /tmp/mainnet.sh ]; then
        /tmp/mainnet.sh stop
        rm /tmp/mainnet.sh
    fi
}

test_node_run() {
    echo "Testing running a node using scripts/tezos-docker-manager.sh using Docker image"
    trap test_node_run_clean_up EXIT INT

    cp scripts/tezos-docker-manager.sh /tmp/mainnet.sh
    chmod +x /tmp/mainnet.sh
    sed -i "s,docker_image=.*,docker_image=$IMAGE," /tmp/mainnet.sh
    /tmp/mainnet.sh start --expected-pow=-0.0
    if ! timeout 60 ./mainnet.sh node log | grep 'Tezos node is now running'; then
        echo "Could not verify that node is running."
        exit 1
    fi

    echo "OK"
}

echo "Smoke testing docker image $IMAGE"

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
    esac
done
