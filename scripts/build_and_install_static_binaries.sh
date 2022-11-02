#!/usr/bin/env sh

# Used in the CI to build and install the static binaries.

set -eu

if [ $# -ne 1 ]; then
    echo "usage: $0 DESTDIR"
    exit 1
fi

tmp_dir=$(mktemp -dt tezos_static_install.XXXXXXXX)
cleanup () {
    set +e
    echo Cleaning up...
    rm -rf "$tmp_dir"
}
trap cleanup EXIT INT

# shellcheck disable=SC2046
dune build   --profile static $(xargs -I {} echo {}.install < script-inputs/static-packages)
# shellcheck disable=SC2046
dune install --profile static --prefix "$tmp_dir" $(cat script-inputs/static-packages)

mv "$tmp_dir/bin" "$1"

# Make sure binaries are statically linked
find -L "$1" -type f -not -name "*.sh" |
while read -r b; do
    file "$(realpath "$b")" | grep "statically linked";
done

# Make sure octez-client knows about build-infos
SHA=$(git rev-parse --short=8 HEAD)
client_version=$("$1/octez-client" --version | cut -f 1 -d ' ')
if [ "$SHA" != "$client_version" ]; then
    echo "Unexpected version for octez-client (expected $SHA, found $client_version)"
    exit 1
fi
echo "octez-client --version returned the expected commit hash: $SHA"
