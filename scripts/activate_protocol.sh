#! /bin/bash

set -e

usage="Usage:
$ ./scripts/activate_protocol.sh src/proto_004_PtDPBVyN
Inserts the protocol in the right files of the build system to compile it
Activate in addition to its predecessor, here proto_003_PsddFKi3."

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
cd "$script_dir"/..

if [ ! -d "$1" ]; then
    echo "$usage"
    exit 1
fi

new_version=$(basename $1 | awk -F'_' '{print $2}')
new_hash=$(basename $1 | awk -F'_' '{print $3}')
full_hash=$(jq .hash < $1/lib_protocol/TEZOS_PROTOCOL)
replacement=${new_version}-${new_hash}
if [[ -z "${new_version}" || -z "${new_hash}" || -z "${full_hash}" ]] ; then
    echo "$usage"
    exit 1
fi

# The pattern to look for is "00X-<hash>".
# Once found it's either replaced or the line is duplicated and then replaced
old_version=$( printf '%03d' $(($new_version -1)) )
old_dir=$(ls -d src/proto_${old_version}_*)
old_hash=$(basename $old_dir | awk -F'_' '{print $3}')
pattern=${old_version}-${old_hash}

# if a line matches PATTERN, a new line is printed where the pattern is replaced
duplicate_and_replace() {
    PATTERN=$1
    REPLACEMENT=$2
    shift 2

    awk -i inplace '{
        print
        if ($0 ~ PATTERN) {
           sub(PATTERN,REPLACEMENT)
           print
        }}' PATTERN=$PATTERN REPLACEMENT=$REPLACEMENT $*
}

# the minimum needed, although you can't bake
duplicate_and_replace ${pattern} ${replacement} active_protocol_versions

# activate in client to bake and use RPCs
duplicate_and_replace -${pattern} -${replacement} \
    src/bin_client/{dune,tezos-client.opam}

# activate in node
duplicate_and_replace -${pattern} -${replacement} \
    src/bin_node/{dune,tezos-node.opam}
duplicate_and_replace -${pattern} -${replacement} \
    src/bin_validation/{dune,tezos-validator.opam}
