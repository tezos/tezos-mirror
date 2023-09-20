#!/usr/bin/env bash

set -e

client_dirs=()

init_sandboxed_client() {

    id="$1"
    host="${2:-localhost}"
    shift 1

    rpc=$((18730 + id))
    client_dir="$(mktemp -d -t tezos-tmp-client.XXXXXXXX)"
    client_dirs+=("$client_dir")
    if [ -n "$SCORU_DATA_DIR" ]; then
        rollup_node_dir="$SCORU_DATA_DIR"
    else
        rollup_node_dir="$(mktemp -d -t tezos-smart-rollup-node.XXXXXXXX)"
    fi
    signer="$local_signer -d $client_dir"
    if [ -n "$USE_TLS" ]; then
        client="$local_client -base-dir $client_dir -endpoint https://$host:$rpc"
        admin_client="$local_admin_client -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_baker="$local_alpha_baker -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_accuser="$local_alpha_accuser -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_sc_rollup_node="$local_alpha_sc_rollup_node -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_sc_rollup_client="$local_alpha_sc_rollup_client -base-dir $client_dir -endpoint https://$host:$rpc"
        compiler="$local_compiler"
    else
        client="$local_client -base-dir $client_dir -endpoint http://$host:$rpc"
        admin_client="$local_admin_client -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_baker="$local_alpha_baker -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_accuser="$local_alpha_accuser -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_sc_rollup_node="$local_alpha_sc_rollup_node -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_sc_rollup_client="$local_alpha_sc_rollup_client -base-dir $client_dir -endpoint http://$host:$rpc"
        compiler="$local_compiler"
    fi
}

cleanup_clients() {
    rm -rf "${client_dirs[@]}"
}


## Waiter ##################################################################

wait_for_the_node_to_be_ready() {
    local count=0
    if $client rpc get /chains/main/blocks/head/hash >/dev/null 2>&1; then return; fi
    printf "Waiting for the node to initialize..."
    sleep 1
    while ! $client rpc get /chains/main/blocks/head/hash >/dev/null 2>&1
    do
        count=$((count+1))
        if [ "$count" -ge 30 ]; then
            echo " timeout."
            exit 2
        fi
        printf "."
        sleep 1
    done
    echo " done."
}

wait_for_the_node_to_be_bootstrapped() {
    wait_for_the_node_to_be_ready
    echo "Waiting for the node to synchronize with the network..."
    $client bootstrapped
}

## Sandboxed client ########################################################

# key pairs from $src_dir/test/sandbox.json

BOOTSTRAP1_IDENTITY="tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
BOOTSTRAP1_PUBLIC="edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"
BOOTSTRAP1_SECRET="unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"

BOOTSTRAP2_IDENTITY="tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN"
BOOTSTRAP2_PUBLIC="edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9"
BOOTSTRAP2_SECRET="unencrypted:edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo"

BOOTSTRAP3_IDENTITY="tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
BOOTSTRAP3_PUBLIC="edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV"
BOOTSTRAP3_SECRET="unencrypted:edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ"

BOOTSTRAP4_IDENTITY="tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv"
BOOTSTRAP4_PUBLIC="edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU"
BOOTSTRAP4_SECRET="unencrypted:edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3"

BOOTSTRAP5_IDENTITY="tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"
BOOTSTRAP5_PUBLIC="edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n"
BOOTSTRAP5_SECRET="unencrypted:edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm"

ACTIVATOR_SECRET="unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"

add_sandboxed_bootstrap_identities() {

    ${client} import secret key bootstrap1 ${BOOTSTRAP1_SECRET}
    ${client} import secret key bootstrap2 ${BOOTSTRAP2_SECRET}
    ${client} import secret key bootstrap3 ${BOOTSTRAP3_SECRET}
    ${client} import secret key bootstrap4 ${BOOTSTRAP4_SECRET}
    ${client} import secret key bootstrap5 ${BOOTSTRAP5_SECRET}

    ${client} import secret key activator ${ACTIVATOR_SECRET}
}

add_liquidity_baking_default_file() {

    echo '{ "liquidity_baking_toggle_vote": "pass" }' > ${client_dir}/per_block_votes.json

}

activate_alpha() {

    # Calling `date` with 'AAA+1' is a small tweak to speed-up
    # the block baking process. Having a one-hour back timestamp
    # avoids having to wait for at least [time_between_block] to
    # produce new blocks.
    ${client} \
        -block genesis \
        activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
        with fitness 1 \
        and key activator \
        and parameters "${parameters_file}"
}

usage() {
    echo "Small script to initialize a client to a local and closed test network with a maximum of 9 nodes."
    echo
    echo "Usage: eval \`$0 <id>\`"
    echo "  where <id> should be an integer between 1 and 9."
}

main () {

    local bin_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
    if [ $(basename "$bin_dir") = "bin_client" ]; then
        local_client="${local_client:-$bin_dir/../../_build/default/src/bin_client/main_client.exe}"
        local_admin_client="${local_admin_client:-$bin_dir/../../_build/default/src/bin_client/main_admin.exe}"
        local_signer="${local_signer:-$bin_dir/../../_build/default/src/bin_signer/main_signer.exe}"
        local_compiler="${local_compiler:-$bin_dir/../../_build/default/src/lib_protocol_compiler/bin/main_native.exe}"

        parameters_file="$bin_dir/../../_build/default/src/proto_alpha/lib_parameters/sandbox-parameters.json"

    else
        # we assume a clean install with octez-(admin-)client in the path
        local_client="${local_client:-$(which octez-client)}"
        local_admin_client="${local_admin_client:-$(which octez-admin-client)}"
        local_signer="${local_signer:-$(which octez-signer)}"
        local_compiler="${local_compiler:-$(which octez-protocol-compiler)}"
    fi

    if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
        usage
        exit 1
    fi

    init_sandboxed_client "$1" "$2"

    add_sandboxed_bootstrap_identities | sed -e 's/^/## /' 1>&2

    add_liquidity_baking_default_file

    mkdir -p $client_dir/bin

    echo '#!/bin/sh' > $client_dir/bin/octez-client
    echo "exec $client \"\$@\"" >> $client_dir/bin/octez-client
    chmod +x $client_dir/bin/octez-client

    echo '#!/bin/sh' > $client_dir/bin/octez-admin-client
    echo "exec $admin_client \"\$@\""  >> $client_dir/bin/octez-admin-client
    chmod +x $client_dir/bin/octez-admin-client

    for protocol in $(cat $bin_dir/../../script-inputs/active_protocol_versions); do
        protocol_underscore=$(echo $protocol | tr -- - _)
        protocol_without_number=$(echo $protocol | tr -d "\-[0-9]")
        local_baker="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_baker/main_baker_$protocol_underscore.exe"
        local_accuser="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_accuser/main_accuser_$protocol_underscore.exe"
        local_sc_rollup_node="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_sc_rollup_node/main_sc_rollup_node_$protocol_underscore.exe"
        local_sc_rollup_client="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_sc_rollup_client/main_sc_rollup_client_$protocol_underscore.exe"

        if [ -n "$USE_TLS" ]; then
            baker="$local_baker -base-dir $client_dir -endpoint https://$host:$rpc"
            accuser="$local_accuser -base-dir $client_dir -endpoint https://$host:$rpc"
            sc_rollup_node="$local_sc_rollup_node -base-dir $client_dir -endpoint https://$host:$rpc"
            sc_rollup_client="$local_sc_rollup_client -base-dir $client_dir -endpoint https://$host:$rpc"
        else
            baker="$local_baker -base-dir $client_dir -endpoint http://$host:$rpc"
            accuser="$local_accuser -base-dir $client_dir -endpoint http://$host:$rpc"
            sc_rollup_node="$local_sc_rollup_node -base-dir $client_dir -endpoint http://$host:$rpc"
            sc_rollup_client="$local_sc_rollup_client -base-dir $client_dir -endpoint http://$host:$rpc"
        fi

        echo '#!/bin/sh' > $client_dir/bin/octez-baker-$protocol_without_number
        echo "exec $baker \"\$@\""  >> $client_dir/bin/octez-baker-$protocol_without_number
        chmod +x $client_dir/bin/octez-baker-$protocol_without_number


        echo '#!/bin/sh' > $client_dir/bin/octez-smart-rollup-node-$protocol_without_number
        echo "exec $sc_rollup_node \"\$@\" -data-dir $rollup_node_dir"  >> $client_dir/bin/octez-smart-rollup-node-$protocol_without_number
        chmod +x $client_dir/bin/octez-smart-rollup-node-$protocol_without_number

        echo '#!/bin/sh' > $client_dir/bin/octez-smart-rollup-client-$protocol_without_number
        echo "exec $sc_rollup_client \"\$@\""  >> $client_dir/bin/octez-smart-rollup-client-$protocol_without_number
        chmod +x $client_dir/bin/octez-smart-rollup-client-$protocol_without_number

        echo '#!/bin/sh' > $client_dir/bin/octez-accuser-$protocol_without_number
        echo "exec $accuser \"\$@\""  >> $client_dir/bin/octez-accuser-$protocol_without_number
        chmod +x $client_dir/bin/octez-accuser-$protocol_without_number
    done

    echo '#!/bin/sh' > $client_dir/bin/octez-signer
    echo "exec $signer \"\$@\""  >> $client_dir/bin/octez-signer
    chmod +x $client_dir/bin/octez-signer

    cat <<EOF
if type octez-client-reset >/dev/null 2>&1 ; then octez-client-reset; fi ;
PATH="$client_dir/bin:\$PATH" ; export PATH ;
alias octez-activate-alpha="$client  -block genesis activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK with fitness 1 and key activator and parameters $parameters_file" ;
alias octez-client-reset="rm -rf \"$client_dir\"; unalias octez-activate-alpha octez-client-reset" ;
alias octez-autocomplete="if [ \$ZSH_NAME ] ; then autoload bashcompinit ; bashcompinit ; fi ; source \"$bin_dir/bash-completion.sh\"" ;
trap octez-client-reset EXIT ;

EOF

    (cat | sed -e 's/^/## /') 1>&2 <<EOF

The client is now properly initialized. In the rest of this shell
session, you might now run \`octez-client\` to communicate with a
tezos node launched with \`launch-sandboxed-node $1\`. For instance:

  octez-client rpc get /chains/main/blocks/head/metadata

Note: if the current protocol version, as reported by the previous
command, is "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im", you
may have to activate in your "sandboxed network" the same economic
protocol as used by the alphanet by running:

  octez-activate-alpha

Warning: all the client data will be removed when you close this shell
or if you run this command a second time.

Activate tab completion by running:

  octez-autocomplete

EOF

}

if [ "$0" == "$BASH_SOURCE" ]; then
    main "$@"
fi
