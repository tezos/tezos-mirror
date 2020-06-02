#! /usr/bin/env bash

set -e

client_dirs=()

init_sandboxed_client() {

    id="$1"
    host="${2:-localhost}"
    shift 1

    rpc=$((18730 + id))
    client_dir="$(mktemp -d -t tezos-tmp-client.XXXXXXXX)"
    client_dirs+=("$client_dir")
    signer="$local_signer -d $client_dir"
    if [ -n "$USE_TLS" ]; then
        client="$local_client -base-dir $client_dir -endpoint https://$host:$rpc"
        admin_client="$local_admin_client -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_baker="$local_alpha_baker -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_endorser="$local_alpha_endorser -base-dir $client_dir -endpoint https://$host:$rpc"
        alpha_accuser="$local_alpha_accuser -base-dir $client_dir -endpoint https://$host:$rpc"
        signer="$local_signer -base-dir $client_dir -endpoint https://$host:$rpc"
        compiler="$local_compiler"
    else
        client="$local_client -base-dir $client_dir -endpoint http://$host:$rpc"
        admin_client="$local_admin_client -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_baker="$local_alpha_baker -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_endorser="$local_alpha_endorser -base-dir $client_dir -endpoint http://$host:$rpc"
        alpha_accuser="$local_alpha_accuser -base-dir $client_dir -endpoint http://$host:$rpc"
        signer="$local_signer -base-dir $client_dir -endpoint http://$host:$rpc"
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

# values from $src_dir/src/proto_alpha/lib_parameters/default_parameters.ml

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

BAKER1_IDENTITY="SG1fpFaowYY8G7PfkYdKkGmsMziHKUfrHRHW"
BAKER1_PUBLIC="edpkuDjETsuw7Tve4hczaEiDd5q8q5CnzppnoENpvf3hSt2g2N6fy5"
BAKER1_SECRET="unencrypted:edsk432L71B91i1sE8rQxPDMo2Yxo4qaYqhktvpt8yovaMpo1NUbBt"

BAKER2_IDENTITY="SG1TLmKJHVJxQosY6iN21AW77HsAapdupxnR"
BAKER2_PUBLIC="edpkuDDr1FWd79GdjtcRAYJ7zufsxHUpCbtjPvQ2SGzp5QGBV6jFXo"
BAKER2_SECRET="unencrypted:edsk3jjD4cLvE1t3SiZKCRiH9SXnJ4jHEsVpfcuRwfe7sfbnsMGX6f"

BAKER3_IDENTITY="SG1hExdK69Z2RZkkQjKtLG6H4L4FGTZeGKHu"
BAKER3_PUBLIC="edpkuxfqVAGvJegMn8h4Qsrfc6ncLYQsAWKaAx2SieFn2Mz5QxNSmL"
BAKER3_SECRET="unencrypted:edsk3EVDFeDsefk1UoWWhDhnBpERhAwqHbVzWnBwA4tdUL8aBhw7RC"

BAKER4_IDENTITY="SG1mHgeWHGMnCUMJ8jZ1Cdh3DkWEcQ88tziJ"
BAKER4_PUBLIC="edpku5MdpRkypXjBpyQ5vEbf3iUkRbt7iVjKvyx7RrkWSXpHS4fSck"
BAKER4_SECRET="unencrypted:edsk2rAmeBDrQ5d1FhDoTdifMvBsqSosmPSBhxL74huszZhfMFtiKb"

BAKER5_IDENTITY="SG1jfZeHRzeWAM1T4zrwunEyUpwWc82D4tbv"
BAKER5_PUBLIC="edpkvFYoA3cwPyVMAheyX29pQFw9gwfxDHJgbthG3caZNuQFNzrwkM"
BAKER5_SECRET="unencrypted:edsk4Ssc63dnYJUptVMaKfmHLJNMBYv9piYiRCiYjcH3gacKWP1z3v"

ACTIVATOR_SECRET="unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"

add_sandboxed_bootstrap_identities() {

    ${client} import secret key bootstrap1 ${BOOTSTRAP1_SECRET}
    ${client} import secret key bootstrap2 ${BOOTSTRAP2_SECRET}
    ${client} import secret key bootstrap3 ${BOOTSTRAP3_SECRET}
    ${client} import secret key bootstrap4 ${BOOTSTRAP4_SECRET}
    ${client} import secret key bootstrap5 ${BOOTSTRAP5_SECRET}

    ${client} import secret key baker1_key ${BAKER1_SECRET}
    ${client} import secret key baker2_key ${BAKER2_SECRET}
    ${client} import secret key baker3_key ${BAKER3_SECRET}
    ${client} import secret key baker4_key ${BAKER4_SECRET}
    ${client} import secret key baker5_key ${BAKER5_SECRET}

    ${client} import secret key activator ${ACTIVATOR_SECRET}
}

client_remember_baker_contracts() {

    $1 remember contract baker1 ${BAKER1_IDENTITY};
    $1 remember contract baker2 ${BAKER2_IDENTITY};
    $1 remember contract baker3 ${BAKER3_IDENTITY};
    $1 remember contract baker4 ${BAKER4_IDENTITY};
    $1 remember contract baker5 ${BAKER5_IDENTITY};
}

remember_baker_contracts() {

    # skip the last client, which activates alpha protocol
    clients=("${client_instances[@]::${#client_instances[@]}-1}")
    for client in "${clients[@]}"; do
        client_remember_baker_contracts "$client"
    done
}

activate_alpha() {

    ${client} \
        -block genesis \
        activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
        with fitness 1 \
        and key activator \
        and parameters "${parameters_file}" \
        --timestamp $(TZ='AAA+1' date +%FT%TZ)

    client_remember_baker_contracts "$client"
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
        local_compiler="${local_compiler:-$bin_dir/../../_build/default/src/lib_protocol_compiler/main_native.exe}"

        parameters_file="$bin_dir/../proto_alpha/parameters/sandbox-parameters.json"

    else
        # we assume a clean install with tezos-(admin-)client in the path
        local_client="${local_client:-$(which tezos-client)}"
        local_admin_client="${local_admin_client:-$(which tezos-admin-client)}"
        local_signer="${local_signer:-$(which tezos-signer)}"
        local_compiler="${local_compiler:-$(which tezos-protocol-compiler)}"
    fi

    if [ $# -lt 1 ] || [ "$1" -le 0 ] || [ 10 -le "$1" ]; then
        usage
        exit 1
    fi

    init_sandboxed_client "$1" "$2"

    add_sandboxed_bootstrap_identities | sed -e 's/^/## /' 1>&2

    mkdir -p $client_dir/bin

    echo '#!/bin/sh' > $client_dir/bin/tezos-client
    echo "exec $client \"\$@\"" >> $client_dir/bin/tezos-client
    chmod +x $client_dir/bin/tezos-client

    echo '#!/bin/sh' > $client_dir/bin/tezos-admin-client
    echo "exec $admin_client \"\$@\""  >> $client_dir/bin/tezos-admin-client
    chmod +x $client_dir/bin/tezos-admin-client

    for protocol in $(cat $bin_dir/../../active_protocol_versions); do
        protocol_underscore=$(echo $protocol | tr -- - _)
        local_baker="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_baker/main_baker_$protocol_underscore.exe"
        local_endorser="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_endorser/main_endorser_$protocol_underscore.exe"
        local_accuser="$bin_dir/../../_build/default/src/proto_$protocol_underscore/bin_accuser/main_accuser_$protocol_underscore.exe"

        if [ -n "$USE_TLS" ]; then
            baker="$local_baker -base-dir $client_dir -endpoint https://$host:$rpc"
            endorser="$local_endorser -base-dir $client_dir -endpoint https://$host:$rpc"
            accuser="$local_accuser -base-dir $client_dir -endpoint https://$host:$rpc"
        else
            baker="$local_baker -base-dir $client_dir -endpoint http://$host:$rpc"
            endorser="$local_endorser -base-dir $client_dir -endpoint http://$host:$rpc"
            accuser="$local_accuser -base-dir $client_dir -endpoint http://$host:$rpc"
        fi

        echo '#!/bin/sh' > $client_dir/bin/tezos-baker-$protocol
        echo "exec $baker \"\$@\""  >> $client_dir/bin/tezos-baker-$protocol
        chmod +x $client_dir/bin/tezos-baker-$protocol

        echo '#!/bin/sh' > $client_dir/bin/tezos-endorser-$protocol
        echo "exec $endorser \"\$@\""  >> $client_dir/bin/tezos-endorser-$protocol
        chmod +x $client_dir/bin/tezos-endorser-$protocol

        echo '#!/bin/sh' > $client_dir/bin/tezos-accuser-$protocol
        echo "exec $accuser \"\$@\""  >> $client_dir/bin/tezos-accuser-$protocol
        chmod +x $client_dir/bin/tezos-accuser-$protocol
    done

    echo '#!/bin/sh' > $client_dir/bin/tezos-signer
    echo "exec $signer \"\$@\""  >> $client_dir/bin/tezos-signer
    chmod +x $client_dir/bin/tezos-signer

    # we can 'remember contract' only after protocol has been activated
    ADD_BAKER_CONTRACTS="${client} remember contract baker1 ${BAKER1_IDENTITY};\
                         ${client} remember contract baker2 ${BAKER2_IDENTITY};\
                         ${client} remember contract baker3 ${BAKER3_IDENTITY};\
                         ${client} remember contract baker4 ${BAKER4_IDENTITY};\
                         ${client} remember contract baker5 ${BAKER5_IDENTITY};"

    cat <<EOF
if type tezos-client-reset >/dev/null 2>&1 ; then tezos-client-reset; fi ;
PATH="$client_dir/bin:\$PATH" ; export PATH ;
alias tezos-activate-alpha="$client  -block genesis activate protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK with fitness 1 and key activator and parameters $parameters_file --timestamp $(TZ='AAA+1' date +%FT%TZ); ${ADD_BAKER_CONTRACTS}" ;
alias tezos-client-reset="rm -rf \"$client_dir\"; unalias tezos-activate-alpha tezos-client-reset" ;
alias tezos-autocomplete="if [ \$ZSH_NAME ] ; then autoload bashcompinit ; bashcompinit ; fi ; source \"$bin_dir/bash-completion.sh\"" ;
trap tezos-client-reset EXIT ;

EOF

    (cat | sed -e 's/^/## /') 1>&2 <<EOF

The client is now properly initialized. In the rest of this shell
session, you might now run \`tezos-client\` to communicate with a
tezos node launched with \`launch-sandboxed-node $1\`. For instance:

  tezos-client rpc get /chains/main/blocks/head/metadata

Note: if the current protocol version, as reported by the previous
command, is "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im", you
may have to activate in your "sandboxed network" the same economic
protocol as used by the alphanet by running:

  tezos-activate-alpha

Warning: all the client data will be removed when you close this shell
or if you run this command a second time.

Activate tab completion by running:

  tezos-autocomplete

EOF

}

if [ "$0" == "$BASH_SOURCE" ]; then
    main "$@"
fi
