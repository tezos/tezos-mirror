#! /usr/bin/env bash

set -e

test_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"
source $test_dir/test_lib.inc.sh "$@"

start_node 1
activate_alpha

$client -w none config update

sleep 2

#tests for the rpc service raw_context
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent' | assert 'No service found at this URL'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/delegates/?depth=3' | assert '{ "ed25519":
    { "02": { "29": null }, "a9": { "ce": null }, "c5": { "5c": null },
      "da": { "c9": null }, "e7": { "67": null } } }'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent?depth=-1' | assert 'Command failed : Extraction depth -1 is invalid'
$client rpc get '/chains/main/blocks/head/context/raw/bytes/non-existent?depth=0' | assert 'No service found at this URL'

bake

key1=foo
key2=bar
key3=boo
key4=king
key5=queen
# key6=p256

$client gen keys $key1
$client gen keys $key2 --sig secp256k1
$client gen keys $key3 --sig ed25519
# $client gen keys $key6 --sig p256

$client list known addresses
$client get balance for bootstrap1

bake_after $client transfer 1,000 from bootstrap1 to $key1 --burn-cap 0.257
bake_after $client transfer 2,000 from bootstrap1 to $key2 --burn-cap 0.257
bake_after $client transfer 3,000 from bootstrap1 to $key3 --burn-cap 0.257
# bake_after $client transfer 4,000 from bootstrap1 to $key6

$client get balance for $key1 | assert "1000 ꜩ"
$client get balance for $key2 | assert "2000 ꜩ"
$client get balance for $key3 | assert "3000 ꜩ"

$client rpc post /chains/main/mempool/filter with \
        '{ "minimal_fees": "0", "minimal_nanotez_per_byte": "0", "minimal_nanotez_per_gas_unit": "0"  }'
bake_after $client transfer 1,000 from $key2 to $key1 --fee 0 --force-low-fee
$client get balance for $key1 | assert "2000 ꜩ"
$client get balance for $key2 | assert "1000 ꜩ"
$client rpc post /chains/main/mempool/filter with '{}'

bake_after $client transfer 1,000 from $key1 to $key2 --fee 0.05
$client get balance for $key1 | assert "999.95 ꜩ"
$client get balance for $key2 | assert "2000 ꜩ"

# Should fail
# $client transfer 999.95 from $key2 to $key1

bake

$client remember script noop file:contracts/opcodes/noop.tz
$client typecheck script file:contracts/opcodes/noop.tz
bake_after $client originate contract noop \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/opcodes/noop.tz --burn-cap 0.295

bake_after $client transfer 10 from bootstrap1 to noop --arg "Unit"


bake_after $client originate contract hardlimit \
        for $key1 transferring 1,000 from bootstrap1 \
        running file:contracts/mini_scenarios/hardlimit.tz --init "3" --burn-cap 0.341
bake_after $client transfer 10 from bootstrap1 to hardlimit --arg "Unit"
bake_after $client transfer 10 from bootstrap1 to hardlimit --arg "Unit"

bake_after $client originate account free_account for $key1 \
        transferring 1,000 from bootstrap1 --delegatable --burn-cap 0.257
$client get delegate for free_account

bake_after $client register key $key2 as delegate
bake_after $client set delegate for free_account to $key2
$client get delegate for free_account

$client rpc post /chains/main/mempool/filter with \
        '{ "minimal_fees": "0", "minimal_nanotez_per_byte": "0", "minimal_nanotez_per_gas_unit": "0"  }'
$client get balance for bootstrap5 | assert "4000000 ꜩ"
bake_after $client transfer 400,000 from bootstrap5 to bootstrap1 --fee 0 --force-low-fee
bake_after $client transfer 400,000 from bootstrap1 to bootstrap5 --fee 0 --force-low-fee
$client get balance for bootstrap5 | assert "4000000 ꜩ"
$client rpc post /chains/main/mempool/filter with '{}'

bake_after $client activate account $key4 with king_commitment.json
bake_after $client activate account $key5 with queen_commitment.json

$client get balance for $key4 | assert "23932454.669343 ꜩ"
$client get balance for $key5 | assert "72954577.464032 ꜩ"

bake_after $client transfer 10 from $key4 to $key5

echo
echo "---- Multisig ----"
echo
echo "-- Origination --"
echo

bake_after $client deploy multisig msig for bootstrap1 transferring 100 from bootstrap1 with threshold 2 on public keys $key1 $key2 $key3 --burn-cap 100

echo
echo "-- Transfer --"
echo

# There are two ways to sign a transaction, if the signer uses a
# client with multisig commands, it can simply call "tezos-client sign
# multisig transaction ..."

SIG1=$($client sign multisig transaction on msig transferring 10 to bootstrap2 using secret key $key1)

## Otherwise, the command "tezos-client prepare multisig transaction
## ..."  can be used to display the sequence of bytes to be signed
## using "tezos-client sign bytes ..."

$client prepare multisig transaction on msig transferring 10 to bootstrap2

TOSIGN=$($client prepare multisig transaction on msig transferring 10 to bootstrap2 --bytes-only)

SIG2=$($client sign bytes "$TOSIGN" for $key2 | cut -d ' ' -f 2)
SIG3=$($client sign bytes "$TOSIGN" for $key3 | cut -d ' ' -f 2)

# Should fail (not enough signatures are provided)
$client from multisig contract msig transfer 10 to bootstrap2 on behalf of bootstrap1 with signatures "$SIG2" |& head -n 1 | assert 'Not enough signatures: only 1 signatures were given but the threshold is currently 2'

# Should succeed
bake_after $client from multisig contract msig transfer 10 to bootstrap2 on behalf of bootstrap1 with signatures "$SIG1" "$SIG3"

echo
echo "-- Delegate change --"
echo

SIG1=$($client sign multisig transaction on msig setting delegate to bootstrap5 using secret key $key1)
TOSIGN=$($client prepare multisig transaction on msig setting delegate to bootstrap5 --bytes-only)
SIG3=$($client sign bytes "$TOSIGN" for $key3 | cut -d ' ' -f 2)
bake_after $client set delegate of multisig contract msig to bootstrap5 on behalf of bootstrap1 with signatures "$SIG1" "$SIG3"

echo
echo "-- Delegate withdraw --"
echo

SIG1=$($client sign multisig transaction on msig withdrawing delegate using secret key $key1)
TOSIGN=$($client prepare multisig transaction on msig withdrawing delegate --bytes-only)
SIG3=$($client sign bytes "$TOSIGN" for $key3 | cut -d ' ' -f 2)

bake_after $client withdraw delegate of multisig contract msig on behalf of bootstrap1 with signatures "$SIG1" "$SIG3"

echo
echo "-- Change of keys and threshold --"
echo

## Test of the signature change: bootstrap1 and bootstrap3 kick bootstrap2 out of the signers.

SIG1=$($client sign multisig transaction on msig using secret key $key1 setting threshold to 2 and public keys to $key1 $key3)
TOSIGN=$($client prepare multisig transaction on msig setting threshold to 2 and public keys to $key1 $key3 --bytes-only)
SIG3=$($client sign bytes "$TOSIGN" for $key3 | cut -d ' ' -f 2)
bake_after $client run transaction "$TOSIGN" on multisig contract msig on behalf of bootstrap1 with signatures "$SIG1" "$SIG3"

echo
echo End of test
echo

show_logs="no"
