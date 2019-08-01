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

$client gen keys $key1
$client gen keys $key2 --sig secp256k1
$client gen keys $key3 --sig ed25519

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
