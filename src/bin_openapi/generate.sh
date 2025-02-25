#!/bin/sh

# This script launches a sandbox node, activates protocol $protocol_name, gets the RPC descriptions
# as JSON, and converts this JSON into an OpenAPI specification.
# You must compile the node and the client before running it.
#
# This script should be replaced by a Tezt script calling the test's core
# logic.

set -e

# Ensure we are running from the root directory of the Tezos repository.
cd "$(dirname "$0")"/../.. || exit

# Tezos binaries.
tezos_node=./octez-node
tezos_client=./octez-client
smart_rollup_node=./octez-smart-rollup-node
dal_node=./octez-dal-node

# Protocol configuration.
protocol_hash=ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
protocol_parameters=src/proto_alpha/parameters/sandbox-parameters.json
protocol_name=alpha

# Secret key to activate the protocol.
activator_secret_key="unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"

# RPC port.
rpc_port=8732
dal_rpc_port=10732

# Temporary files.
tmp=openapi-tmp
data_dir=$tmp/octez-sandbox
client_dir=$tmp/octez-client
dal_node_data_dir=$tmp/octez-dal-node
api_json=$tmp/rpc-api.json
proto_api_json=$tmp/proto-api.json
mempool_api_json=$tmp/mempool-api.json
dal_api_json=$tmp/dal-api.json

# Generated files.
openapi_json=docs/api/rpc-openapi-dev.json
proto_openapi_json=docs/api/$protocol_name-openapi.json
mempool_openapi_json=docs/api/$protocol_name-mempool-openapi.json
smart_rollup_node_openapi_json=docs/api/$protocol_name-smart-rollup-node-openapi.json
dal_node_openapi_json=docs/api/dal-node-openapi-dev.json

rm -rf "$tmp"

# Get version number.
version=$(dune exec octez-version -- --full-with-commit)

# Start a sandbox node.
$tezos_node config init --data-dir $data_dir \
  --network sandbox \
  --expected-pow 0 \
  --rpc-addr localhost:$rpc_port \
  --synchronisation-threshold 0 \
  --connections 0 \
  --history-mode rolling:19
$tezos_node identity generate --data-dir $data_dir
$tezos_node run --data-dir $data_dir --connections 0 --synchronisation-threshold 0 &
node_pid="$!"

# Wait for the node to be ready (sorry for the hackish way...)
sleep 3

# Activate the protocol.
mkdir $client_dir
$tezos_client --base-dir $client_dir --endpoint http://localhost:$rpc_port --block genesis activate protocol \
  $protocol_hash with fitness 1 and key $activator_secret_key and parameters $protocol_parameters
# Wait a bit again...
sleep 1

# Run a DAL node
mkdir $dal_node_data_dir
$dal_node config init --data-dir $dal_node_data_dir \
  --endpoint "http://localhost:$rpc_port" --expected-pow 0
$dal_node run --data-dir $dal_node_data_dir &
dal_node_pid="$!"

# Wait a bit again...
sleep 1

# Get the RPC descriptions.
curl "http://localhost:$rpc_port/describe/?recurse=yes" > $api_json
curl "http://localhost:$rpc_port/describe/chains/main/blocks/head?recurse=yes" > $proto_api_json
curl "http://localhost:$rpc_port/describe/chains/main/mempool?recurse=yes" > $mempool_api_json
curl "http://localhost:$dal_rpc_port/describe/?recurse=yes" > $dal_api_json

# Kill the nodes.
kill -9 "$node_pid"
kill -9 "$dal_node_pid"

# Remove RPC starting with "/private/"
clean_private_rpc() {
  jq 'delpaths([paths | select(.[-1] | strings | startswith("/private/"))])'
}

# Convert the RPC descriptions.
dune exec src/bin_openapi/rpc_openapi.exe -- \
  "$version" \
  "Octez RPC" \
  "The RPC API served by the Octez node." \
  $api_json |
  clean_private_rpc "$@" > $openapi_json
echo "Generated OpenAPI specification: $openapi_json"
dune exec src/bin_openapi/rpc_openapi.exe -- \
  "$version" \
  "Octez Protocol $protocol_name RPC" \
  "The RPC API for protocol $protocol_name served by the Octez node." \
  $proto_api_json |
  clean_private_rpc "$@" > $proto_openapi_json
echo "Generated OpenAPI specification: $proto_openapi_json"
dune exec src/bin_openapi/rpc_openapi.exe -- \
  "$version" \
  "Octez Mempool RPC" "The RPC API for the mempool served by the Octez node." \
  $mempool_api_json |
  clean_private_rpc "$@" > $mempool_openapi_json
echo "Generated OpenAPI specification: $mempool_openapi_json"
dune exec src/bin_openapi/rpc_openapi.exe -- \
  "$version" \
  "Octez DAL Node RPC" "The RPC API for the Octez DAL node." \
  $dal_api_json |
  clean_private_rpc "$@" > $dal_node_openapi_json
echo "Generated OpenAPI specification: $dal_node_openapi_json"

# Gernerate openapi file for rollup node
$smart_rollup_node generate openapi -P $protocol_hash > $smart_rollup_node_openapi_json
echo "Generated OpenAPI specification: $smart_rollup_node_openapi_json"

echo "You can now clean up with: rm -rf $tmp"
