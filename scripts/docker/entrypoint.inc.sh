#!/bin/sh

# make sure all these variables are defined in the parent script
node=${node:?}
client=${client:?}
admin_client=${admin_client:?}
baker=${baker:?}
endorser=${endorser:?}
accuser=${accuser:?}
signer=${signer:?}
smart_rollup_node=${smart_rollup_node:?}
client_dir=${client_dir:?}
node_dir=${node_dir:?}
node_data_dir=${node_data_dir:?}
smart_rollup_node_data_dir=${smart_rollup_node_data_dir:?}

configure_client() {

  _client_config="$HOME/.tezos-client/config"
  mkdir -p "$client_dir" "$HOME/.tezos-client"

  if [ ! -f "$_client_config" ]; then
    "$client" --base-dir "$client_dir" \
      --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
      config init --output "$_client_config" > /dev/null 2>&1
  else
    "$client" --base-dir "$client_dir" \
      --config-file "$_client_config" \
      --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
      config update > /dev/null 2>&1
  fi

}

wait_for_the_node_to_be_ready() {
  _count=0
  if "$client" rpc get /chains/main/blocks/head/hash > /dev/null 2>&1; then return; fi
  printf "Waiting for the node to initialize..."
  sleep 1
  while ! "$client" rpc get /chains/main/blocks/head/hash > /dev/null 2>&1; do
    _count=$((_count + 1))
    if [ "$_count" -ge 30 ]; then
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
  "$client" bootstrapped
}

check_image_version() {

  mkdir -p "$node_dir"

  # Check if we have to reset the chain because the image we want to
  # run has a incompatible version with the blockchain we have stored
  # locally on disk

  _image_version="$(cat "/usr/local/share/tezos/alphanet_version")"
  echo "Current public chain: $_image_version."
  _local_data_version=""
  if [ -f "$node_dir/alphanet_version" ]; then
    _local_data_version="$(cat "$node_dir/alphanet_version")"
    echo "Local chain data: $_local_data_version."
  fi
  if [ "$_local_data_version" != "$_image_version" ]; then
    echo "Removing outdated chain data..."
    if [ -f "$node_data_dir/identity.json" ]; then
      mv "$node_data_dir/identity.json" /tmp
    fi
    rm -rf "$node_data_dir"
    rm -rf "$client_dir/"*nonces
    rm -rf "$client_dir/"*endorsements
    rm -rf "$client_dir/"*blocks
    if [ -f "/tmp/identity.json" ]; then
      mkdir -p "$node_data_dir"
      mv /tmp/identity.json "$node_data_dir/"
    fi
    cp "/usr/local/share/tezos/alphanet_version" \
      "$node_dir/alphanet_version"
  fi

  mkdir -p "$node_data_dir"

}

launch_node() {

  check_image_version

  config_args=""
  node_args=""

  # Remove arguments incompatible with the config command and
  # retains them to pass them to the run command

  for i in "$@"; do
    case "$i" in
    "--force-history-mode-switch")
      node_args="$node_args $i "
      ;;
    *)
      config_args="$config_args $i "
      ;;
    esac
  done

  # We do not quote $config_args to make sure that it is a list of
  # arguments and not a string. Thus, we must disable a shellcheck
  # warning

  #shellcheck disable=SC2086
  if [ ! -f "$node_data_dir/config.json" ]; then
    echo "Configuring the node..."
    "$node" config init \
      --data-dir "$node_data_dir" \
      --rpc-addr "$NODE_RPC_ADDR:$NODE_RPC_PORT" \
      --allow-all-rpc "$NODE_RPC_ADDR:$NODE_RPC_PORT" \
      $config_args
  else
    echo "Updating the node configuration..."
    "$node" config update \
      --data-dir "$node_data_dir" \
      --rpc-addr "$NODE_RPC_ADDR:$NODE_RPC_PORT" \
      --allow-all-rpc "$NODE_RPC_ADDR:$NODE_RPC_PORT" \
      $config_args
  fi

  for i in "$@"; do
    if [ "$i" = "--help" ]; then exit 0; fi
  done

  # Generate a new identity if not present

  if [ ! -f "$node_data_dir/identity.json" ]; then
    echo "Generating a new node identity..."
    "$node" identity generate "${IDENTITY_POW:-26}". \
      --data-dir "$node_data_dir"
  fi

  configure_client

  # Launching the node

  #shellcheck disable=SC2086
  exec "$node" run --data-dir "$node_data_dir" $node_args

}

upgrade_node_storage() {

  check_image_version

  exec "$node" upgrade storage --data-dir "$node_data_dir"

}

snapshot_import() {

  check_image_version

  exec "$node" snapshot "$@" import /snapshot --data-dir "$node_data_dir"

}

launch_baker() {
  configure_client
  exec "$baker" --chain main \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run with local node "$node_data_dir" "$@"
}

launch_baker_test() {
  configure_client
  exec "$baker" --chain test \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run with local node "$node_data_dir" "$@"
}

launch_endorser() {
  configure_client
  wait_for_the_node_to_be_bootstrapped
  exec "$endorser" --chain main \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run "$@"
}

launch_endorser_test() {
  configure_client
  wait_for_the_node_to_be_bootstrapped
  exec "$endorser" --chain test \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run "$@"
}

launch_accuser() {
  configure_client
  wait_for_the_node_to_be_bootstrapped
  exec "$accuser" --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run "$@"
}

launch_accuser_test() {
  configure_client
  wait_for_the_node_to_be_bootstrapped
  exec "$accuser" --chain test \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    run "$@"
}

launch_smart_rollup_node() {
  configure_client

  if [ ! -f "$smart_rollup_node_data_dir/config.json" ]; then
    echo "Configuring the rollup node..."
    "$smart_rollup_node" init observer config \
      for "$SOR_ALIAS_OR_ADDR" \
      with operators \
      --data-dir "$smart_rollup_node_data_dir"
  fi

  exec "$smart_rollup_node" \
    --base-dir "$client_dir" \
    --endpoint "http://$NODE_HOST:$NODE_RPC_PORT" \
    "$@" \
    --data-dir "$smart_rollup_node_data_dir"
}
