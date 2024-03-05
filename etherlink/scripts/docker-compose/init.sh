#!/usr/bin/env bash

set -e

script_dir=$(dirname "$0")

#shellcheck source=etherlink/scripts/docker-compose/.env
. "${script_dir}/.env"

run_in_docker() {
  bin="$1"
  shift 1
  docker_run=(docker run -v "${HOST_TEZOS_DATA_DIR}":/home/tezos tezos/tezos-bare:"${OCTEZ_TAG}")
  "${docker_run[@]}" "/usr/local/bin/${bin}" "$@"
}

docker_compose() {
  if (command -v "docker-compose" &> /dev/null); then
    docker-compose "$@"
  else
    docker compose "$@"
  fi
}

docker_update_images() {
  # pull latest version
  docker pull tezos/tezos-bare:"${OCTEZ_TAG}"
  docker build -t tezos_with_curl:"${OCTEZ_TAG}" "${script_dir}"/tezos_with_curl/ --build-arg OCTEZ_TAG="${OCTEZ_TAG}"
}

create_chain_id() {
  # Convert the number to hexadecimal and little endian format
  hex_le=$(printf "%064x" "${EVM_CHAIN_ID}" | tac -rs ..)

  # Pad to 256 bits (64 hex characters)
  printf "%064s" "$hex_le"
}

add_kernel_config_set() {
  file="$1"
  key="$2"
  value="$3"
  comment="$4"
  {
    echo "  - set: # ${comment}"
    echo "      value: ${value}"
    echo "      to: ${key}"
  } >> "$file"
}

add_kernel_config_contract() {
  file="$1"
  key="$2"
  alias="$3"
  label="$4"

  if address=$(run_in_docker octez-client --endpoint "$ENDPOINT" show known contract "$alias" 2> /dev/null); then
    hex=$(printf '%s' "${address}" | xxd -p -c 36)
    add_kernel_config_set "$file" "$key" "$hex" "${label}: ${address}"
  fi
}

create_kernel_config() {
  file="$1"

  # chain ID config
  evm_chain_id=$(create_chain_id "${EVM_CHAIN_ID}")
  add_kernel_config_set "$file" /evm/chain_id "${evm_chain_id}" "Chain ID: ${EVM_CHAIN_ID}"

  # ticketer config
  add_kernel_config_contract "$file" /evm/ticketer "${BRIDGE_ALIAS}" "BRIDGE"

  # evm admin config
  add_kernel_config_contract "$file" /evm/admin "${EVM_ADMIN_ALIAS}_contract" "ADMIN"

  # sequencer admin config
  add_kernel_config_contract "$file" /evm/sequencer_admin "${SEQUENCER_ADMIN_ALIAS}_contract" "SEQUENCER_ADMIN"

  # delayed bridge config
  add_kernel_config_contract "$file" /evm/delayed_bridge "${DELAYED_BRIDGE_ALIAS}" "DELAYED_BRIDGE"

  # evm accounts config
  for account in "${EVM_ACCOUNTS[@]}"; do
    add_kernel_config_set "$file" "/evm/world_state/eth_accounts/${account}/balance" 0000dc0a0713000c1e0200000000000000000000000000000000000000000000 ""
  done

  # sequencer_config
  if [[ -n $SEQUENCER_ALIAS ]]; then
    pubkey=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show address "${SEQUENCER_ALIAS}" | grep Public | grep -oE "edpk.*")
    pubkey_hex=$(printf '%s' "${pubkey}" | xxd -p -c 54)
    add_kernel_config_set "$file" "/evm/sequencer" "${pubkey_hex}" "SEQUENCER_PUBKEY: ${pubkey}"
  fi
}

build_kernel() {
  mkdir -p "${HOST_TEZOS_DATA_DIR}/.tezos-client"
  cp "${EVM_KERNEL_CONFIG}" "$script_dir"/evm_config_tmp.yaml
  create_kernel_config "$script_dir"/evm_config_tmp.yaml
  # build kernel in an image (e.g. tezos/tezos-bare:master) with new chain id
  commit="$(git rev-parse HEAD)"
  docker build --no-cache -t etherlink_kernel:"$commit" --build-arg EVM_CONFIG="etherlink/scripts/docker-compose/evm_config_tmp.yaml" --build-arg CI_COMMIT_SHA="$commit" -f "$script_dir/evm_kernel_builder.Dockerfile" "${script_dir}/../../.."
  container_name=$(docker create etherlink_kernel:"$commit")
  docker cp "${container_name}":/kernel/ "${HOST_TEZOS_DATA_DIR}/"
}

generate_key() {
  alias=$1
  echo "Generate a ${alias} key. Nothing happens if key already exists"
  # if next command fails it's because the alias already
  # exists. Don't override it.
  run_in_docker octez-client --endpoint "${ENDPOINT}" gen keys "${alias}" || echo "${alias} already exists"
  echo "You can now top up the balance for ${alias}"
}

balance_account_is_enough() {
  address="$1"
  alias="$2"
  minimum="$3"
  balance=$(run_in_docker octez-client --endpoint "${ENDPOINT}" get balance for "${alias}")
  balance=${balance%" ꜩ"} # remove ꜩ
  balance=${balance%.*}   # remove floating part
  echo "balance of ${alias} is ${balance} ꜩ (truncated)."
  if [[ "${balance}" -ge "${minimum}" ]]; then
    return 0
  else
    echo "Top up the balance for ${address} at least with ${minimum}"
    return 1
  fi
}

originate_evm_rollup() {
  kernel_path="$1"
  echo "originate a new evm rollup '${ROLLUP_ALIAS}' with '${ORIGINATOR_ALIAS}', and kernel '${kernel_path}'"
  kernel="$(xxd -p "${kernel_path}" | tr -d '\n')"
  run_in_docker octez-client --endpoint "${ENDPOINT}" \
    originate smart rollup "${ROLLUP_ALIAS}" \
    from "${ORIGINATOR_ALIAS}" \
    of kind wasm_2_0_0 of type "(or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes)" \
    with kernel "${kernel}" \
    --burn-cap 999 --force
}

init_rollup_node_config() {
  echo "create rollup node config and copy kernel preimage"
  run_in_docker octez-smart-rollup-node init "${ROLLUP_NODE_MODE}" config for "${ROLLUP_ALIAS}" with operators "${OPERATOR_ALIAS}" --rpc-addr 0.0.0.0 --rpc-port 8733 --cors-origins '*' --cors-headers '*'
  cp -R "${HOST_TEZOS_DATA_DIR}"/kernel/_evm_installer_preimages/ "${HOST_TEZOS_DATA_DIR}"/.tezos-smart-rollup-node/wasm_2_0_0
}

init_octez_node() {
  docker_update_images
  mkdir -p "$HOST_TEZOS_DATA_DIR"
  # init octez node storage
  run_in_docker octez-node config init --network "${TZNETWORK_ADDRESS}"
  # download snapshot
  if [[ -n ${SNAPSHOT_URL} ]]; then
    wget -O "${HOST_TEZOS_DATA_DIR}/snapshot" "${SNAPSHOT_URL}"
    run_in_docker octez-node snapshot import /home/tezos/snapshot
  fi
}

originate_contracts() {
  generate_key "${ORIGINATOR_ALIAS}"
  loop_until_balance_is_enough "${ORIGINATOR_ALIAS}" 100
  if [[ -n ${EXCHANGER_ALIAS} ]]; then
    originate_exchanger
    if [[ -n ${BRIDGE_ALIAS} ]]; then
      originate_bridge
    fi
  fi
  if [[ -n ${EVM_ADMIN_ALIAS} ]]; then
    generate_key "${EVM_ADMIN_ALIAS}"
    evm_admin_address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show address "${EVM_ADMIN_ALIAS}" | grep Hash | grep -oE "tz.*")
    originate_admin "${EVM_ADMIN_ALIAS}_contract" "${evm_admin_address}"
  fi
  if [[ -n ${SEQUENCER_ADMIN_ALIAS} ]]; then
    generate_key "${SEQUENCER_ADMIN_ALIAS}"
    sequencer_admin_address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show address "${SEQUENCER_ADMIN_ALIAS}" | grep Hash | grep -oE "tz.*")
    originate_admin "${SEQUENCER_ADMIN_ALIAS}_contract" "${sequencer_admin_address}"
  fi
  if [[ -n ${DELAYED_BRIDGE_ALIAS} ]]; then
    originate_contract delayed_transaction_bridge.tz "${DELAYED_BRIDGE_ALIAS}" Unit
  fi
}

# this function:
# 1/ updates the docker images/
# 2/ build the kernel based on latest octez master version.
#    kernels and pre-images are copied into "${HOST_TEZOS_DATA_DIR}/kernel"
# 3/ originate a new rollup with the build kernel
# 4/ initialise the octez-smart-rollup-node configuration
init_rollup() {
  docker_update_images
  build_kernel
  kernel="${HOST_TEZOS_DATA_DIR}"/kernel/evm_installer.wasm
  originate_evm_rollup "${kernel}"
  init_rollup_node_config
}

loop_until_balance_is_enough() {
  alias=$1
  minimum_balance=$2
  address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show address "${alias}" | grep Hash | grep -oE "tz.*")
  until balance_account_is_enough "${address}" "${alias}" "${minimum_balance}"; do
    if [[ -n $FAUCET ]] && command -v npx &> /dev/null; then
      npx @tacoinfra/get-tez -a $((minimum_balance + 100)) -f "$FAUCET" "$address"
    fi
    sleep 10.
  done
}

originate_contract() {
  filename=$1
  contract_alias=$2
  storage=$3
  mkdir -p "${HOST_TEZOS_DATA_DIR}"/contracts
  cp "${script_dir}/../../tezos_contracts/${filename}" "${HOST_TEZOS_DATA_DIR}/contracts"
  echo "originate ${filename} contract (alias: ${contract_alias})"
  run_in_docker octez-client --endpoint "${ENDPOINT}" originate contract "${contract_alias}" transferring 0 from "${ORIGINATOR_ALIAS}" running "contracts/${filename}" --init "$storage" --burn-cap 0.5
}

originate_exchanger() {
  originate_contract exchanger.tz "${EXCHANGER_ALIAS}" Unit
}

originate_bridge() {
  exchanger_address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show known contract "${EXCHANGER_ALIAS}")
  originate_contract evm_bridge.tz "${BRIDGE_ALIAS}" "Pair \"${exchanger_address}\" None"
}

originate_admin() {
  admin_alias=$1
  admin_address=$2
  originate_contract admin.tz "${admin_alias}" "\"${admin_address}\""
}

deposit() {
  amount=$1
  src=$2
  l2_address=$3
  rollup_address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show known smart rollup "${ROLLUP_ALIAS}")
  run_in_docker octez-client --endpoint "${ENDPOINT}" transfer "$amount" from "$src" to "${BRIDGE_ALIAS}" --entrypoint deposit --arg "Pair \"${rollup_address}\" $l2_address" --burn-cap 0.03075
}

command=$1
shift 1

case $command in
generate_key)
  generate_key "$@"
  ;;
check_balance)
  loop_until_balance_is_enough "$@"
  ;;
originate_rollup)
  originate_evm_rollup "$@"
  ;;
init_rollup_node_config)
  init_rollup_node_config "$@"
  ;;
init_octez_node)
  init_octez_node
  ;;
build_kernel)
  docker_update_images
  build_kernel
  kernel="${HOST_TEZOS_DATA_DIR}"/kernel/evm_installer.wasm
  ;;
init_rollup)
  if [[ -n ${OPERATOR_ALIAS} ]]; then
    generate_key "${OPERATOR_ALIAS}"
    loop_until_balance_is_enough "${OPERATOR_ALIAS}" "${MINIMUM_OPERATOR_BALANCE}"
  fi
  if [[ -n ${SEQUENCER_ALIAS} ]]; then
    generate_key "${SEQUENCER_ALIAS}"
  fi
  generate_key "${ORIGINATOR_ALIAS}"
  loop_until_balance_is_enough "${ORIGINATOR_ALIAS}" 100
  init_rollup
  echo "You can now start the docker with \"./init.sh run\""
  ;;
reset_rollup)
  docker_compose stop smart-rollup-node sequencer

  rm -r "${HOST_TEZOS_DATA_DIR}/.tezos-smart-rollup-node" "${HOST_TEZOS_DATA_DIR}/.octez-evm-node" "${HOST_TEZOS_DATA_DIR}/kernel"

  init_rollup

  docker_compose up -d --remove-orphans
  ;;
originate_contracts)
  originate_contracts
  ;;
deposit)
  deposit "$@"
  ;;
run)
  docker_compose up -d
  ;;
restart)
  docker_compose restart
  ;;
*)
  cat << EOF
Available commands:
  - generate_key <alias>
  - check_balance <alias> <minimal balance>
  - originate_rollup <source> <rollup_alias>
  - init_rollup_node_config <rollup_mode> <rollup_alias> <operators>
  - init_octez_node:
    download snapshot, and init octez-node config
  - originate_contracts:
    originate contracts
  - build_kernel:
    build latest evm kernel
  - init_rollup:
    build lastest evm kernel, originate the rollup, create operator, wait until operator balance
     is topped then create rollup node config.
  - reset_rollup:
    remove rollup node data dir, sequencer data dir, blockscout data dir,
    and existing kernel.
    Then build lastest evm kernel, originate a new rollup with it and
    initialise the rollup node config in:
     "${HOST_TEZOS_DATA_DIR}".
  - run
    execute docker compose up
  - restart
    execute docker compose restart
  - deposit <amount> <source: tezos address> <target: evm address>
    deposit xtz to etherlink via the ticketer
EOF
  ;;
esac
