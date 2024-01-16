#!/usr/bin/env bash

set -e

script_dir="$(dirname "$0")"

#shellcheck source=etherlink/scripts/docker-compose/.env
. "${script_dir}/.env"

run_in_docker() {
  bin="$1"
  shift 1
  docker_run=(docker run --log-driver=json-file -v "${HOST_TEZOS_DATA_DIR}":/home/tezos tezos/tezos-bare:"${OCTEZ_TAG}")
  "${docker_run[@]}" "/usr/local/bin/${bin}" "$@"
}

docker_update_images() {
  # pull latest version
  docker pull tezos/tezos-bare:"${OCTEZ_TAG}"
  docker build -t tezos_with_curl:"${OCTEZ_TAG}" tezos_with_curl/ --build-arg OCTEZ_TAG="${OCTEZ_TAG}"
}

create_chain_id() {
  number="$1"
  # Convert the number to hexadecimal and little endian format
  hex_le=$(printf "%064x" "$number" | tac -rs ..)

  # Pad to 256 bits (64 hex characters)
  padded_hex_le=$(printf "%064s" "$hex_le")
  echo "$padded_hex_le"
}

build_kernel() {
  docker build -t etherlink_kernel:"${OCTEZ_TAG}" evm_kernel_builder/ --build-arg OCTEZ_TAG="${OCTEZ_TAG}"
  evm_chain_id=$(create_chain_id "${EVM_CHAIN_ID}")
  evm_chain_id_config="  - set:\n      # Chain ID: ${EVM_CHAIN_ID}\n      value: ${evm_chain_id}\n      to: /evm/chain_id\n"
  # build kernel in an image (e.g. tezos/tezos-bare:master) with new chain id
  docker build -t etherlink_kernel:"${OCTEZ_TAG}" evm_kernel_builder/ --build-arg OCTEZ_TAG="${OCTEZ_TAG}" --build-arg EVM_CHAIN_ID_CONFIG="${evm_chain_id_config}"
  container_name=$(docker create etherlink_kernel:"${OCTEZ_TAG}")
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
  source="$1"
  rollup_alias="$2"
  kernel_path="$3"
  echo "originate a new evm rollup '${rollup_alias}' with '${source}', and kernel '${kernel_path}'"
  kernel="$(xxd -p "${kernel_path}" | tr -d '\n')"
  run_in_docker octez-client --endpoint "${ENDPOINT}" \
    originate smart rollup "${rollup_alias}" \
    from "${source}" \
    of kind wasm_2_0_0 of type "(or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes)" \
    with kernel "${kernel}" \
    --burn-cap 999 --force
}

init_rollup_node_config() {
  mode="$1"
  rollup_alias="$2"
  operators="$3"
  echo "create rollup node config and copy kernel preimage"
  run_in_docker octez-smart-rollup-node init "${mode}" config for "${rollup_alias}" with operators "${operators[@]}" --rpc-addr 0.0.0.0 --rpc-port 8733
  cp -R "${HOST_TEZOS_DATA_DIR}"/kernel/_evm_installer_preimages/ "${HOST_TEZOS_DATA_DIR}"/.tezos-smart-rollup-node/wasm_2_0_0
}

init_octez_node() {
  docker_update_images

  # init octez node storage
  docker run --log-driver=json-file -v "${HOST_TEZOS_DATA_DIR}":/home/tezos tezos/tezos-bare:"${OCTEZ_TAG}" /usr/local/bin/octez-node config init --network "${TZNETWORK}" --data-dir /home/tezos/.tezos-node
  # download snapshot
  if [[ -n ${SNAPSHOT_URL} ]]; then
    wget -O "${HOST_TEZOS_DATA_DIR}/snapshot" "${SNAPSHOT_URL}"
    docker run --log-driver=json-file -v "${HOST_TEZOS_DATA_DIR}":/home/tezos tezos/tezos-bare:"${OCTEZ_TAG}" /usr/local/bin/octez-node snapshot import /home/tezos/snapshot --data-dir /home/tezos/.tezos-node
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

  KERNEL="${HOST_TEZOS_DATA_DIR}"/kernel/sequencer.wasm
  originate_evm_rollup "${ORIGINATOR_ALIAS}" "${ROLLUP_ALIAS}" "${KERNEL}"

  init_rollup_node_config "${ROLLUP_NODE_MODE}" "${ROLLUP_ALIAS}" "${OPERATOR_ALIAS}"
}

loop_until_balance_is_enough() {
  alias=$1
  minimum_balance=$2
  address=$(run_in_docker octez-client --endpoint "${ENDPOINT}" show address "${alias}" | grep Hash | grep -oE "tz.*")
  echo "loop until ${alias} (e.g. ${address}) as at last a balance of ${minimum_balance}"
  until balance_account_is_enough "${address}" "${alias}" "${minimum_balance}"; do
    sleep 10.
  done
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
init_rollup)
  if [[ -n ${OPERATOR_ALIAS} ]]; then
    generate_key "${OPERATOR_ALIAS}"
    loop_until_balance_is_enough "${OPERATOR_ALIAS}" "${MINIMUM_OPERATOR_BALANCE}"
  fi
  generate_key "${ORIGINATOR_ALIAS}"
  loop_until_balance_is_enough "${ORIGINATOR_ALIAS}" 100
  init_rollup
  echo "You can now start the docker with \"docker compose up -d\""
  ;;
reset_rollup)
  docker-compose stop smart-rollup-node sequencer blockscout blockscout-db blockscout-redis-db

  rm -r "${HOST_TEZOS_DATA_DIR}/.tezos-smart-rollup-node" "${HOST_TEZOS_DATA_DIR}/.octez-evm-node" "${HOST_TEZOS_DATA_DIR}/kernel"

  init_rollup

  docker-compose up -d --remove-orphans
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
  - init_rollup:
    build lastest evm kernel, originate the rollup, create operator, wait until operator balance
     is topped then create rollup node config.
  - reset_rollup:
    remove rollup node data dir, sequencer data dir, blockscout data dir,
    and existing kernel.
    Then build lastest evm kernel, originate a new rollup with it and
    initialise the rollup node config in:
     "${HOST_TEZOS_DATA_DIR}".
EOF
  ;;
esac
