#!/usr/bin/env bash

set -e

script_dir=$(dirname "$0")

#shellcheck source=etherlink/scripts/docker-compose/.env
. "${script_dir}/.env"

docker_compose() {
  if (command -v "docker-compose" &> /dev/null); then
    docker-compose "$@"
  else
    docker compose "$@"
  fi
}

run_in_docker_compose() {
  service="$1"
  shift 1
  docker_compose run "${service}" "$@"
}

docker_update_images() {
  # pull latest version
  docker pull tezos/tezos-bare:"${OCTEZ_TAG}"
  docker pull tezos/tezos-bare:"${ROLLUP_OCTEZ_TAG}"
  docker pull tezos/tezos-bare:"${EVM_OCTEZ_TAG}"
}

# fallback function to say which datadir to delete. This allows to recall the
# script if it fails at some point
script_failed() {
  dirs="$1"
  docker_compose down
  echo "Script failed, please delete following datadir to restart the script from scratch ${HOST_TEZOS_DATA_DIR}/${dirs}."
}

assert_init_can_run() {
  if [[ -d "${HOST_TEZOS_DATA_DIR}" ]]; then
    echo "${HOST_TEZOS_DATA_DIR} already exists. To run, please remove it."
    exit 1
  fi
}

init_octez_node() {
  trap 'script_failed ".tezos-node"' ERR
  mkdir -p "$HOST_TEZOS_DATA_DIR"
  # init octez node storage
  if [[ -n ${TZNETWORK_ADDRESS} ]]; then
    run_in_docker_compose octez-node config init --network "${TZNETWORK_ADDRESS}"
  fi
  # download snapshot and import it.
  if [[ -n ${SNAPSHOT_URL} ]]; then
    # Do not download the snapshot if it already exists.
    if [ ! -e "${HOST_TEZOS_DATA_DIR}/snapshot" ]; then
      wget -O "${HOST_TEZOS_DATA_DIR}/snapshot" "${SNAPSHOT_URL}"
    else
      echo "Snapshot ${HOST_TEZOS_DATA_DIR}/snapshot already exists, using it."
    fi
    echo "importing snapshot ${HOST_TEZOS_DATA_DIR}/snapshot"
    run_in_docker_compose octez-node snapshot import /home/tezos/snapshot
  fi
}

init_rollup_node() {
  trap 'script_failed "{.tezos-node, .tezos-smart-rollup-node}"' ERR
  echo "creating rollup node config"
  run_in_docker_compose rollup-node init "${ROLLUP_NODE_MODE}" config for "${ROLLUP_ADDRESS}" \
    with operators \
    --rpc-addr 0.0.0.0 --rpc-port 8932 --cors-origins '*' \
    --cors-headers '*' \
    --pre-images-endpoint "${PREIMAGES_ENDPOINT}" \
    --acl-override allow-all
  # download snapshot and import it.
  if [[ -n ${ROLLUP_SNAPSHOT_URL} ]]; then
    # Do not download the snapshot if it already exists.
    if [ ! -e "${HOST_TEZOS_DATA_DIR}/rollup-snapshot" ]; then
      wget -O "${HOST_TEZOS_DATA_DIR}/rollup-snapshot" "${ROLLUP_SNAPSHOT_URL}"
    else
      echo "Snapshot ${HOST_TEZOS_DATA_DIR}/rollup-snapshot already exists, using it."
    fi
    echo "importing snapshot ${HOST_TEZOS_DATA_DIR}/rollup-snapshot"
    run_in_docker_compose rollup-node --endpoint "${ARCHIVE_OCTEZ_NODE_ENDPOINT}" snapshot import /home/tezos/rollup-snapshot
  fi
}

init_evm_node() {
  trap 'script_failed "{.tezos-node, .tezos-smart-rollup-node, .tezos-evm-node}"' ERR
  echo "creating evm node config"
  case "$1" in
  proxy)
    run_in_docker_compose proxy init config --evm-node-endpoint https://relay.mainnet.etherlink.com --rollup-node-endpoint http://rollup-node:8932 --cors-origins '*' --cors-headers '*' --rpc-addr 0.0.0.0 --rpc-port 8545 --keep-alive
    ;;
  observer)
    run_in_docker_compose observer init config --evm-node-endpoint https://relay.mainnet.etherlink.com --rollup-node-endpoint http://rollup-node:8932 --cors-origins '*' --cors-headers '*' --rpc-addr 0.0.0.0 --rpc-port 8545 --keep-alive
    # download snapshot and import it.
    if [[ -n ${EVM_NODE_SNAPSHOT_URL} ]]; then
      # Do not download the snapshot if it already exists.
      if [ ! -e "${HOST_TEZOS_DATA_DIR}/evm-snapshot" ]; then
        wget -O "${HOST_TEZOS_DATA_DIR}/evm-snapshot" "${EVM_NODE_SNAPSHOT_URL}"
      else
        echo "Snapshot ${HOST_TEZOS_DATA_DIR}/evm-snapshot already exists, using it."
      fi
      echo "importing snapshot ${HOST_TEZOS_DATA_DIR}/evm-snapshot"
      run_in_docker_compose observer snapshot import /home/tezos/evm-snapshot
    fi
    ;;
  *)
    cat << EOF
Available evm node profile are "proxy" and "observer"
EOF
    ;;
  esac

}

init() {
  docker_update_images
  init_octez_node
  docker_compose up octez-node -d
  echo "waiting for the octez-node to be bootstrapped"
  # The while loop allows to ignore the possible first iteration where
  # the endpoint is not up yet (while the container starts).
  while ! run_in_docker_compose curl-runner --max-time 1200 http://octez-node:8732/monitor/bootstrapped; do
    sleep 5.
  done
  init_rollup_node
  OCTEZ_NODE_ENDPOINT="${ARCHIVE_OCTEZ_NODE_ENDPOINT}" docker_compose up rollup-node -d
  echo "waiting for the rollup-node to be bootstrapped"
  # The while loop allows to ignore the possible first iteration where
  # the endpoint is not up yet (while the container starts).
  while ! run_in_docker_compose curl-runner --max-time 1200 http://rollup-node:8932/local/synchronized; do
    sleep 5.
  done

  init_evm_node "$@"
  docker_compose stop
}

command="${1:-init}"
# Perform a safe shift
if [ "$#" -ge 1 ]; then
  shift 1
fi

case $command in
init_octez_node)
  init_octez_node
  ;;
init_rollup_node)
  init_rollup_node
  ;;
init_evm_node)
  init_evm_node "$@"
  ;;
init)
  assert_init_can_run
  init "${2:-observer}"
  ;;
run)
  docker_compose up --profile "${2:-observer}" -d
  ;;
restart)
  docker_compose restart
  ;;
*)
  cat << EOF
Available commands:
  - init (proxy | observer):
    initialize the full stack. By default runs observer.
  - run (proxy | observer)
    execute docker compose up. By default runs observer.
  - restart
    execute docker compose restart
EOF
  ;;
esac
