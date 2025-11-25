#!/usr/bin/env bash
set -euo pipefail

# ==============================================================================
# Tezos DAL Archiver - DAL node commands generator for multiple machines
# ------------------------------------------------------------------------------
# This script does NOT launch anything.
# It only generates ready-to-use `octez-dal-node run` commands,
# grouped by machine and DAL node index.
# ==============================================================================

# Default options
SLOT_RANGE="0-31"
SLOTS_PER_DAL_NODE=4
DAL_HISTORY_MODE="auto"
NUM_MACHINES=1
DAL_RPC_HOST="127.0.0.1"
DAL_METRICS_HOST="127.0.0.1"
L1_RPC_URL="http://127.0.0.1:8732"
DATA_DIR_BASE="\$HOME/tezos-dal-archives"

help_all() {
  cat << EOF
Usage:
  dal-node-archiver-plan.sh gen [options]

Subcommands:
  gen    Generate DAL node commands (default if omitted)
  help   Show this help

Options:
  --num-machines=<N>           Number of machines (default: $NUM_MACHINES)
  --slot-indices-ranges=a-b    Inclusive slot range (default: $SLOT_RANGE)
  --slots-per-dal-node=<n>     Slots handled per DAL node (default: $SLOTS_PER_DAL_NODE)
  --dal-history-mode=<mode>    full | auto (default: $DAL_HISTORY_MODE)
  --dal-rpc-addr=<HOST>        (default $DAL_RPC_HOST)
  --dal-metrics-addr=<HOST>    (default $DAL_METRICS_HOST)
  --data-dir-base=<PATH>       Base directory for DAL nodes
                               (default: $DATA_DIR_BASE)

Notes:
  - This script only PRINTS the commands. It does not run them.
  - Replace <PUBLIC-ADDR> with the actual hostname/IP of each machine.
  - Data directories are fixed to: <base>/octez-dal-node-archiver-<slot_id>
  - Endpoint is fixed to: $L1_RPC_URL
  - Port scheme:
      p2p     : 12000 + slot_id
      RPC     : 13000 + slot_id
      metrics : 14000 + slot_id
EOF
}

parse_opts() {
  for a in "$@"; do
    case "$a" in
    --num-machines=*) NUM_MACHINES="${a#*=}" ;;
    --slot-indices-ranges=*) SLOT_RANGE="${a#*=}" ;;
    --slots-per-dal-node=*) SLOTS_PER_DAL_NODE="${a#*=}" ;;
    --dal-history-mode=*) DAL_HISTORY_MODE="${a#*=}" ;;
    --dal-rpc-addr=*) DAL_RPC_HOST="${a#*=}" ;;
    --dal-metrics-addr=*) DAL_METRICS_HOST="${a#*=}" ;;
    --data-dir-base=*) DATA_DIR_BASE="${a#*=}" ;;
    --help)
      help_all
      exit 0
      ;;
    help)
      help_all
      exit 0
      ;;
    gen) ;; # handled by dispatcher
    *)
      echo "Unknown option: $a"
      help_all
      exit 1
      ;;
    esac
  done

  [[ "$SLOT_RANGE" =~ ^([0-9]+)-([0-9]+)$ ]] || {
    echo "Bad --slot-indices-ranges"
    exit 1
  }
  SLOT_MIN="${BASH_REMATCH[1]}"
  SLOT_MAX="${BASH_REMATCH[2]}"
  ((SLOT_MIN <= SLOT_MAX)) || {
    echo "min>max"
    exit 1
  }

  if ! [[ "$SLOTS_PER_DAL_NODE" =~ ^[0-9]+$ ]] || ! ((SLOTS_PER_DAL_NODE > 0)); then
    echo "slots-per-dal-node must be >0"
    exit 1
  fi

  if ! [[ "$NUM_MACHINES" =~ ^[0-9]+$ ]] || ! ((NUM_MACHINES > 0)); then
    echo "num-machines must be >0"
    exit 1
  fi

  TOTAL_SLOTS=$((SLOT_MAX - SLOT_MIN + 1))
  NUM_DAL_NODES=$(((TOTAL_SLOTS + SLOTS_PER_DAL_NODE - 1) / SLOTS_PER_DAL_NODE))

  # Summary values
  DAL_PER_MACHINE=$(((NUM_DAL_NODES + NUM_MACHINES - 1) / NUM_MACHINES))
  SLOTS_PER_NODE=$(((TOTAL_SLOTS + NUM_DAL_NODES - 1) / NUM_DAL_NODES))
}

generate_commands() {
  echo "# Summary"
  echo "# Machines: $NUM_MACHINES"
  echo "# DAL nodes: $NUM_DAL_NODES total (~$DAL_PER_MACHINE per machine)"
  echo "# Slots: $TOTAL_SLOTS total (~$SLOTS_PER_NODE per DAL node)"
  echo

  machine_cmds=()
  for ((m = 1; m <= NUM_MACHINES; m++)); do
    machine_cmds[m]=""
  done

  for ((k = 0; k < NUM_DAL_NODES; k++)); do
    local first=$((SLOT_MIN + k))
    local s=$first
    local count=0
    local arr=()
    while ((s <= SLOT_MAX && count < SLOTS_PER_DAL_NODE)); do
      arr+=("$s")
      count=$((count + 1))
      s=$((s + NUM_DAL_NODES))
    done
    local profiles
    profiles=$(
      IFS=,
      echo "${arr[*]}"
    )

    local id="$first"
    local net=$((12000 + id)) rpc=$((13000 + id)) met=$((14000 + id))
    local dir="$DATA_DIR_BASE/octez-dal-node-archiver-$id"

    local machine=$(((k % NUM_MACHINES) + 1))
    local node_index=$(((k / NUM_MACHINES) + 1))

    local cmd="# DAL Node ${machine}.${node_index}
octez-dal-node run \\
  --operator-profiles $profiles \\
  --history-mode $DAL_HISTORY_MODE \\
  --disable-amplification \\
  --endpoint $L1_RPC_URL \\
  --data-dir $dir \\
  --public-addr <PUBLIC-ADDR>:$net \\
  --net-addr 0.0.0.0:$net \\
  --rpc-addr $DAL_RPC_HOST:$rpc \\
  --metrics-addr $DAL_METRICS_HOST:$met
"

    machine_cmds[machine]="${machine_cmds[$machine]}$cmd"$'\n'
  done

  for ((m = 1; m <= NUM_MACHINES; m++)); do
    echo "# Machine $m"
    echo
    echo "${machine_cmds[$m]}"
  done
}

# Dispatcher
if (($# == 0)); then
  subcmd="gen"
else
  subcmd="$1"
  shift
fi

case "$subcmd" in
help | --help) help_all ;;
gen)
  parse_opts "$@"
  generate_commands
  ;;
*)
  echo "Unknown subcommand: $subcmd"
  help_all
  exit 1
  ;;
esac
