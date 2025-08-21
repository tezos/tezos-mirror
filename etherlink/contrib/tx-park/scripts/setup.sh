#!/bin/bash

export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=y

notice() {
  echo -e "$(date +%b\ %d\ %H:%m:%S.%3N) \033[1;34mNotice\033[0m | $1"
}

if [ $# -lt 2 ]; then
  echo "Usage: $0 <input.env> <output.env> [project_user] [project_name]"
  echo "Example: $0 template.env output.env"
  exit 1
fi

INPUT_ENV="$1"
OUTPUT_ENV="$2"

notice "running $0 with input $INPUT_ENV and output $OUTPUT_ENV"

TMP_DIR=$(mktemp -d)
notice "running $0 using temporary directory $TMP_DIR"

CLIENT_DIR=$TMP_DIR/client
mkdir -p "$CLIENT_DIR"

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
OCTEZ_CLIENT="$SCRIPT_DIR/../../../../octez-client -d $CLIENT_DIR --endpoint https://rpc.tzkt.io/ghostnet/"

if [ -n "$3" ]; then
  PROJECT_USER="$3"
else
  read -rp "Enter value for PROJECT_USER: " PROJECT_USER
fi

if [ -n "$4" ]; then
  PROJECT_NAME="$4"
else
  read -rp "Enter value for PROJECT_NAME: " PROJECT_NAME
fi

extract_value() {
  grep "^$1=" "$INPUT_ENV" | sed "s/^$1=//"
}

CURRENT_ADMIN_SK=$(extract_value "ADMIN_SK")
CURRENT_SEQUENCER_SK=$(extract_value "SEQUENCER_SK")
CURRENT_BATCHER1_SK=$(extract_value "SMART_ROLLUP_BATCHER1_SK")
CURRENT_BATCHER2_SK=$(extract_value "SMART_ROLLUP_BATCHER2_SK")
CURRENT_BATCHER3_SK=$(extract_value "SMART_ROLLUP_BATCHER3_SK")

if [ -z "$CURRENT_SEQUENCER_SK" ]; then
  notice "generating sequencer account"
  $OCTEZ_CLIENT gen keys sequencer
  SEQUENCER_SK=$(jq -r '.[] | select(.name == "sequencer") | .value' "$CLIENT_DIR"/secret_keys)
else
  SEQUENCER_SK="$CURRENT_SEQUENCER_SK"
fi

if [ -z "$CURRENT_ADMIN_SK" ]; then
  notice "generating rollup-admin account"
  $OCTEZ_CLIENT gen keys rollup-admin
  ADMIN_PK=$(jq -r '.[] | select(.name == "rollup-admin") | .value' "$CLIENT_DIR"/public_key_hashs)
  ADMIN_SK=$(jq -r '.[] | select(.name == "rollup-admin") | .value' "$CLIENT_DIR"/secret_keys)
else
  $OCTEZ_CLIENT import secret key rollup-admin "$CURRENT_ADMIN_SK"
  ADMIN_PK=$(jq -r '.[] | select(.name == "rollup-admin") | .value' "$CLIENT_DIR"/public_key_hashs)
  ADMIN_SK="$CURRENT_ADMIN_SK"
fi

if [ -z "$CURRENT_BATCHER1_SK" ]; then
  notice "generating rollup-batcher1 account"
  $OCTEZ_CLIENT gen keys rollup-batcher1
  BATCHER1_PK=$(jq -r '.[] | select(.name == "rollup-batcher1") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER1_SK=$(jq -r '.[] | select(.name == "rollup-batcher1") | .value' "$CLIENT_DIR"/secret_keys)
else
  $OCTEZ_CLIENT import secret key rollup-batcher1 "$CURRENT_BATCHER1_SK"
  BATCHER1_PK=$(jq -r '.[] | select(.name == "rollup-batcher1") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER1_SK="$CURRENT_BATCHER1_SK"
fi

if [ -z "$CURRENT_BATCHER2_SK" ]; then
  notice "generating rollup-batcher2 account"
  $OCTEZ_CLIENT gen keys rollup-batcher2
  BATCHER2_PK=$(jq -r '.[] | select(.name == "rollup-batcher2") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER2_SK=$(jq -r '.[] | select(.name == "rollup-batcher2") | .value' "$CLIENT_DIR"/secret_keys)
else
  $OCTEZ_CLIENT import secret key rollup-batcher2 "$CURRENT_BATCHER2_SK"
  BATCHER2_PK=$(jq -r '.[] | select(.name == "rollup-batcher2") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER2_SK="$CURRENT_BATCHER2_SK"
fi

if [ -z "$CURRENT_BATCHER3_SK" ]; then
  notice "generating rollup-batcher3 account"
  $OCTEZ_CLIENT gen keys rollup-batcher3
  BATCHER3_PK=$(jq -r '.[] | select(.name == "rollup-batcher3") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER3_SK=$(jq -r '.[] | select(.name == "rollup-batcher3") | .value' "$CLIENT_DIR"/secret_keys)
else
  $OCTEZ_CLIENT import secret key rollup-batcher3 "$CURRENT_BATCHER3_SK"
  BATCHER3_PK=$(jq -r '.[] | select(.name == "rollup-batcher3") | .value' "$CLIENT_DIR"/public_key_hashs)
  BATCHER3_SK="$CURRENT_BATCHER3_SK"
fi

check_and_fund() {
  local account_name=$1
  local pk=$2
  local amount=$3

  local balance

  balance=$(curl -s "https://api.ghostnet.tzkt.io/v1/accounts/$pk/balance")

  if [[ $balance -lt 5000000 ]]; then
    notice "funding the $account_name account with $amount tez"
    npx @tacoinfra/get-tez "$pk" --amount "$amount" --network ghostnet
  fi
}

check_and_fund "rollup admin" "$ADMIN_PK" 5
check_and_fund "rollup batcher account 1" "$BATCHER1_PK" 5
check_and_fund "rollup batcher account 2" "$BATCHER2_PK" 5
check_and_fund "rollup batcher account 3" "$BATCHER3_PK" 5

TMP_FILE=$(mktemp -p "$TMP_DIR")

while IFS= read -r line; do
  if [[ "$line" =~ ^PROJECT_USER= ]]; then
    echo "PROJECT_USER=$PROJECT_USER"
  elif [[ "$line" =~ ^PROJECT_NAME= ]]; then
    echo "PROJECT_NAME=$PROJECT_NAME"
  elif [[ "$line" =~ ^ADMIN_SK= ]]; then
    echo "ADMIN_SK=$ADMIN_SK"
  elif [[ "$line" =~ ^SEQUENCER_SK= ]]; then
    echo "SEQUENCER_SK=$SEQUENCER_SK"
  elif [[ "$line" =~ ^SMART_ROLLUP_BATCHER1_SK= ]]; then
    echo "SMART_ROLLUP_BATCHER1_SK=$BATCHER1_SK"
  elif [[ "$line" =~ ^SMART_ROLLUP_BATCHER2_SK= ]]; then
    echo "SMART_ROLLUP_BATCHER2_SK=$BATCHER2_SK"
  elif [[ "$line" =~ ^SMART_ROLLUP_BATCHER3_SK= ]]; then
    echo "SMART_ROLLUP_BATCHER3_SK=$BATCHER3_SK"
  else
    echo "$line"
  fi
done < "$INPUT_ENV" > "$TMP_FILE"

cp "$TMP_FILE" "$OUTPUT_ENV"

notice "successfully generated $OUTPUT_ENV, removing $TMP_DIR directory"
rm -rf "$TMP_DIR"

notice "build the Docker image using the generated env file with the following command:"
notice "\t docker-compose --env-file $OUTPUT_ENV build"
