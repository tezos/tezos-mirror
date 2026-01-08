#!/usr/bin/env bash
# Benchmark script that deploys a fresh ERC-20, mints funds, and measures latency
# using eth_sendRawTransactionSync. Cast is only used for encoding/signing.

set -euo pipefail

require_cmd() {
  if ! command -v "$1" > /dev/null 2>&1; then
    echo "Error: '$1' command not found. Please install it before running this script." >&2
    exit 1
  fi
}

require_cmd cast
require_cmd jq
require_cmd perl
require_cmd curl

NODE_URL=${NODE_URL:-"http://localhost:8545"}
OUTPUT_DIR=${OUTPUT_DIR:-"sub_block_latency_benchmark_results_$(date +%Y%m%d_%H%M%S)"}
CSV_FILE="$OUTPUT_DIR/results.csv"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ERC20_BIN="$SCRIPT_DIR/contracts/bin/LatencyERC20.bin"
SEND_SYNC_TIMEOUT_MS=${SEND_SYNC_TIMEOUT_MS:-"5000"}
GAS_LIMIT_BUFFER=${GAS_LIMIT_BUFFER:-200000}

usage() {
  cat << 'EOF'
Usage:
  benchmark_sub_block_latency.sh <private_key> <nb_transactions_sent> <nb_time_between_transactions_ms>
Arguments:
  private_key                      Hex-encoded private key used to deploy, mint, and transfer tokens.
  nb_transactions_sent             Number of ERC-20 transfer transactions to send for benchmarking.
  nb_time_between_transactions_ms  Delay in milliseconds between two consecutive transfers.
Environment:
  NODE_URL                         RPC endpoint (default: http://localhost:8545).
  OUTPUT_DIR                       Folder where benchmark artefacts are stored.
  ERC20_FUNDING_AMOUNT_WEI         Amount minted to the deployer right after deployment (default: 10^21).
  ERC20_TRANSFER_AMOUNT_WEI        Amount used for each benchmark transfer (default: 10^18).
  SEND_SYNC_TIMEOUT_MS             Timeout passed to eth_sendRawTransactionSync (default: 120000 ms).
  GAS_LIMIT_BUFFER                 Extra gas units added on top of eth_estimateGas (default: 20000).
EOF
  exit 1
}

if [[ "$#" -ne 3 ]]; then
  usage
fi

PRIVATE_KEY="$1"
nb_transactions_sent="$2"
nb_time_between_transactions_ms="$3"

if ! [[ "$nb_transactions_sent" =~ ^[0-9]+$ ]] || [[ "$nb_transactions_sent" -le 0 ]]; then
  echo "Error: <nb_transactions_sent> must be a positive integer." >&2
  exit 1
fi

if ! [[ "$nb_time_between_transactions_ms" =~ ^[0-9]+$ ]]; then
  echo "Error: <nb_time_between_transactions_ms> must be a non-negative integer (milliseconds)." >&2
  exit 1
fi

if [[ ! -f "$ERC20_BIN" ]]; then
  echo "Error: Compiled ERC-20 artifact not found at $ERC20_BIN" >&2
  exit 1
fi

hex_to_dec() {
  local hex="${1:-0x0}"
  perl -e '
    use bigint;
    my $val = hex($ARGV[0]);
    print $val;
  ' "$hex"
}

add_dec() {
  perl -e '
    use bigint;
    print $ARGV[0] + $ARGV[1];
  ' "$1" "$2"
}

apply_margin() {
  local base="$1"
  local margin="${2:-$GAS_LIMIT_BUFFER}"
  add_dec "$base" "$margin"
}

CURRENT_RPC_ID=0
rpc_call() {
  local method="$1"
  local params="${2:-[]}"
  CURRENT_RPC_ID=$((CURRENT_RPC_ID + 1))
  local payload
  payload=$(
    cat << EOF
{"jsonrpc":"2.0","id":$CURRENT_RPC_ID,"method":"$method","params":$params}
EOF
  )
  local response
  if ! response=$(curl -sS -H "Content-Type: application/json" --data "$payload" "$NODE_URL"); then
    echo "Error: RPC call to $method failed." >&2
    exit 1
  fi
  local has_error
  has_error=$(echo "$response" | jq -e '.error // empty' 2> /dev/null || true)
  if [[ -n "$has_error" && "$has_error" != "null" ]]; then
    echo "RPC $method error: $(echo "$response" | jq -r '.error.message')" >&2
    exit 1
  fi
  echo "$response"
}

rpc_get_hex_result() {
  local method="$1"
  local params="${2:-[]}"
  rpc_call "$method" "$params" | jq -r '.result'
}

estimate_gas_dec() {
  local from="$1"
  local to="$2"
  local data="$3"
  local value_hex="$4"
  local call_obj="{\"from\":\"$from\""
  if [[ -n "$to" && "$to" != "0x" ]]; then
    call_obj+=",\"to\":\"$to\""
  fi
  call_obj+=",\"data\":\"$data\""
  if [[ "$value_hex" != "0x0" && "$value_hex" != "0x" ]]; then
    call_obj+=",\"value\":\"$value_hex\""
  fi
  call_obj+="}"

  local response
  response=$(rpc_call "eth_estimateGas" "[$call_obj]")
  hex_to_dec "$(echo "$response" | jq -r '.result')"
}

current_millis() {
  perl -MTime::HiRes -e 'printf("%d\n", int(Time::HiRes::time() * 1000));'
}

sleep_ms() {
  local delay_ms="$1"
  if [[ "$delay_ms" -le 0 ]]; then
    return
  fi
  perl -e 'select undef, undef, undef, $ARGV[0] / 1000;' "$delay_ms"
}

CHAIN_ID_HEX=$(rpc_get_hex_result "eth_chainId")
CHAIN_ID_DEC=$(hex_to_dec "$CHAIN_ID_HEX")
GAS_PRICE_HEX=$(rpc_get_hex_result "eth_gasPrice")
GAS_PRICE_DEC=$(hex_to_dec "$GAS_PRICE_HEX")
SENDER_ADDRESS=$(cast wallet address --private-key "$PRIVATE_KEY")
START_NONCE_HEX=$(rpc_get_hex_result "eth_getTransactionCount" "[\"$SENDER_ADDRESS\",\"pending\"]")
NEXT_NONCE_DEC=$(hex_to_dec "$START_NONCE_HEX")
FUNDED_ADDRESS="$SENDER_ADDRESS"
TRANSFER_RECIPIENT="0x0000000000000000000000000000000000000000"

send_transaction_sync() {
  local to="$1"
  local data="$2"
  local value_dec="$3"
  local gas_limit_dec="$4"
  local description="$5"
  shift 5
  local sig=""
  if [[ "$#" -gt 0 ]]; then
    sig="$1"
    shift
  fi

  local raw_tx
  if [[ -z "$to" || "$to" == "0x" ]]; then
    raw_tx=$(cast mktx \
      --legacy \
      --chain "$CHAIN_ID_DEC" \
      --nonce "$NEXT_NONCE_DEC" \
      --gas-price "$GAS_PRICE_DEC" \
      --gas-limit "$gas_limit_dec" \
      --value "$value_dec" \
      --private-key "$PRIVATE_KEY" \
      --create "$data")
  elif [[ -n "$sig" ]]; then
    raw_tx=$(cast mktx \
      --legacy \
      --chain "$CHAIN_ID_DEC" \
      --nonce "$NEXT_NONCE_DEC" \
      --gas-price "$GAS_PRICE_DEC" \
      --gas-limit "$gas_limit_dec" \
      --value "$value_dec" \
      --private-key "$PRIVATE_KEY" \
      "$to" \
      "$sig" \
      "$@")
  else
    echo "Error: $description requires a function signature when sending to a contract." >&2
    exit 1
  fi

  echo "Sending $description transaction (nonce $NEXT_NONCE_DEC)..." >&2
  local response
  response=$(rpc_call "eth_sendRawTransactionSync" "[\"$raw_tx\",\"$SEND_SYNC_TIMEOUT_MS\"]")
  NEXT_NONCE_DEC=$(add_dec "$NEXT_NONCE_DEC" 1)

  local tx_hash
  tx_hash=$(echo "$response" | jq -r '.result.transactionHash // empty')
  if [[ -z "$tx_hash" ]]; then
    echo "Error: $description did not return a transaction hash." >&2
    echo "$response" >&2
    exit 1
  fi
  SEND_TX_RESPONSE="$response"
}

status_is_success() {
  case "$1" in
  0x1 | 1 | true | True | TRUE) return 0 ;;
  *) return 1 ;;
  esac
}

ERC20_FUNDING_AMOUNT_WEI=${ERC20_FUNDING_AMOUNT_WEI:-1000000000000000000000}
ERC20_TRANSFER_AMOUNT_WEI=${ERC20_TRANSFER_AMOUNT_WEI:-1000000000000000000}

mkdir -p "$OUTPUT_DIR"
echo "transaction_index,time_for_receipt_ms" > "$CSV_FILE"
latencies=()
SEND_TX_RESPONSE=""

timestamp_label=$(date -u +%Y%m%d%H%M%S)
TOKEN_NAME="LatencyToken_${timestamp_label}"
TOKEN_SYMBOL="LAT${timestamp_label: -4}"

echo "Deploying ERC-20 '$TOKEN_NAME' ($TOKEN_SYMBOL) from $SENDER_ADDRESS ..."
BYTECODE=$(tr -d '\n\r' < "$ERC20_BIN")
if [[ -z "$BYTECODE" || "$BYTECODE" == "null" ]]; then
  echo "Error: failed to read bytecode from $ERC20_BIN" >&2
  exit 1
fi
CONSTRUCTOR_ARGS=$(cast abi-encode "constructor(string,string)" "$TOKEN_NAME" "$TOKEN_SYMBOL")
CONSTRUCTOR_HEX=${CONSTRUCTOR_ARGS#0x}
if [[ -n "$CONSTRUCTOR_HEX" ]]; then
  DEPLOY_DATA="0x${BYTECODE#0x}${CONSTRUCTOR_HEX}"
else
  DEPLOY_DATA="0x${BYTECODE#0x}"
fi
DEPLOY_GAS_DEC=$(estimate_gas_dec "$SENDER_ADDRESS" "" "$DEPLOY_DATA" "0x0")
DEPLOY_GAS_LIMIT_DEC=$(apply_margin "$DEPLOY_GAS_DEC")
send_transaction_sync "" "$DEPLOY_DATA" "0" "$DEPLOY_GAS_LIMIT_DEC" "Deployment"
DEPLOY_RECEIPT="$SEND_TX_RESPONSE"
CONTRACT_ADDRESS=$(echo "$DEPLOY_RECEIPT" | jq -r '.result.contractAddress // empty')
if [[ -z "$CONTRACT_ADDRESS" || "$CONTRACT_ADDRESS" == "null" ]]; then
  echo "Error: deployment receipt missing contract address." >&2
  exit 1
fi
echo "ERC-20 deployed at $CONTRACT_ADDRESS"

total_mint_amount=$(perl -e '
  use bigint;
  my ($fund, $per, $count) = @ARGV;
  print $fund + $per * $count;
' "$ERC20_FUNDING_AMOUNT_WEI" "$ERC20_TRANSFER_AMOUNT_WEI" "$nb_transactions_sent")
MINT_CALLDATA=$(cast calldata "mint(address,uint256)" "$FUNDED_ADDRESS" "$total_mint_amount")
MINT_GAS_DEC=$(estimate_gas_dec "$SENDER_ADDRESS" "$CONTRACT_ADDRESS" "$MINT_CALLDATA" "0x0")
MINT_GAS_LIMIT_DEC=$(apply_margin "$MINT_GAS_DEC")
send_transaction_sync "$CONTRACT_ADDRESS" "$MINT_CALLDATA" "0" "$MINT_GAS_LIMIT_DEC" "Mint" "mint(address,uint256)" "$FUNDED_ADDRESS" "$total_mint_amount"
MINT_RECEIPT="$SEND_TX_RESPONSE"
MINT_STATUS=$(echo "$MINT_RECEIPT" | jq -r '.result.status // empty')
if ! status_is_success "$MINT_STATUS"; then
  echo "Warning: mint transaction failed according to receipt." >&2
fi

TRANSFER_CALLDATA=$(cast calldata "transfer(address,uint256)" "$TRANSFER_RECIPIENT" "$ERC20_TRANSFER_AMOUNT_WEI")
TRANSFER_GAS_DEC=$(estimate_gas_dec "$SENDER_ADDRESS" "$CONTRACT_ADDRESS" "$TRANSFER_CALLDATA" "0x0")
TRANSFER_GAS_LIMIT_DEC=$(apply_margin "$TRANSFER_GAS_DEC")

echo "Starting benchmark: $nb_transactions_sent transfers, ${nb_time_between_transactions_ms}ms apart."
for ((i = 1; i <= nb_transactions_sent; i++)); do
  start_ms=$(current_millis)
  send_transaction_sync "$CONTRACT_ADDRESS" "$TRANSFER_CALLDATA" "0" "$TRANSFER_GAS_LIMIT_DEC" "Transfer #$i" "transfer(address,uint256)" "$TRANSFER_RECIPIENT" "$ERC20_TRANSFER_AMOUNT_WEI"
  TRANSFER_RECEIPT="$SEND_TX_RESPONSE"
  status_value=$(echo "$TRANSFER_RECEIPT" | jq -r '.result.status // empty')
  if ! status_is_success "$status_value"; then
    echo "Warning: transfer #$i failed (status=$status_value)." >&2
  fi
  end_ms=$(current_millis)
  latency_ms=$((end_ms - start_ms))
  latencies+=("$latency_ms")
  echo "$i,$latency_ms" >> "$CSV_FILE"
  tx_hash=$(echo "$TRANSFER_RECEIPT" | jq -r '.result.transactionHash // empty')
  echo "Transfer #$i confirmed in ${latency_ms}ms (tx: $tx_hash)"
  if [[ "$i" -lt "$nb_transactions_sent" ]]; then
    sleep_ms "$nb_time_between_transactions_ms"
  fi
done

trimmed_avg=""
p80_cutoff=""
if ((${#latencies[@]} > 0)); then
  sorted_latencies=()
  while IFS= read -r value; do
    [[ -z "$value" ]] && continue
    sorted_latencies+=("$value")
  done < <(printf "%s\n" "${latencies[@]}" | sort -n)
  count=${#sorted_latencies[@]}
  limit=$(((count * 8 + 9) / 10))
  ((limit < 1)) && limit=1
  ((limit > count)) && limit=$count
  p80_cutoff=${sorted_latencies[limit - 1]}
  sum=0
  for ((idx = 0; idx < limit; idx++)); do
    sum=$((sum + sorted_latencies[idx]))
  done
  trimmed_avg=$(perl -e 'printf("%.2f", $ARGV[0] / $ARGV[1]);' "$sum" "$limit")
  echo "P80-trimmed mean latency: ${trimmed_avg} ms (P80 cutoff ${p80_cutoff} ms)"
fi

cat << EOF > "$OUTPUT_DIR/metadata.txt"
ERC20_NAME=$TOKEN_NAME
ERC20_SYMBOL=$TOKEN_SYMBOL
ERC20_CONTRACT=$CONTRACT_ADDRESS
DEPLOYER=$SENDER_ADDRESS
FUNDED_ADDRESS=$FUNDED_ADDRESS
TRANSFER_RECIPIENT=$TRANSFER_RECIPIENT
FUNDING_AMOUNT_WEI=$ERC20_FUNDING_AMOUNT_WEI
MINTED_TOTAL_WEI=$total_mint_amount
TRANSFER_AMOUNT_WEI=$ERC20_TRANSFER_AMOUNT_WEI
NODE_URL=$NODE_URL
GAS_PRICE_WEI=$GAS_PRICE_DEC
TOTAL_TRANSACTIONS=$nb_transactions_sent
INTERVAL_MS=$nb_time_between_transactions_ms
P80_MEAN_MS=$trimmed_avg
P80_CUTOFF_MS=$p80_cutoff
EOF

echo "Benchmark complete. Results saved to $CSV_FILE"
