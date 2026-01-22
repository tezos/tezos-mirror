#!/usr/bin/env bash
# Benchmark script measuring latency using eth_sendRawTransactionSync

set -euo pipefail

NODE_URL=${NODE_URL:-"http://localhost:8545"}
OUTPUT_DIR=${OUTPUT_DIR:-"ic_benchmark_results_$(date +%Y%m%d_%H%M%S)"}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ERC20_BIN="$SCRIPT_DIR/contracts/bin/LatencyERC20.bin"
SEND_SYNC_TIMEOUT_MS=${SEND_SYNC_TIMEOUT_MS:-"5000"}
GAS_LIMIT_BUFFER=${GAS_LIMIT_BUFFER:-200000}
# Curl timeout = RPC timeout + 5s buffer (in seconds)
CURL_TIMEOUT=$(((SEND_SYNC_TIMEOUT_MS / 1000) + 5))

if [[ "$#" -lt 2 ]] || [[ "$#" -gt 3 ]]; then
  echo "Usage: $0 <private_key> <nb_transactions> [percentile]"
  echo "Arguments:"
  echo "  private_key           Private key for signing transactions"
  echo "  nb_transactions       Number of transfer transactions to benchmark"
  echo "  percentile            Percentile for trimmed mean (50-100, default: 80)"
  echo "Environment variables:"
  echo "  NODE_URL              RPC endpoint (default: http://localhost:8545)"
  echo "  SEND_SYNC_TIMEOUT_MS  Timeout for eth_sendRawTransactionSync (default: 5000)"
  exit 1
fi

PRIVATE_KEY="$1"
NB_TX="$2"
PERCENTILE="${3:-80}"

# Validate percentile
if ! [[ "$PERCENTILE" =~ ^[0-9]+$ ]] || ((PERCENTILE < 50)) || ((PERCENTILE > 100)); then
  echo "Error: percentile must be a number between 50 and 100" >&2
  exit 1
fi

if [[ ! -f "$ERC20_BIN" ]]; then
  echo "Error: $ERC20_BIN not found" >&2
  exit 1
fi

current_millis() { perl -MTime::HiRes -e 'printf("%d\n", int(Time::HiRes::time() * 1000));'; }

RPC_RESPONSE="" RPC_LATENCY_MS=0 RPC_ID=0
rpc_call() {
  local method="$1" params="${2:-[]}" start_ms err curl_exit
  RPC_ID=$((RPC_ID + 1))
  start_ms=$(current_millis)

  # Use timeout to prevent hanging
  set +e
  RPC_RESPONSE=$(curl -sS --max-time "$CURL_TIMEOUT" -H "Content-Type: application/json" \
    --data "{\"jsonrpc\":\"2.0\",\"id\":$RPC_ID,\"method\":\"$method\",\"params\":$params}" "$NODE_URL" 2>&1)
  curl_exit=$?
  set -e

  RPC_LATENCY_MS=$(($(current_millis) - start_ms))

  # Handle curl errors
  if [[ $curl_exit -ne 0 ]]; then
    echo "Error: curl failed with exit code $curl_exit" >&2
    if [[ $curl_exit -eq 28 ]]; then
      echo "  Timeout after ${CURL_TIMEOUT}s waiting for $method response" >&2
      echo "  Make sure the targeted observer enables instant confirmations" >&2
      echo "  Try increasing SEND_SYNC_TIMEOUT_MS (current: ${SEND_SYNC_TIMEOUT_MS}ms)" >&2
    elif [[ $curl_exit -eq 7 ]]; then
      echo "  Could not connect to $NODE_URL - is the node running?" >&2
    fi
    exit 1
  fi

  # Handle RPC errors
  err=$(echo "$RPC_RESPONSE" | jq -r '.error.message // empty' 2> /dev/null || echo "")
  if [[ -n "$err" ]]; then
    echo "RPC $method error: $err" >&2
    # Provide helpful context for common errors
    if [[ "$err" == *"Nonce too low"* ]]; then
      echo "  The nonce is stale - a transaction with this nonce was already included" >&2
    elif [[ "$err" == *"stale"* ]]; then
      echo "  The transaction became stale (took too long to be included)" >&2
    elif [[ "$err" == *"insufficient"* ]] || [[ "$err" == *"gas"* ]]; then
      echo "  Gas estimation issue - try increasing GAS_LIMIT_BUFFER (current: $GAS_LIMIT_BUFFER)" >&2
    elif [[ "$err" == *"already known"* ]]; then
      echo "  Transaction already in mempool - likely a duplicate submission" >&2
    fi
    exit 1
  fi

  # Validate we got a proper JSON response
  if ! echo "$RPC_RESPONSE" | jq -e '.result' > /dev/null 2>&1; then
    echo "Error: Invalid RPC response from $method" >&2
    echo "  Response: ${RPC_RESPONSE:0:200}" >&2
    exit 1
  fi
}

echo "Connecting to $NODE_URL..."
SENDER=$(cast wallet address --private-key "$PRIVATE_KEY")
echo "Sender: $SENDER"

rpc_call "eth_chainId"
CHAIN_ID=$(printf "%d" "$(echo "$RPC_RESPONSE" | jq -r '.result')")
echo "Chain ID: $CHAIN_ID"

rpc_call "eth_gasPrice"
GAS_PRICE=$(printf "%d" "$(echo "$RPC_RESPONSE" | jq -r '.result')")
echo "Gas price: $GAS_PRICE wei"

rpc_call "eth_getTransactionCount" "[\"$SENDER\",\"pending\"]"
NONCE=$(printf "%d" "$(echo "$RPC_RESPONSE" | jq -r '.result')")
echo "Starting nonce: $NONCE"

send_sync() {
  local raw_tx="$1" desc="$2" tx status
  echo "Sending $desc (nonce $NONCE)..." >&2
  rpc_call "eth_sendRawTransactionSync" "[\"$raw_tx\",\"$SEND_SYNC_TIMEOUT_MS\",\"pending\"]"
  NONCE=$((NONCE + 1))

  tx=$(echo "$RPC_RESPONSE" | jq -r '.result.transactionHash // empty')
  if [[ -z "$tx" ]]; then
    echo "Error: $desc did not return transaction hash" >&2
    echo "  Response: ${RPC_RESPONSE:0:500}" >&2
    # Check for stale/timeout in result
    status=$(echo "$RPC_RESPONSE" | jq -r '.result // empty')
    if [[ "$status" == *"stale"* ]] || [[ "$status" == *"timeout"* ]]; then
      echo "  Transaction timed out waiting for receipt" >&2
    fi
    exit 1
  fi
  echo "Hash: $tx"
}

# Deploy ERC-20
echo ""
echo "=== Deploying ERC-20 contract ==="
BYTECODE=$(tr -d '\n\r' < "$ERC20_BIN")
CONSTRUCTOR=$(cast abi-encode "constructor(string,string)" "LatencyToken" "LAT")
DEPLOY_DATA="0x${BYTECODE#0x}${CONSTRUCTOR#0x}"
DEPLOY_GAS=$(cast estimate --create "$DEPLOY_DATA" -f "$SENDER" -r "$NODE_URL" 2> /dev/null || echo "12000000")
echo "Estimated deploy gas: $DEPLOY_GAS (+ $GAS_LIMIT_BUFFER buffer)"
RAW_TX=$(cast mktx --legacy --chain "$CHAIN_ID" --nonce "$NONCE" --gas-price "$GAS_PRICE" \
  --gas-limit $((DEPLOY_GAS + GAS_LIMIT_BUFFER)) --private-key "$PRIVATE_KEY" --create "$DEPLOY_DATA")
send_sync "$RAW_TX" "Deploy"
CONTRACT=$(echo "$RPC_RESPONSE" | jq -r '.result.contractAddress')
echo "Deployed at $CONTRACT"

# Mint tokens
echo ""
echo "=== Minting tokens ==="
MINT_AMOUNT="1000000000000000000000000"
MINT_GAS=$(cast estimate "$CONTRACT" "mint(address,uint256)" "$SENDER" "$MINT_AMOUNT" -f "$SENDER" -r "$NODE_URL" 2> /dev/null || echo "100000")
echo "Estimated mint gas: $MINT_GAS (+ $GAS_LIMIT_BUFFER buffer)"
RAW_TX=$(cast mktx --legacy --chain "$CHAIN_ID" --nonce "$NONCE" --gas-price "$GAS_PRICE" \
  --gas-limit $((MINT_GAS + GAS_LIMIT_BUFFER)) --private-key "$PRIVATE_KEY" \
  "$CONTRACT" "mint(address,uint256)" "$SENDER" "$MINT_AMOUNT")
send_sync "$RAW_TX" "Mint"
echo "Minted $MINT_AMOUNT tokens"

# Prepare transfer
RECIPIENT="0x0000000000000000000000000000000000000001"
TRANSFER_AMOUNT="1000000000000000000"
TRANSFER_GAS=$(cast estimate "$CONTRACT" "transfer(address,uint256)" "$RECIPIENT" "$TRANSFER_AMOUNT" -f "$SENDER" -r "$NODE_URL" 2> /dev/null || echo "100000")
TRANSFER_GAS_LIMIT=$((TRANSFER_GAS + GAS_LIMIT_BUFFER))
echo ""
echo "=== Starting benchmark ==="
echo "Transfer gas limit: $TRANSFER_GAS_LIMIT"

# Benchmark
mkdir -p "$OUTPUT_DIR"
CSV_FILE="$OUTPUT_DIR/results.csv"
echo "index,latency_ms" > "$CSV_FILE"
latencies=()

echo "Running $NB_TX transfers..."
for ((i = 1; i <= NB_TX; i++)); do
  RAW_TX=$(cast mktx --legacy --chain "$CHAIN_ID" --nonce "$NONCE" --gas-price "$GAS_PRICE" \
    --gas-limit "$TRANSFER_GAS_LIMIT" --private-key "$PRIVATE_KEY" \
    "$CONTRACT" "transfer(address,uint256)" "$RECIPIENT" "$TRANSFER_AMOUNT")
  send_sync "$RAW_TX" "Transfer #$i"
  latencies+=("$RPC_LATENCY_MS")
  echo "$i,$RPC_LATENCY_MS" >> "$CSV_FILE"
  echo "Transfer #$i: ${RPC_LATENCY_MS}ms"
done

# Statistics
echo ""
echo "=== Results ==="
if ((${#latencies[@]} > 0)); then
  mapfile -t sorted < <(printf '%s\n' "${latencies[@]}" | sort -n)
  count=${#sorted[@]}
  p_idx=$(((count * PERCENTILE) / 100))
  if ((p_idx >= count)); then p_idx=$((count - 1)); fi
  sum=0
  for ((j = 0; j <= p_idx; j++)); do sum=$((sum + sorted[j])); done
  echo "P${PERCENTILE}-trimmed mean: $((sum / (p_idx + 1)))ms (P${PERCENTILE} cutoff: ${sorted[p_idx]}ms)"
  echo "Min: ${sorted[0]}ms, Max: ${sorted[count - 1]}ms"
fi

echo "Results saved to $CSV_FILE"
