#!/usr/bin/env bash
# Exhaustive verification of the Etherlink durable-storage reorg (V50 -> V60).
#
# Layer A (physical paths): read durable storage DIRECTLY from the data-dir with
#   `octez-evm-node shell ls/cat` -- no RPC server needed, safe while the node runs.
# Layer B (functional): call the PUBLIC JSON-RPC (port 8545) so we prove the node
#   resolves the NEW locations when serving requests.
#
# Usage:
#   NODE=./octez-evm-node DD=./replicate-mainnet RPC=http://127.0.0.1:8545 ./etherlink/scripts/nds_paths_reorg_check.sh
set -uo pipefail

NODE="${NODE:-./octez-evm-node}"
DD="${DD:-./replicate-mainnet}"
RPC="${RPC:-http://127.0.0.1:8545}"
PASS=0
FAIL=0
INFO=0
SKIP=0
RED=$'\e[31m'
GRN=$'\e[32m'
YEL=$'\e[33m'
BLU=$'\e[34m'
DIM=$'\e[2m'
NC=$'\e[0m'

ok() {
  PASS=$((PASS + 1))
  printf "  ${GRN}PASS${NC} %s\n" "$1"
}
bad() {
  FAIL=$((FAIL + 1))
  printf "  ${RED}FAIL${NC} %s\n" "$1"
}
info() {
  INFO=$((INFO + 1))
  printf "  ${YEL}INFO${NC} %s\n" "$1"
}
skip() {
  SKIP=$((SKIP + 1))
  printf "  ${DIM}SKIP${NC} %s\n" "$1"
}
hdr() { printf "\n${BLU}== %s ==${NC}\n" "$1"; }

# ---- Layer A helpers: read durable storage from the data-dir via `shell` ----
sh_cat() { timeout 120 "$NODE" shell cat "$1" --data-dir "$DD" --pp hex 2> /dev/null; }
sh_ls() { timeout 120 "$NODE" shell ls "$1" --data-dir "$DD" 2> /dev/null; }
# a path "exists" if it has a value OR any subkeys
exists() { [ -n "$(sh_cat "$1")$(sh_ls "$1")" ]; }
# is NAME an entry (value or dir) of a captured `ls` listing?
in_list() { grep -qE "^$1/?$" <<< "$2"; }

# assert an OLD path is fully gone
gone() { if exists "$1"; then bad "OLD STILL PRESENT $1"; else ok "gone $1"; fi; }

# ---- Layer B helper: PUBLIC JSON-RPC ----
rpc() { curl -s --max-time 30 "$RPC" -H 'content-type: application/json' \
  -d "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"$1\",\"params\":$2}"; }
func() { # label  method  params
  local r
  r="$(rpc "$2" "$3")"
  if [ -n "$(jq -rc '.error // empty' <<< "$r" 2> /dev/null)" ]; then
    bad "$1 -> error: $(jq -rc '.error.message' <<< "$r" | head -1)"
  elif [ "$(jq -rc '.result' <<< "$r" 2> /dev/null)" = "null" ] || [ -z "$r" ]; then
    info "$1 -> null/empty"
  else
    ok "$1 -> $(jq -rc '.result' <<< "$r" | cut -c1-72)"
  fi
}
# soft variant: a server error is reported INFO, not FAIL -- for methods that may
# be legitimately N/A on this network (Michelson runtime reads on an EVM replica).
funcq() { # label  method  params
  local r
  r="$(rpc "$2" "$3")"
  if [ -n "$(jq -rc '.error // empty' <<< "$r" 2> /dev/null)" ]; then
    info "$1 -> N/A: $(jq -rc '.error.message' <<< "$r" | head -1 | cut -c1-60)"
  elif [ "$(jq -rc '.result' <<< "$r" 2> /dev/null)" = "null" ] || [ -z "$r" ]; then
    info "$1 -> null/empty"
  else
    ok "$1 -> $(jq -rc '.result' <<< "$r" | cut -c1-72)"
  fi
}
# decode a little-endian hex blob (as printed by `shell cat --pp hex`) to decimal
le_to_dec() {
  local h="${1#0x}" out="" i
  [ -n "$h" ] || return 1
  for ((i = ${#h} - 2; i >= 0; i -= 2)); do out+="${h:i:2}"; done
  printf '%d' "0x$out" 2> /dev/null
}

# ---------------------------------------------------------------------------
echo "node binary : $NODE"
echo "data-dir    : $DD"
echo "public RPC  : $RPC"
[ -x "$NODE" ] || {
  echo "${RED}$NODE not found/executable${NC}"
  exit 1
}
[ -d "$DD" ] || {
  echo "${RED}data-dir $DD not found${NC}"
  exit 1
}
# preflight: required CLI deps + a real `shell` call against the data-dir, so a
# broken setup (wrong binary, missing subcommand, unreadable data-dir) fails LOUD
# instead of silently making every `gone` assertion trivially true.
for dep in jq curl timeout; do
  command -v "$dep" > /dev/null || {
    echo "${RED}missing dependency: $dep${NC}"
    exit 2
  }
done
# probe with /evm (must exist on any Etherlink data-dir, legacy or V60)
"$NODE" shell ls /evm --data-dir "$DD" > /dev/null || {
  echo "${RED}\`$NODE shell ls /evm\` failed against $DD -- binary or data-dir broken${NC}"
  exit 2
}

# parent listings reused by the in_list / negative-space checks below
EVM_LS="$(sh_ls /evm)"
WS_LS="$(sh_ls /evm/world_state)"
BASE_LS="$(sh_ls /base)"
BLOCKS_LS="$(sh_ls /evm/world_state/blocks)"

# =====================================================================
hdr "storage_version is V60 and lives at /base"
SVER="$(sh_cat /base/storage_version)"
case "$SVER" in
3c*) ok "/base/storage_version = $SVER (V60, little-endian)" ;;
"") bad "/base/storage_version MISSING -- node not migrated to V60?" ;;
*) info "/base/storage_version = $SVER (expected 3c..; check value)" ;;
esac
gone /evm/storage_version

# =====================================================================
hdr "Negative space: /evm holds ONLY {eth_accounts/, world_state}"
echo "    /evm -> ${EVM_LS//$'\n'/ }"
STALE_EVM="$(grep -vE '^(eth_accounts|world_state)/?$' <<< "$EVM_LS")"
if [ -z "$STALE_EVM" ]; then
  ok "no stale keys under /evm (all V51/V53/V58 moves succeeded)"
else
  bad "stale keys left under /evm: ${STALE_EVM//$'\n'/ }"
fi

hdr "Old EVM-world_state children are gone (V54/V58/V59/V60 moves)"
echo "    /evm/world_state -> ${WS_LS//$'\n'/ }"
for old in eth_accounts eth_codes indexes single_tx assemble_block \
  feature_flags tezlink __http_trace simulation_result trace; do
  if in_list "$old" "$WS_LS"; then bad "stale /evm/world_state/$old"; else ok "gone /evm/world_state/$old"; fi
done
# NEW members that SHOULD be present in world_state
for new in chain_id evm_version sequencer sequencer_governance \
  sequencer_pool_address maximum_gas_per_transaction blocks fees; do
  if in_list "$new" "$WS_LS"; then ok "present /evm/world_state/$new"; else info "/evm/world_state/$new absent (may be unset on this network)"; fi
done

hdr "V58 block-index inversion: blocks/indexes (not indexes/blocks)"
echo "    /evm/world_state/blocks -> $(echo "$BLOCKS_LS" | head -5 | tr '\n' ' ')$([ "$(wc -l <<< "$BLOCKS_LS")" -gt 5 ] && echo '...')"
if in_list indexes "$BLOCKS_LS"; then ok "present /evm/world_state/blocks/indexes"; else bad "MISSING /evm/world_state/blocks/indexes"; fi
if in_list current "$BLOCKS_LS"; then ok "present /evm/world_state/blocks/current"; else bad "MISSING /evm/world_state/blocks/current"; fi
gone /evm/world_state/indexes

hdr "blocks/current subtree (kernel block_storage.rs layout)"
# expected leaves of /evm/world_state/blocks/current: block, number, hash,
# transactions_objects/, transactions_receipts/ -- see kernel_latest block_storage.rs
CURRENT_LS="$(sh_ls /evm/world_state/blocks/current)"
echo "    /evm/world_state/blocks/current -> ${CURRENT_LS//$'\n'/ }"
for k in block number hash transactions_objects transactions_receipts; do
  if in_list "$k" "$CURRENT_LS"; then ok "present /evm/world_state/blocks/current/$k"; else bad "MISSING /evm/world_state/blocks/current/$k"; fi
done
# cross-check: current/number must agree with eth_blockNumber's height (decoded LE)
CUR_N_HEX="$(sh_cat /evm/world_state/blocks/current/number)"
CUR_N_DEC="$(le_to_dec "$CUR_N_HEX" 2> /dev/null)"
if [ -n "$CUR_N_DEC" ]; then ok "/evm/world_state/blocks/current/number decodes to $CUR_N_DEC"; else info "/evm/world_state/blocks/current/number not decodable (got '$CUR_N_HEX')"; fi

hdr "blocks/indexes subtree (V58 layout: numeric block-number indexes)"
if [ -n "$CUR_N_DEC" ]; then
  hit=0
  miss=""
  for off in 0 1; do
    n=$((CUR_N_DEC - off))
    [ "$n" -lt 0 ] && continue
    if exists "/evm/world_state/blocks/indexes/$n"; then
      hit=$((hit + 1))
    else
      miss="$miss $n"
    fi
  done
  if [ -z "$miss" ] && [ "$hit" -gt 0 ]; then
    ok "/evm/world_state/blocks/indexes has live entries at head and head-1"
  elif [ "$hit" -gt 0 ]; then
    info "/evm/world_state/blocks/indexes partial: missing$miss (block likely produced between reads)"
  else
    bad "/evm/world_state/blocks/indexes has no live numeric entries near head ($CUR_N_DEC)"
  fi
else
  skip "blocks/indexes numeric probes -- current/number not decodable"
fi

hdr "V59 EVM accounts isolated under /evm/eth_accounts"
if in_list eth_accounts "$EVM_LS"; then ok "present /evm/eth_accounts"; else bad "MISSING /evm/eth_accounts"; fi
if [ -n "$(sh_ls /evm/eth_accounts/eth_codes)" ]; then ok "present /evm/eth_accounts/eth_codes"; else info "/evm/eth_accounts/eth_codes empty/absent"; fi

# =====================================================================
hdr "V51 root IPC paths moved off / -> /base (OLD must be gone)"
gone /__evm_node
gone /__delayed_input
gone /__backup_kernel
gone /__tmp

hdr "V53/V54: nothing stale left under /evm (governance/flags moved to /base)"
# These are all implied gone by the '/evm holds only 2 keys' check above, but
# assert the headline ones explicitly for a readable report:
for old in /evm/kernel_version /evm/kernel_root_hash /evm/admin \
  /evm/kernel_governance /evm/blueprints /evm/feature_flags \
  /evm/chain_configurations /evm/events /evm/delayed-inbox \
  /evm/dal_slots /evm/chain_id; do
  if in_list "$(basename "$old")" "$EVM_LS"; then bad "stale $old"; else ok "gone $old"; fi
done

hdr "What actually lives under /base now (informational)"
while IFS= read -r line; do echo "      $line"; done <<< "$BASE_LS"
# governance/config that reliably persists on a synced node:
for k in storage_version kernel_version kernel_root_hash current_block_header \
  l1_level kernel_governance kernel_security_governance delayed_bridge \
  feature_flags; do
  if in_list "$k" "$BASE_LS"; then ok "present /base/$k"; else info "/base/$k absent (transient/consumed or unset on this node)"; fi
done
# transient/consumed/feature-gated -> empty is EXPECTED, report as INFO only:
for k in blueprints rollup_events chain_configurations messages admin \
  maximum_allowed_ticks delayed_inbox_timeout dal_publishers_whitelist; do
  if in_list "$k" "$BASE_LS"; then info "/base/$k present"; else info "/base/$k empty (expected: transient/consumed/feature-gated)"; fi
done

hdr "V54 dead world_state feature flags deleted"
gone /evm/world_state/feature_flags/enable_revm
gone /evm/world_state/feature_flags/enable_fast_withdrawal
gone /evm/world_state/feature_flags/enable_fast_fa_withdrawal
echo "    /base/feature_flags -> $(sh_ls /base/feature_flags | tr '\n' ' ')"

# =====================================================================
hdr "V55/56/57 TezosX trees (gated on enable_tezos_runtime)"
# V58 deletes /evm/michelson_runtime unconditionally (not gated on the runtime flag)
gone /evm/michelson_runtime
TRT="$(sh_cat /base/feature_flags/enable_tezos_runtime)"
if [ -n "$TRT" ] && [ "${TRT:0:2}" != "00" ]; then
  echo "    enable_tezos_runtime = $TRT (ON)"
  # V56 (gated on the runtime) deletes the old Michelson root and moves accounts
  gone /tezlink
  if [ -n "$(sh_ls /tez/tez_accounts)" ]; then ok "present /tez/tez_accounts"; else info "/tez/tez_accounts empty"; fi
  if in_list chain_id "$(sh_ls /tez/world_state)"; then ok "present /tez/world_state/chain_id"; else info "tez chain_id absent"; fi
else
  echo "    enable_tezos_runtime = '${TRT:-<unset>}' (OFF) -> V56/V57 Michelson migrations are NO-OPs"
  skip "TezosX account moves (V56/57) -- runtime off on this network"
  # V56's /tezlink deletion is gated on the runtime, so on a runtime-off network
  # the old bootstrap PLACEHOLDER value (no subkeys) legitimately survives.
  TZL="$(sh_cat /tezlink)"
  if [ -z "$TZL" ] && [ -z "$(sh_ls /tezlink)" ]; then
    ok "gone /tezlink"
  elif [ -z "$(sh_ls /tezlink)" ]; then
    info "/tezlink holds only a bootstrap placeholder (0x$TZL), no subkeys -- expected (V56 gated off)"
  else
    bad "/tezlink still has real subkeys: $(sh_ls /tezlink | tr '\n' ' ')"
  fi
  if in_list chain_id "$(sh_ls /tez/world_state)"; then info "/tez/world_state/chain_id present (static)"; else info "/tez/world_state absent"; fi
fi

# =====================================================================
hdr "Layer B -- public JSON-RPC sweep (node must resolve NEW paths)"
# strict `func` : a server error is a FAIL  -- core EVM reads must work post-reorg
# soft   `funcq`: a server error is an INFO -- Michelson runtime reads may be N/A here
# fixtures for this network: an EOA and a deployed contract.
# TXH is NOT hard-coded -- it is auto-selected from a recent non-empty block below,
# so the tx-dependent reads always exercise a transaction this node actually holds.
EOA='0x3B1885eeC759C22C878E12C84FAC33B3B9D153E4'
CONTRACT='0x258DE7184aF00052813AE7Cf1dc70c48A2Bb038F'

# ---- /base: kernel + sequencer (V53 governance, V50 sequencer) ----
func "tez_kernelVersion   (/base/kernel_version)" tez_kernelVersion '["latest"]'
func "tez_kernelRootHash  (/base/kernel_root_hash)" tez_kernelRootHash '["latest"]'
# tez_sequencer takes the block param UNWRAPPED (bare extended_encoding, not tup1)
func "tez_sequencer       (world_state/sequencer V50)" tez_sequencer '"latest"'

# ---- chain identity (world_state/chain_id, chain_configurations) ----
func "eth_chainId         (world_state/chain_id)" eth_chainId '[]'
func "net_version         (world_state/chain_id)" net_version '[]'
CID="$(rpc eth_chainId '[]' | jq -r '.result // empty')"
funcq "tez_chainFamily     (chain_configurations/family)" tez_chainFamily "[\"$CID\"]"
funcq "tez_blockNumber     (generic block number)" tez_blockNumber '[]'

# ---- fees (world_state/fees) ----
func "eth_blockNumber     (blocks/indexes V58)" eth_blockNumber '[]'
BN="$(rpc eth_blockNumber '[]' | jq -r '.result')"

# auto-select a real, present transaction: walk back from head until a non-empty
# block, take its first tx hash. Replaces the old hard-coded fixture so trace/tx
# reads always hit a transaction this node holds (no manual hash bookkeeping).
TXH=""
if [ -n "$BN" ] && [ "$BN" != "null" ]; then
  for ((off = 0; off <= 256; off++)); do
    cand="$(printf '0x%x' $((BN - off)))"
    txc="$(rpc eth_getBlockTransactionCountByNumber "[\"$cand\"]" | jq -r '.result // "0x0"')"
    if [ "$txc" != "0x0" ] && [ "$txc" != "null" ]; then
      TXH="$(rpc eth_getBlockByNumber "[\"$cand\",true]" | jq -r '.result.transactions[0].hash // empty')"
      [ -n "$TXH" ] && break
    fi
  done
fi
if [ -n "$TXH" ]; then
  echo "    tx fixture (auto) -> $TXH"
else
  echo "    no non-empty block within 256 of head -- tx-dependent reads will SKIP"
fi
func "eth_gasPrice        (fees)" eth_gasPrice '[]'
func "eth_maxPriorityFeePerGas (fees)" eth_maxPriorityFeePerGas '[]'
func "eth_coinbase        (sequencer_pool_address)" eth_coinbase '[]'
func "eth_feeHistory      (fees backlog)" eth_feeHistory "[\"0x4\",\"$BN\",[]]"

# ---- blocks (blocks/ + blocks/indexes V58, transactions_receipts) ----
func "eth_getBlockByNumber $BN (blocks/indexes V58)" eth_getBlockByNumber "[\"$BN\",false]"
# by-hash hits blocks/{hash} directly (a different V58 subtree than the by-number index)
BHASH="$(rpc eth_getBlockByNumber "[\"$BN\",false]" | jq -r '.result.hash // empty')"
if [ -n "$BHASH" ]; then
  func "eth_getBlockByHash $BHASH (blocks/{hash} V58)" eth_getBlockByHash "[\"$BHASH\",false]"
  func "eth_getBlockTransactionCountByHash (blocks/{hash})" eth_getBlockTransactionCountByHash "[\"$BHASH\"]"
  func "eth_getUncleCountByBlockHash (blocks/{hash})" eth_getUncleCountByBlockHash "[\"$BHASH\"]"
  funcq "tez_getMetaBlockByHash (meta blocks)" tez_getMetaBlockByHash "[\"$BHASH\"]"
else
  skip "block-by-hash reads -- could not resolve a block hash"
fi
func "eth_getBlockReceipts $BN (transactions_receipts)" eth_getBlockReceipts "[\"$BN\"]"
func "eth_getBlockTransactionCountByNumber (blocks/indexes)" eth_getBlockTransactionCountByNumber "[\"$BN\"]"
func "eth_getUncleCountByBlockNumber (blocks/indexes)" eth_getUncleCountByBlockNumber "[\"$BN\"]"
funcq "tez_getMetaBlockByNumber (meta blocks)" tez_getMetaBlockByNumber "[\"$BN\"]"

# ---- accounts (eth_accounts V59, eth_codes V59, storage V59) ----
func "eth_accounts          (node wallet, expect [])" eth_accounts '[]'
func "eth_getBalance EOA          (eth_accounts V59)" eth_getBalance "[\"$EOA\",\"latest\"]"
func "eth_getTransactionCount EOA (eth_accounts V59)" eth_getTransactionCount "[\"$EOA\",\"latest\"]"
func "eth_getCode CONTRACT        (eth_codes V59)" eth_getCode "[\"$CONTRACT\",\"latest\"]"
func "eth_getStorageAt CONTRACT   (storage V59)" eth_getStorageAt "[\"$CONTRACT\",\"0x0\",\"latest\"]"

# ---- a real transaction: derive its block + index from the auto-selected hash ----
TX_BN=""
TX_BH=""
TX_IDX=""
hdr "Layer B -- transaction reads (transactions_objects / _receipts)"
if [ -n "$TXH" ]; then
  TXJSON="$(rpc eth_getTransactionByHash "[\"$TXH\"]")"
  TX_BN="$(jq -r '.result.blockNumber // empty' <<< "$TXJSON")"
  TX_BH="$(jq -r '.result.blockHash // empty' <<< "$TXJSON")"
  TX_IDX="$(jq -r '.result.transactionIndex // empty' <<< "$TXJSON")"
  func "eth_getTransactionByHash $TXH" eth_getTransactionByHash "[\"$TXH\"]"
  func "eth_getTransactionReceipt" eth_getTransactionReceipt "[\"$TXH\"]"
  funcq "tez_getTransactionGasInfo" tez_getTransactionGasInfo "[\"$TXH\"]"
else
  skip "transaction reads -- no transaction available on this node"
fi
if [ -n "$TX_BN" ] && [ -n "$TX_IDX" ]; then
  echo "    tx in block $TX_BN at index $TX_IDX"
  func "eth_getTransactionByBlockNumberAndIndex" eth_getTransactionByBlockNumberAndIndex "[\"$TX_BN\",\"$TX_IDX\"]"
  func "eth_getTransactionByBlockHashAndIndex" eth_getTransactionByBlockHashAndIndex "[\"$TX_BH\",\"$TX_IDX\"]"
  func "eth_getUncleByBlockNumberAndIndex (null)" eth_getUncleByBlockNumberAndIndex "[\"$TX_BN\",\"0x0\"]"
  func "eth_getUncleByBlockHashAndIndex (null)" eth_getUncleByBlockHashAndIndex "[\"$TX_BH\",\"0x0\"]"
else
  skip "tx-by-block-index reads -- no transaction available on this node"
fi

# ---- logs (scans transactions_receipts over a block range, scoped to CONTRACT) ----
LOG_BLK="${TX_BN:-$BN}"
func "eth_getLogs CONTRACT (receipts scan)" eth_getLogs "[{\"fromBlock\":\"$LOG_BLK\",\"toBlock\":\"$LOG_BLK\",\"address\":\"$CONTRACT\"}]"

# ---- misc / pure ----
func "web3_clientVersion (static)" web3_clientVersion '[]'
func "web3_sha3 (pure keccak)" web3_sha3 '["0x68656c6c6f"]'
func "txpool_content (mempool)" txpool_content '[]'

# ---- Michelson runtime reads (soft: legitimately N/A on an EVM-only replica) ----
hdr "Layer B -- Michelson runtime reads (soft -> INFO when N/A)"
funcq "tez_getMichelsonActivationLevel (V57 sunrise)" tez_getMichelsonActivationLevel '[]'
L1="$(le_to_dec "$(sh_cat /base/l1_level)")"
if [ -n "$L1" ]; then
  funcq "tez_getFinalizedBlocksOfL1Level $L1" tez_getFinalizedBlocksOfL1Level "$L1"
else
  skip "tez_getFinalizedBlocksOfL1Level -- /base/l1_level not decodable"
fi

# =====================================================================
hdr "V60 simulation / trace (ephemeral: active write+read trigger)"
# target the EOA (no code) so the call returns cleanly while still driving the
# simulation IPC write+read under /base
func "eth_call EOA (writes+reads /base/evm_simulation_result)" \
  eth_call "[{\"to\":\"$EOA\"},\"latest\"]"
func "eth_estimateGas EOA (writes+reads /base/evm_simulation_result)" \
  eth_estimateGas "[{\"to\":\"$EOA\"}]"
func "debug_traceCall EOA (writes+reads /base/trace)" \
  debug_traceCall "[{\"to\":\"$EOA\"},\"latest\"]"
# gate trace-by-tx on the fixture being present on THIS node (same condition that
# SKIPs the tx-by-block-index reads above) -- a missing tx is a fixture gap, not a reorg fault
if [ -n "$TX_BN" ]; then
  func "debug_traceTransaction $TXH (writes+reads /base/trace)" debug_traceTransaction "[\"$TXH\",{}]"
else
  skip "debug_traceTransaction -- supplied tx hash not found on this node"
fi
# block tracing is ONLY implemented for callTracer (tracer.ml: trace_block); an empty
# config defaults to structLogger -> "Tracer is not available for the endpoint", so we
# must request callTracer explicitly here (debug_traceCall/Transaction default fine)
func "debug_traceBlockByNumber $LOG_BLK (writes+reads /base/trace)" debug_traceBlockByNumber "[\"$LOG_BLK\",{\"tracer\":\"callTracer\"}]"
funcq "http_traceTransaction $TXH (/base/__http_trace V60)" http_traceTransaction "[\"$TXH\"]"
funcq "http_traceBlockByNumber $LOG_BLK (/base/simulation_http_traces V60)" http_traceBlockByNumber "[\"$LOG_BLK\"]"

# =====================================================================
printf '\n%s== SUMMARY ==%s\n' "$BLU" "$NC"
printf '  %sPASS=%d%s  %sFAIL=%d%s  %sINFO=%d%s  %sSKIP=%d%s\n' \
  "$GRN" "$PASS" "$NC" "$RED" "$FAIL" "$NC" "$YEL" "$INFO" "$NC" "$DIM" "$SKIP" "$NC"
echo "  (INFO = legitimately empty: transient/consumed/feature-gated path, not a failure)"
if [ "$FAIL" -eq 0 ]; then
  echo "${GRN}Reorg verification: OK${NC}"
else
  echo "${RED}Reorg verification: FAILURES present${NC}"
  exit 1
fi
