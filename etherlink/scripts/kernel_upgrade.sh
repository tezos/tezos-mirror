#!/usr/bin/env bash
# Submit (and optionally build / generate) an EVM kernel-upgrade payload to an
# Etherlink / TezosX network.
#
# Combines steps 1, 2, 3, 5 and 6 of the kernel-upgrade runbook:
#   1. (Optional, with --build-commit) Build the kernel WASM at a given commit
#   2. Generate the root hash of a kernel WASM (smart-rollup-installer)
#   3. Generate the upgrade payload      (octez-evm-node make upgrade payload)
#   5. Sign and submit the transfer to the admin contract (octez-client)
#   6. (Optional) Verify by polling tez_kernelVersion on the EVM RPC
#
# Step 4 (uploading new preimages to the smart-rollup-node) is deployment-
# specific (scp, kubectl cp, ...) and must be done out-of-band BEFORE
# broadcasting, otherwise the rollup will stall at activation. Use
# --preimages-dir to keep the preimages this script generates so you can
# upload them.

set -euo pipefail

usage() {
  cat << 'EOF'
Usage: kernel-upgrade.sh [options]

Payload source — choose exactly one:
  --build-commit COMMIT             Run git checkout + build-wasm.sh on COMMIT,
                                    then use the produced WASM. Implies --kernel.
                                    Also defaults --expected-version to COMMIT.
                                    WARNING: this checks out the commit in your
                                    working tree (commit/stash changes first).
  --kernel WASM                     Compute root hash from a WASM file, then payload.
                                    Requires --rollup and --activation.
  --root-hash HASH                  Use a precomputed root hash, generate payload.
                                    Requires --rollup and --activation.
  --payload VALUE                   Use a pre-built payload as-is.
  --payload-file PATH               Read a pre-built payload from a file.

Payload generation parameters (for --kernel / --root-hash):
  --rollup SR1                      Smart rollup address (sr1...) or alias.
  --activation TS                   Activation timestamp:
                                      - delay:    +10m, +1h, +30s
                                      - RFC3339:  2026-04-28T15:30:00Z
                                      - epoch s:  1761662400
  --preimages-dir DIR               Where to write preimages [default: temp dir].
                                    Set this to keep them for upload to the rollup node.

Deploy parameters (omit them all + pass --print-payload to skip deploy):
  -e, --endpoint URL                L1 RPC endpoint of the target network.
  -c, --admin-contract KT1          Admin contract address.
  -k, --admin-key VALUE             Admin secret key. One of:
                                      - alias known to octez-client
                                      - "encrypted:edesk1..."
                                      - "unencrypted:edsk..."
                                      - bare "edsk..." / "edesk1..." (scheme inferred)

Modes:
  --print-payload                   Print the generated payload and exit (no deploy).
  --dry-run                         Simulate the transfer (DEFAULT).
  --broadcast                       Actually broadcast the transaction.
  --verify-only                     Skip generation/deploy; just poll the EVM RPC
                                    until the kernel version matches.

Preimages push (step 4 of the runbook, optional):
  --push-preimages USER@HOST        Push preimages to a remote rollup node via
                                    `tar -cf - | ssh ... 'sudo -u <user> tar -xf'`.
                                    Effective only when --kernel or --build-commit
                                    actually produced preimages locally.
  --push-preimages-data-dir PATH    Remote rollup data dir containing wasm_2_0_0/
                                    [default: /opt/octez-evm-node/data_dir]
  --push-preimages-user USER        Remote user to extract as via sudo
                                    [default: tezos]

Verification (step 6 of the runbook):
  --evm-rpc URL                     EVM JSON-RPC endpoint (e.g. .../rpc).
  --expected-version VALUE          Expected `tez_kernelVersion` result
                                    (typically the new kernel's commit hash).
  --verify-timeout SECONDS          How long to wait for activation [default: 600].
  --verify-interval SECONDS         Poll interval [default: 15].
                                    If --evm-rpc AND --expected-version are set,
                                    verification runs automatically after --broadcast.

Other:
  --burn-cap N                      Burn cap for the transfer [default: 1].
  --client-dir PATH                 octez-client base dir [default: temp].
  --octez-client BIN                Path to octez-client.
  --octez-evm-node BIN              Path to octez-evm-node.
  --smart-rollup-installer BIN      Path to smart-rollup-installer.
  -h, --help                        Show this help.

Examples:

  # End-to-end from a freshly built kernel:
  etherlink/scripts/kernel-upgrade.sh \
      --kernel etherlink/kernels-$COMMIT/evm_kernel.wasm \
      --preimages-dir ./working-dir/wasm_2_0_0 \
      --rollup sr1LKpvu5AMnytKyiGgTs8ytMXe25JRnKgcY \
      --activation +10m \
      --endpoint https://rpc.tzkt.io/shadownet \
      --admin-contract KT1... \
      --admin-key parrot-dictator
      # ...then re-run with --broadcast.

  # Just produce the payload (e.g. to forward to a hardware signer):
  etherlink/scripts/kernel-upgrade.sh \
      --root-hash <ROOT_HASH> \
      --rollup sr1... \
      --activation +10m \
      --print-payload

  # End-to-end with auto-pushed preimages:
  etherlink-machines/kernel-upgrade.sh \
      --build-commit "$COMMIT_HASH" \
      --preimages-dir ./working-dir/wasm_2_0_0 \
      --rollup sr1LKpvu5AMnytKyiGgTs8ytMXe25JRnKgcY \
      --activation +15m \
      --endpoint https://rpc.tzkt.io/shadownet \
      --admin-contract KT1... \
      --admin-key DICTATOR \
      --push-preimages nomadic@parrot-sequencer-1.nomadic-labs.eu \
      --evm-rpc https://parrot-observer-1.nomadic-labs.eu \
      --broadcast

  # Deploy a pre-built payload, then auto-verify:
  etherlink/scripts/kernel-upgrade.sh \
      --payload-file payload.txt \
      --endpoint https://rpc.tzkt.io/shadownet \
      --admin-contract KT1... \
      --admin-key alias \
      --evm-rpc https://parrot-observer-1.nomadic-labs.eu \
      --expected-version <COMMIT_HASH> \
      --broadcast

  # Verify only (e.g. after a manual broadcast):
  etherlink/scripts/kernel-upgrade.sh \
      --verify-only \
      --evm-rpc https://parrot-observer-1.nomadic-labs.eu \
      --expected-version <COMMIT_HASH>

Reminder: upload preimages from --preimages-dir to the rollup node
(<rollup_data>/wasm_2_0_0/) BEFORE broadcasting, or activation will stall.
EOF
}

err() {
  echo "error: $*" >&2
  exit 2
}
log() { echo ">>> $*" >&2; }

# Poll the EVM JSON-RPC `tez_kernelVersion` until it matches $expected, or fail.
verify_kernel_version() {
  local rpc="$1" expected="$2" timeout="$3" interval="$4"
  command -v curl > /dev/null || err "curl is required for verification"

  local deadline now raw got
  deadline=$(($(date +%s) + timeout))
  log "verifying $rpc -> tez_kernelVersion == '$expected' (timeout ${timeout}s)"

  while true; do
    raw=""
    raw="$(curl --silent --max-time 10 -X POST \
      -H 'Content-Type: application/json' \
      --data '{"jsonrpc":"2.0","method":"tez_kernelVersion","params":[],"id":1}' \
      "$rpc" 2> /dev/null || true)"
    # Extract "result":"..." (commit hash). No jq dependency.
    got="$(printf '%s' "$raw" | sed -nE 's/.*"result":"([^"]*)".*/\1/p' | head -n1)"

    if [[ -n "$got" && "$got" == "$expected" ]]; then
      log "kernel version matches: $got"
      return 0
    fi

    now=$(date +%s)
    if ((now >= deadline)); then
      err "timeout: kernel version is '${got:-<no response>}' (expected '$expected')"
    fi
    log "current='${got:-<no response>}' expected='$expected', retrying in ${interval}s..."
    sleep "$interval"
  done
}

BUILD_COMMIT=""
KERNEL=""
ROOT_HASH=""
PAYLOAD=""
PAYLOAD_FILE=""
ROLLUP=""
ACTIVATION=""
PREIMAGES_DIR=""
ENDPOINT=""
ADMIN_CONTRACT=""
ADMIN_KEY=""
PRINT_PAYLOAD="false"
DRY_RUN="true"
VERIFY_ONLY="false"
PUSH_PREIMAGES=""
PUSH_PREIMAGES_DATA_DIR="/opt/octez-evm-node/data_dir"
PUSH_PREIMAGES_USER="tezos"
EVM_RPC=""
EXPECTED_VERSION=""
VERIFY_TIMEOUT="600"
VERIFY_INTERVAL="15"
BURN_CAP="1"
CLIENT_DIR=""
OCTEZ_CLIENT_BIN="${OCTEZ_CLIENT:-octez-client}"
OCTEZ_EVM_NODE_BIN="${OCTEZ_EVM_NODE:-octez-evm-node}"
SMART_ROLLUP_INSTALLER_BIN="${SMART_ROLLUP_INSTALLER:-smart-rollup-installer}"

while [[ $# -gt 0 ]]; do
  case "$1" in
  --build-commit)
    BUILD_COMMIT="$2"
    shift 2
    ;;
  --kernel)
    KERNEL="$2"
    shift 2
    ;;
  --root-hash)
    ROOT_HASH="$2"
    shift 2
    ;;
  --payload)
    PAYLOAD="$2"
    shift 2
    ;;
  --payload-file)
    PAYLOAD_FILE="$2"
    shift 2
    ;;
  --rollup)
    ROLLUP="$2"
    shift 2
    ;;
  --activation)
    ACTIVATION="$2"
    shift 2
    ;;
  --preimages-dir)
    PREIMAGES_DIR="$2"
    shift 2
    ;;
  -e | --endpoint)
    ENDPOINT="$2"
    shift 2
    ;;
  -c | --admin-contract)
    ADMIN_CONTRACT="$2"
    shift 2
    ;;
  -k | --admin-key)
    ADMIN_KEY="$2"
    shift 2
    ;;
  --print-payload)
    PRINT_PAYLOAD="true"
    shift
    ;;
  --dry-run)
    DRY_RUN="true"
    shift
    ;;
  --broadcast)
    DRY_RUN="false"
    shift
    ;;
  --verify-only)
    VERIFY_ONLY="true"
    shift
    ;;
  --evm-rpc)
    EVM_RPC="$2"
    shift 2
    ;;
  --expected-version)
    EXPECTED_VERSION="$2"
    shift 2
    ;;
  --verify-timeout)
    VERIFY_TIMEOUT="$2"
    shift 2
    ;;
  --verify-interval)
    VERIFY_INTERVAL="$2"
    shift 2
    ;;
  --push-preimages)
    PUSH_PREIMAGES="$2"
    shift 2
    ;;
  --push-preimages-data-dir)
    PUSH_PREIMAGES_DATA_DIR="$2"
    shift 2
    ;;
  --push-preimages-user)
    PUSH_PREIMAGES_USER="$2"
    shift 2
    ;;
  --burn-cap)
    BURN_CAP="$2"
    shift 2
    ;;
  --client-dir)
    CLIENT_DIR="$2"
    shift 2
    ;;
  --octez-client)
    OCTEZ_CLIENT_BIN="$2"
    shift 2
    ;;
  --octez-evm-node)
    OCTEZ_EVM_NODE_BIN="$2"
    shift 2
    ;;
  --smart-rollup-installer)
    SMART_ROLLUP_INSTALLER_BIN="$2"
    shift 2
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  *)
    echo "error: unknown argument: $1" >&2
    usage >&2
    exit 2
    ;;
  esac
done

# --build-commit implicitly sets --expected-version (the kernel's identity is
# the commit hash). Only apply this default when --evm-rpc is also set, so a
# dry-run without verification doesn't trigger the verify-args sanity check.
if [[ -n "$BUILD_COMMIT" && -n "$EVM_RPC" && -z "$EXPECTED_VERSION" ]]; then
  EXPECTED_VERSION="$BUILD_COMMIT"
fi

# --verify-only short-circuits everything else
if [[ "$VERIFY_ONLY" == "true" ]]; then
  [[ -n "$EVM_RPC" ]] || err "--verify-only requires --evm-rpc"
  [[ -n "$EXPECTED_VERSION" ]] || err "--verify-only requires --expected-version"
  verify_kernel_version "$EVM_RPC" "$EXPECTED_VERSION" "$VERIFY_TIMEOUT" "$VERIFY_INTERVAL"
  exit 0
fi

# Verification args sanity: both or neither
if [[ -n "$EVM_RPC" && -z "$EXPECTED_VERSION" ]] || [[ -z "$EVM_RPC" && -n "$EXPECTED_VERSION" ]]; then
  err "--evm-rpc and --expected-version must be used together"
fi

# Exactly one payload source
sources=0
[[ -n "$BUILD_COMMIT" ]] && sources=$((sources + 1))
[[ -n "$KERNEL" ]] && sources=$((sources + 1))
[[ -n "$ROOT_HASH" ]] && sources=$((sources + 1))
[[ -n "$PAYLOAD" ]] && sources=$((sources + 1))
[[ -n "$PAYLOAD_FILE" ]] && sources=$((sources + 1))
if [[ "$sources" -ne 1 ]]; then
  err "specify exactly one of: --build-commit / --kernel / --root-hash / --payload / --payload-file"
fi

# Step 1 (optional): build WASM at a given commit.
if [[ -n "$BUILD_COMMIT" ]]; then
  command -v git > /dev/null || err "git is required for --build-commit"
  repo_root="$(git rev-parse --show-toplevel 2> /dev/null)" ||
    err "--build-commit must be run from inside the tezos git repo"
  [[ -x "$repo_root/etherlink/scripts/build-wasm.sh" ]] ||
    err "$repo_root/etherlink/scripts/build-wasm.sh not found or not executable"

  log "checking out $BUILD_COMMIT"
  (cd "$repo_root" && git checkout "$BUILD_COMMIT") >&2

  log "building kernel at $BUILD_COMMIT (this can take a while)"
  (cd "$repo_root" && ./etherlink/scripts/build-wasm.sh) >&2

  KERNEL="$repo_root/etherlink/kernels-$BUILD_COMMIT/evm_kernel.wasm"
  [[ -r "$KERNEL" ]] || err "build did not produce expected kernel: $KERNEL"
  log "built kernel: $KERNEL"
fi

# Generation requires rollup + activation
if [[ -n "$KERNEL" || -n "$ROOT_HASH" ]]; then
  [[ -n "$ROLLUP" ]] || err "--rollup is required when generating a payload"
  [[ -n "$ACTIVATION" ]] || err "--activation is required when generating a payload"
fi

# Step 2: kernel WASM -> root hash (+ preimages)
if [[ -n "$KERNEL" ]]; then
  [[ -r "$KERNEL" ]] || err "cannot read kernel: $KERNEL"
  command -v "$SMART_ROLLUP_INSTALLER_BIN" > /dev/null ||
    err "smart-rollup-installer not found: $SMART_ROLLUP_INSTALLER_BIN"

  if [[ -z "$PREIMAGES_DIR" ]]; then
    PREIMAGES_DIR="$(mktemp -d -t kernel-upgrade-preimages-XXXXXX)"
    log "generating preimages in $PREIMAGES_DIR (use --preimages-dir to keep them)"
  else
    mkdir -p "$PREIMAGES_DIR"
    log "generating preimages in $PREIMAGES_DIR"
  fi

  raw="$("$SMART_ROLLUP_INSTALLER_BIN" get-reveal-installer \
    --upgrade-to "$KERNEL" \
    --output /dev/null \
    --preimages-dir "$PREIMAGES_DIR" \
    --display-root-hash)"
  ROOT_HASH="$(printf '%s\n' "$raw" | sed -n 's/^ROOT_HASH:[[:space:]]*//p' | head -n1)"
  [[ -n "$ROOT_HASH" ]] || err "could not parse ROOT_HASH from smart-rollup-installer output: $raw"
  log "ROOT_HASH=$ROOT_HASH"
fi

# Step 3: root hash -> payload
if [[ -n "$ROOT_HASH" ]]; then
  command -v "$OCTEZ_EVM_NODE_BIN" > /dev/null ||
    err "octez-evm-node not found: $OCTEZ_EVM_NODE_BIN"

  payload_args=(
    make upgrade payload
    with root hash "$ROOT_HASH"
    at activation timestamp "$ACTIVATION"
  )

  # `octez-evm-node make upgrade payload --rollup` looks the value up as a
  # WALLET ALIAS (not as an sr1 address). When the caller passes a raw sr1...
  # address, register it on the fly in a temp wallet and point octez-evm-node
  # at that wallet via --wallet-dir.
  case "$ROLLUP" in
  sr1*)
    command -v "$OCTEZ_CLIENT_BIN" > /dev/null ||
      err "octez-client is required to register the rollup alias for octez-evm-node"
    if [[ -z "$CLIENT_DIR" ]]; then
      CLIENT_DIR="$(mktemp -d -t kernel-upgrade-client-XXXXXX)"
      trap 'rm -rf "$CLIENT_DIR"' EXIT
    fi
    rollup_alias="kernel-upgrade-rollup-$$"
    log "registering rollup $ROLLUP as alias '$rollup_alias' in $CLIENT_DIR"
    "$OCTEZ_CLIENT_BIN" --base-dir "$CLIENT_DIR" \
      remember smart rollup "$rollup_alias" "$ROLLUP" --force > /dev/null
    payload_args+=(--rollup "$rollup_alias" --wallet-dir "$CLIENT_DIR")
    ;;
  *)
    payload_args+=(--rollup "$ROLLUP")
    ;;
  esac

  log "generating upgrade payload"
  raw="$("$OCTEZ_EVM_NODE_BIN" "${payload_args[@]}")"
  PAYLOAD="$(printf '%s\n' "$raw" | awk 'NF{p=$0} END{print p}')"
  [[ -n "$PAYLOAD" ]] || err "could not parse payload from octez-evm-node output: $raw"

  # Normalize the output for octez-client --arg:
  # 1. octez-evm-node wraps the value in single quotes for shell-paste use,
  #    e.g. 'Pair "sr1..." eba1...'. Strip them.
  PAYLOAD="${PAYLOAD#\'}"
  PAYLOAD="${PAYLOAD%\'}"
  # 2. The trailing bytes are emitted without the `0x` prefix that Michelson
  #    requires for a bytes literal — add it if missing.
  case "$PAYLOAD" in
  *\"\ 0x*) ;;
  *\"\ *) PAYLOAD="$(printf '%s' "$PAYLOAD" | sed -E 's/" ([0-9a-fA-F]+)$/" 0x\1/')" ;;
  esac
  log "PAYLOAD=$PAYLOAD"
fi

# Load payload from file if requested
if [[ -n "$PAYLOAD_FILE" ]]; then
  [[ -r "$PAYLOAD_FILE" ]] || err "cannot read payload file: $PAYLOAD_FILE"
  PAYLOAD="$(< "$PAYLOAD_FILE")"
fi

[[ -n "$PAYLOAD" ]] || err "internal error: no payload to deploy"

# Print-and-exit
if [[ "$PRINT_PAYLOAD" == "true" ]]; then
  printf '%s\n' "$PAYLOAD"
  exit 0
fi

# Step 4 (runbook): push preimages to the remote rollup node, if requested.
# Only on --broadcast: dry-run is a local check, no need to touch the rollup.
if [[ -n "$PUSH_PREIMAGES" ]]; then
  if [[ "$DRY_RUN" == "true" ]]; then
    log "skipping preimages push (--dry-run)"
  else
    [[ -n "$PREIMAGES_DIR" && -d "$PREIMAGES_DIR" ]] ||
      err "--push-preimages requires preimages to have been generated locally (use --kernel or --build-commit, not --root-hash / --payload)"
    command -v ssh > /dev/null || err "ssh is required for --push-preimages"
    command -v tar > /dev/null || err "tar is required for --push-preimages"

    log "pushing preimages from $PREIMAGES_DIR to $PUSH_PREIMAGES:$PUSH_PREIMAGES_DATA_DIR/wasm_2_0_0/ (extract as $PUSH_PREIMAGES_USER)"
    # We deliberately want client-side expansion of $PUSH_PREIMAGES_USER and
    # $PUSH_PREIMAGES_DATA_DIR — they are local script variables, not remote.
    # shellcheck disable=SC2029
    (cd "$PREIMAGES_DIR" && tar -cf - .) |
      ssh "$PUSH_PREIMAGES" \
        "sudo -u $PUSH_PREIMAGES_USER tar -C $PUSH_PREIMAGES_DATA_DIR/wasm_2_0_0 -xf -"
    log "preimages push completed"
  fi
fi

# Step 5: deploy
[[ -n "$ENDPOINT" ]] || err "missing --endpoint (or pass --print-payload to skip deploy)"
[[ -n "$ADMIN_CONTRACT" ]] || err "missing --admin-contract"
[[ -n "$ADMIN_KEY" ]] || err "missing --admin-key"

command -v "$OCTEZ_CLIENT_BIN" > /dev/null ||
  err "octez-client not found: $OCTEZ_CLIENT_BIN"

# Normalize bare key formats to encrypted:/unencrypted:.
case "$ADMIN_KEY" in
edesk*) ADMIN_KEY="encrypted:$ADMIN_KEY" ;;
edsk* | spsk* | p2sk*) ADMIN_KEY="unencrypted:$ADMIN_KEY" ;;
esac

# If the admin key is a literal SK we import it into a temp wallet. If it's an
# alias, we use the user's default wallet (~/.tezos-client) so existing aliases
# like DICTATOR resolve.
deploy_base_dir=()
case "$ADMIN_KEY" in
encrypted:* | unencrypted:*)
  if [[ -z "$CLIENT_DIR" ]]; then
    CLIENT_DIR="$(mktemp -d -t kernel-upgrade-client-XXXXXX)"
    trap 'rm -rf "$CLIENT_DIR"' EXIT
  fi
  deploy_base_dir=(--base-dir "$CLIENT_DIR")
  ADMIN_ALIAS="kernel-upgrade-admin-$$"
  log "importing admin key as alias '$ADMIN_ALIAS' in $CLIENT_DIR"
  "$OCTEZ_CLIENT_BIN" "${deploy_base_dir[@]}" --endpoint "$ENDPOINT" \
    import secret key "$ADMIN_ALIAS" "$ADMIN_KEY" --force > /dev/null
  wallet_descr="$CLIENT_DIR (temp)"
  ;;
*)
  ADMIN_ALIAS="$ADMIN_KEY"
  wallet_descr="${TEZOS_CLIENT_DIR:-$HOME/.tezos-client} (default)"
  ;;
esac

cat >&2 << EOF

===== Kernel upgrade =====
Endpoint:        $ENDPOINT
Admin contract:  $ADMIN_CONTRACT
Admin alias:     $ADMIN_ALIAS
Wallet:          $wallet_descr
Mode:            $([ "$DRY_RUN" = "true" ] && echo 'DRY RUN (use --broadcast to submit)' || echo 'BROADCAST')

EOF

# transfer 0 from <admin> to <KT1> --arg <payload>. octez-client accepts the KT1
# directly for `to` — no need to remember it as a contract alias first.
transfer_args=(
  ${deploy_base_dir[@]+"${deploy_base_dir[@]}"}
  --endpoint "$ENDPOINT"
  transfer 0
  from "$ADMIN_ALIAS"
  to "$ADMIN_CONTRACT"
  --arg "$PAYLOAD"
  --burn-cap "$BURN_CAP"
)
if [[ "$DRY_RUN" == "true" ]]; then
  transfer_args+=(--dry-run)
fi

"$OCTEZ_CLIENT_BIN" "${transfer_args[@]}"

if [[ "$DRY_RUN" == "true" ]]; then
  echo >&2
  echo "Dry-run successful. Re-run with --broadcast to actually submit." >&2
  exit 0
fi

# Step 6: optional verification after a real broadcast.
if [[ -n "$EVM_RPC" && -n "$EXPECTED_VERSION" ]]; then
  echo >&2
  verify_kernel_version "$EVM_RPC" "$EXPECTED_VERSION" "$VERIFY_TIMEOUT" "$VERIFY_INTERVAL"
fi
