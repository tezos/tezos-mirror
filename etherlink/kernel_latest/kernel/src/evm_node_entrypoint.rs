// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! EVM Node Entrypoints.
//!
//! The module contain functions that may be called by the evm node
//! only. It allows to call specific functions of the kernel without
//! using the inbox and a specific message.

use crate::{
    apply::{
        ExecutionResult, RuntimeExecutionInfo, TezosExecutionInfo,
        WITHDRAWAL_OUTBOX_QUEUE,
    },
    block::bip_from_blueprint,
    blueprint::Blueprint,
    blueprint_storage::read_current_blueprint_header,
    chains::{self, TezosXChainConfig},
    configuration::{fetch_common_config, fetch_tezosx_configuration},
    delayed_inbox::DelayedInbox,
    journal::{prepare_tezosx_journal, TezosXHashes},
    sub_block,
    transaction::Transaction,
};
use mir::ast::{Entrypoint, IntoMicheline, Type};
use mir::gas::Gas;
use mir::parser::Parser;
use primitive_types::{H160, H256, U256};
use rlp::{Rlp, RlpStream};
use std::collections::HashMap;
use tezos_crypto_rs::hash::OperationHash;
use tezos_crypto_rs::public_key_hash::PublicKeyHash;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_ethereum::{
    rlp_helpers::{
        append_option_canonical, decode_field, decode_field_bool, decode_tx_hash, next,
        FromRlpBytes,
    },
    transaction::TransactionHash,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::KernelHost;
use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::outbox::OutboxQueue;
use tezos_smart_rollup_host::storage::{CoreStorage, StorageV1};
use tezos_smart_rollup_host::wasm::WasmHost;
use tezos_smart_rollup_keyspace::{Key, KeySpace};

#[cfg(target_arch = "wasm32")]
use tezos_smart_rollup_core::rollup_host::RollupHost;

// Keys inside the `/base` keyspace. Each resolves to the durable path formed by
// prefixing `/base` to the relative key below.
const DELAYED_INPUT_KEY: Key = Key::from_static(b"/__delayed_input");

const TEZOSX_SIMULATION_INPUT_KEY: Key = Key::from_static(b"/__simulation/input");
const TEZOSX_SIMULATION_RESULT_KEY: Key = Key::from_static(b"/__simulation/result");

pub(crate) const TEZOSX_ENTRYPOINTS_INPUT_KEY: Key =
    Key::from_static(b"/tezosx_entrypoints/input");
pub(crate) const TEZOSX_ENTRYPOINTS_RESULT_KEY: Key =
    Key::from_static(b"/tezosx_entrypoints/result");

pub(crate) const TEZOSX_RUN_CODE_INPUT_KEY: Key =
    Key::from_static(b"/tezosx_run_code/input");
pub(crate) const TEZOSX_RUN_CODE_RESULT_KEY: Key =
    Key::from_static(b"/tezosx_run_code/result");

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn populate_delayed_inbox() {
    let mut sdk_host = unsafe { RollupHost::new() };
    populate_delayed_inbox_with_durable_storage(&mut sdk_host);
}

#[allow(dead_code)]
pub fn populate_delayed_inbox_with_durable_storage<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let payload = rk.base().get(&DELAYED_INPUT_KEY).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap().into();
    let mut delayed_inbox = DelayedInbox::from_base(rk.base()).unwrap();
    let common = fetch_common_config(&mut rk);
    delayed_inbox
        .save_transaction(&mut rk, transaction, 0.into(), 0u32, &common)
        .unwrap();
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn drop_delayed_transaction() {
    let mut sdk_host = unsafe { RollupHost::new() };
    drop_delayed_transaction_with_durable_storage(&mut sdk_host);
}

#[allow(dead_code)]
pub fn drop_delayed_transaction_with_durable_storage<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let payload = rk.base().get(&DELAYED_INPUT_KEY).unwrap();
    let transaction_hash: TransactionHash = decode_tx_hash(Rlp::new(&payload)).unwrap();
    let mut delayed_inbox = DelayedInbox::from_base(rk.base()).unwrap();
    delayed_inbox
        .delete(rk.base_mut(), crate::delayed_inbox::Hash(transaction_hash))
        .unwrap();
}
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn single_tx_execution() {
    let mut sdk_host = unsafe { RollupHost::new() };
    single_tx_execution_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn single_tx_execution_fn<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let tx_input = match sub_block::read_single_tx_execution_input(rk.base_mut()) {
        Ok(Some(input)) => input,
        Ok(None) => {
            log!(
                Error,
                "No single transaction execution input found in storage"
            );
            return;
        }
        Err(err) => {
            log!(
                Error,
                "Error while reading single transaction execution input: {:?}",
                err
            );
            return;
        }
    };
    match sub_block::handle_run_transaction(&mut rk, tx_input) {
        Ok(()) => (),
        Err(err) => {
            log!(
                Error,
                "Error during single transaction execution: {:?}",
                err
            );
        }
    }
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn assemble_block() {
    let mut sdk_host = unsafe { RollupHost::new() };
    assemble_block_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn assemble_block_fn<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage + WasmHost,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let assemble_block_input = match sub_block::read_assemble_block_input(rk.base_mut()) {
        Ok(Some(input)) => input,
        Ok(None) => {
            log!(Error, "No assemble block input found in storage");
            return;
        }
        Err(err) => {
            log!(Error, "Error while reading assemble block input: {:?}", err);
            return;
        }
    };
    match sub_block::assemble_block(&mut rk, assemble_block_input) {
        Ok(()) => (),
        Err(err) => {
            log!(Error, "Error while assembling block: {:?}", err);
        }
    }
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn tezosx_simulate() {
    let mut sdk_host = unsafe { RollupHost::new() };
    tezosx_simulate_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn tezosx_simulate_fn<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let input = match rk.base().get(&TEZOSX_SIMULATION_INPUT_KEY) {
        Some(bytes) => bytes,
        None => {
            log!(Error, "Tezos X simulation input not found");
            return;
        }
    };

    // Input is RLP-encoded as a list: [skip_signature_check, transaction_bytes].
    // Fees checks are always skipped during simulation.

    if input.is_empty() {
        log!(Error, "Tezos X simulation: empty input");
        return;
    }

    let rlp = Rlp::new(&input);
    let mut it = rlp.iter();
    let skip_signature_check: bool =
        match next(&mut it).and_then(|f| decode_field_bool(&f, "skip_signature_check")) {
            Ok(v) => v,
            Err(err) => {
                log!(
                    Error,
                    "Tezos X simulation: failed to decode input: {:?}",
                    err
                );
                return;
            }
        };
    let transaction_bytes: Vec<u8> =
        match next(&mut it).and_then(|f| decode_field(&f, "transaction_bytes")) {
            Ok(v) => v,
            Err(err) => {
                log!(
                    Error,
                    "Tezos X simulation: failed to decode input: {:?}",
                    err
                );
                return;
            }
        };

    log!(
        Debug,
        "Tezos X simulation starts, skip signature flag: {skip_signature_check:?}, \
         input length: {:?}",
        transaction_bytes.len()
    );

    let chain_config = fetch_tezosx_configuration(&mut rk);
    let blueprint_header = match read_current_blueprint_header(rk.base()) {
        Ok(h) => h,
        Err(err) => {
            log!(
                Error,
                "Tezos X simulation: failed to read blueprint header: {:?}",
                err
            );
            return;
        }
    };
    let registry = chain_config.init_registry();

    // Parse the transaction bytes as a Tezos X transaction (blueprint
    // version 1 format: tag byte + raw bytes).
    let transaction =
        match TezosXChainConfig::transaction_from_bytes(&transaction_bytes, 1) {
            Ok(tx) => tx,
            Err(err) => {
                log!(
                    Error,
                    "Tezos X simulation: failed to parse transaction: {:?}",
                    err
                );
                return;
            }
        };

    let block_in_progress = bip_from_blueprint(
        rk.host(),
        &chain_config,
        blueprint_header.number,
        H256::zero(),
        H256::zero(),
        Blueprint {
            transactions: vec![],
            timestamp: blueprint_header.timestamp,
        },
    );

    let block_constants = match chain_config.constants(
        rk.host_mut(),
        &block_in_progress,
        U256::zero(),
        H160::zero(),
    ) {
        Ok(c) => c,
        Err(err) => {
            log!(
                Error,
                "Tezos X simulation: failed to build block constants: {:?}",
                err
            );
            return;
        }
    };

    let outbox_queue = OutboxQueue::new(&WITHDRAWAL_OUTBOX_QUEUE, u32::MAX)
        .expect("WITHDRAWAL_OUTBOX_QUEUE is a valid path");

    // This "tezosx_simulate_fn" function is actually called in two
    // cases: actual simulation (the user has not signed the operation
    // yet, and does not yet know what gas limit and fees are
    // required) and pre-application (the user has signed the
    // operation, all the fields are set, the user wants a detailed
    // receipt before publishing the operation.  We use the
    // `skip_signature_check` boolean from the input to tell these two
    // cases apart; when it is set we are in simulation mode and there
    // is no reason to check minimal fees (they are probably 0
    // anyway), when it is unset we are in preapplication mode and we
    // want to be as close as possible to what will happen during
    // application.
    let skip_fees_check = skip_signature_check;

    // Only the Michelson arm needs an externally-owned journal: it calls
    // `apply_tezos_operation` directly so the HTTP traces of the crossings it
    // dispatches can be captured. The Ethereum arm goes through
    // `apply_transaction`, which builds and owns its own journal, so it has
    // neither a journal to seed here nor traces to collect — hence the empty
    // trace list it returns below.
    let (execution_result, traces) = match transaction {
        chains::TezosXTransaction::Tezos(operation) => {
            // Seed the journal's origination nonce exactly as
            // `TezosXChainConfig::apply_tezos_operation` does, so pre-applied
            // KT1s match the ones application produces: a Michelson operation
            // uses its real hash, a deposit its blueprint-supplied `tx_hash`.
            // `apply_tezos_operation` also reads this back as the receipt's
            // operation hash.
            let michelson = match &operation.content {
                chains::TezlinkContent::Tezos(inner) => match inner.hash() {
                    Ok(hash) => hash,
                    Err(err) => {
                        log!(
                            Error,
                            "Tezos X simulation: failed to hash operation: {:?}",
                            err
                        );
                        return;
                    }
                },
                chains::TezlinkContent::Deposit(_) => {
                    OperationHash::from(operation.tx_hash)
                }
            };
            // Built through `prepare_tezosx_journal` like every applied
            // journal, so this path cannot drift from block production on the
            // setup it shares. The live simulation block goes in, not
            // `BlockConstants::dummy()`, so a CRAC dispatched here observes
            // `BASEFEE`, `GASLIMIT`, ... as execution would. The HTTP-trace
            // flag is read on the base host, before any `SafeStorage`
            // wrapping, exactly as the applied path does. The simulation block
            // carries no prior operation, hence an internal-operation base of
            // 0, and no tracer: this entrypoint reports HTTP traces, not EVM
            // call traces.
            let mut trace_journal = prepare_tezosx_journal(
                tezosx_journal::CracId::new(0, 0),
                &TezosXHashes::from_michelson_operation(michelson),
                &block_constants.evm_runtime_block_constants,
                crate::storage::is_http_trace_enabled(rk.base()),
                &chain_config.debug_features,
                0,
                None,
            );
            let enable_gas_refund = chain_config
                .experimental_features
                .is_michelson_gas_refund_enabled();
            let execution_result = chains::apply_tezos_operation(
                chain_config.michelson_chain_id(),
                &block_in_progress,
                &mut rk,
                &registry,
                &block_constants.michelson_runtime_block_constants,
                operation,
                None,
                skip_signature_check,
                skip_fees_check,
                // Simulation / pre-application: do not enforce branch liveness
                // (lenient, like skip_signature_check; matches L1 run_operation).
                true,
                Some(&outbox_queue),
                Some(&block_constants.evm_runtime_block_constants),
                &mut trace_journal,
                enable_gas_refund,
                // Off the block-production hot path (simulation / pre-application):
                // conservatively preserve the prior always-promote behavior. The
                // per-op store_has probe this patch removes is only taken on the
                // block-production path.
                true,
                true,
                false, // da fees are disabled in simulation
            )
            .map(|x| match x {
                ExecutionResult::Valid(res) => {
                    ExecutionResult::Valid(RuntimeExecutionInfo::Tezos(res))
                }
                ExecutionResult::Invalid => ExecutionResult::Invalid,
            });
            (execution_result, trace_journal.into_http_traces())
        }
        _ => (
            chain_config.apply_transaction(
                &block_in_progress,
                &mut rk,
                &registry,
                &outbox_queue,
                &block_constants,
                transaction,
                0,
                None,
                None,
                skip_signature_check,
                skip_fees_check,
                // This is the [tezosx_simulate] entrypoint, not the replay
                // path: simulation writes its own aggregate traces via
                // [store_simulation_http_traces], so the per-tx [http_trace*]
                // capture is not wanted here.
                false,
            ),
            Vec::new(),
        ),
    };

    let execution_result = match execution_result {
        Ok(result) => result,
        Err(err) => {
            log!(
                Error,
                "Tezos X simulation: operation execution failed: {:?}",
                err
            );
            return;
        }
    };

    // Store captured HTTP traces.
    if let Err(err) = crate::storage::store_simulation_http_traces(rk.base_mut(), &traces)
    {
        log!(
            Error,
            "Tezos X simulation: failed to store HTTP traces: {:?}",
            err
        );
    }

    let applied_operation = match execution_result {
        ExecutionResult::Valid(RuntimeExecutionInfo::Tezos(TezosExecutionInfo {
            op,
            cross_runtime_effects: _,
            consumed_milligas: _,
        })) => op,
        ExecutionResult::Valid(RuntimeExecutionInfo::Ethereum(_)) => {
            log!(
                Error,
                "Tezos X simulation: unexpected Ethereum execution result"
            );
            return;
        }
        ExecutionResult::Invalid => {
            log!(Error, "Tezos X simulation: operation was invalid");
            return;
        }
    };

    log!(
        Debug,
        "Tezos X simulation finished, result: {:?}",
        applied_operation
    );
    let op_bytes = match applied_operation.to_bytes() {
        Ok(b) => b,
        Err(err) => {
            log!(
                Error,
                "Tezos X simulation: failed to serialize result: {:?}",
                err
            );
            return;
        }
    };
    // Result is RLP-encoded as a value containing the serialized operation.
    let mut stream = RlpStream::new();
    stream.append(&op_bytes);
    if let Err(err) = rk
        .base_mut()
        .set(&TEZOSX_SIMULATION_RESULT_KEY, stream.out())
    {
        log!(Error, "Error writing Tezos X simulation result: {:?}", err);
    }
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn tezosx_michelson_entrypoints() {
    let mut sdk_host = unsafe { RollupHost::new() };
    tezosx_michelson_entrypoints_entry(&mut sdk_host);
}

#[allow(dead_code)]
pub fn tezosx_michelson_entrypoints_entry<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    tezosx_michelson_entrypoints_fn(&mut rk);
}

/// Takes the handle rather than the host: the caller owns the single `/base`
/// load, so no second one is nested inside this query.
#[allow(dead_code)]
pub fn tezosx_michelson_entrypoints_fn<Host, R, KS>(
    rk: &mut RuntimeKeyspaces<KernelHost<R, Host>, KS>,
) where
    R: StorageV1,
    Host: std::borrow::BorrowMut<R> + std::borrow::Borrow<R>,
    KS: KeySpace,
{
    let input = match rk.base().get(&TEZOSX_ENTRYPOINTS_INPUT_KEY) {
        Some(bytes) => bytes,
        None => {
            log!(Error, "Tezos X entrypoints input not found");
            return;
        }
    };
    handle_query_entrypoints_to(rk, &input, &TEZOSX_ENTRYPOINTS_RESULT_KEY);
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn tezosx_run_code() {
    let mut sdk_host = unsafe { RollupHost::new() };
    tezosx_run_code_fn(&mut sdk_host);
}

/// Input of the `tezosx_run_code` entrypoint. Mirrors the node's
/// `Run_code.input_encoding` (`tezos_backend.ml`) field for field; the
/// unit tests below pin the wire bytes. See
/// [`tezos_execution::RunCodeParams`] for the fields' semantics.
#[derive(NomReader)]
#[cfg_attr(test, derive(Debug, PartialEq, BinWriter))]
struct RunCodeInput {
    #[encoding(dynamic, bytes)]
    script: Vec<u8>,
    #[encoding(dynamic, bytes)]
    storage: Vec<u8>,
    #[encoding(dynamic, bytes)]
    input: Vec<u8>,
    entrypoint: String,
    chain_id: tezos_crypto_rs::hash::ChainId,
    /// Must be a KT1.
    self_address: Contract,
    /// Mutez; must be non-negative.
    amount: i64,
    sender: Option<Contract>,
    payer: Option<PublicKeyHash>,
    /// Mutez; must be non-negative.
    balance: Option<i64>,
    /// Gas limit in *gas units* (as on L1), not milligas; must be
    /// non-negative.
    gas: Option<i64>,
    now: Option<i64>,
    level: Option<u32>,
}

/// Result of `tezosx_run_code`, decoded by the node's
/// `Run_code.result_encoding`. The variant order is the union tag; an
/// unknown tag fails the node's decoder outright, never a misclassified
/// script failure.
#[derive(BinWriter)]
enum RunCodeResult {
    /// The resulting storage, Micheline-encoded (optimized-legacy).
    Success(RunCodeStorage),
    /// Ill-typed input or failed execution. Deterministic: do not retry.
    ExecutionError(String),
    /// Durable-storage / host failure. Transient: may retry.
    HostError(String),
}

/// A named field so it can take `#[encoding(dynamic, bytes)]`: on a
/// tuple variant the derive accepts the attribute but ignores it, and
/// the length prefix the node expects disappears.
#[derive(BinWriter)]
struct RunCodeStorage {
    #[encoding(dynamic, bytes)]
    storage: Vec<u8>,
}

impl From<Result<tezos_execution::RunCodeOutput, tezos_execution::RunCodeError>>
    for RunCodeResult
{
    fn from(
        result: Result<tezos_execution::RunCodeOutput, tezos_execution::RunCodeError>,
    ) -> Self {
        use tezos_execution::RunCodeError;
        match result {
            Ok(output) => Self::Success(RunCodeStorage {
                storage: output.storage,
            }),
            Err(RunCodeError::Execution(msg)) => Self::ExecutionError(msg),
            Err(RunCodeError::Host(msg)) => Self::HostError(msg),
        }
    }
}

/// Run an arbitrary Michelson script against the current state without
/// originating it — the primitive behind the node's `run_script_view`.
///
/// [`tezos_execution::run_code`] writes as it interprets; the run is
/// wrapped in a [`SafeStorage`](tezos_evm_runtime::safe_storage::SafeStorage)
/// transaction that is always reverted, so
/// the entrypoint leaves the state it was given untouched. The result is
/// written afterwards, through the unwrapped host.
#[allow(dead_code)]
pub fn tezosx_run_code_fn<Host>(host: &mut Host)
where
    Host: StorageV1 + CoreStorage,
{
    let mut rk: RuntimeKeyspaces<KernelHost<Host, &mut Host>, _> =
        match RuntimeKeyspaces::init(host) {
            Ok(rk) => rk,
            Err(err) => {
                log!(Error, "Failed to init the runtime keyspaces: {:?}", err);
                return;
            }
        };
    let result = match rk.base().get(&TEZOSX_RUN_CODE_INPUT_KEY) {
        // Reported through the error channel rather than leaving the node
        // to fail on an absent result key. `Host`, not `Execution`: the
        // caller's script is not what is wrong.
        None => RunCodeResult::HostError("run_code input not found".to_string()),
        Some(payload) => run_code_from_input(&mut rk, &payload).into(),
    };
    let mut bytes = Vec::new();
    if let Err(err) = result.bin_write(&mut bytes) {
        log!(
            Error,
            "Error encoding the Tezos X run_code result: {:?}",
            err
        );
        return;
    }
    if let Err(err) = rk.base_mut().set(&TEZOSX_RUN_CODE_RESULT_KEY, bytes) {
        log!(
            Error,
            "Error writing the Tezos X run_code result: {:?}",
            err
        );
    }
}

/// Validate a decoded input against the runtime's ranges, and inject the
/// block's own `now` and `level` where the caller named none. Pure, so
/// the boundary semantics are unit-testable without a host.
fn run_code_params(
    input: RunCodeInput,
    default_now: i64,
    default_level: u32,
) -> Result<tezos_execution::RunCodeParams, String> {
    use tezos_execution::TezlinkOperationGas;

    let self_address = match input.self_address {
        Contract::Originated(kt1) => kt1,
        Contract::Implicit(pkh) => {
            return Err(format!("`self` must be a KT1, got {pkh}"))
        }
    };
    let amount: u64 = input
        .amount
        .try_into()
        .map_err(|_| "`amount` must not be negative".to_string())?;
    let sender = input.sender.map(tezos_execution::address_from_contract);
    let balance = input
        .balance
        .map(|balance| {
            balance
                .try_into()
                .map_err(|_| "`balance` must not be negative".to_string())
        })
        .transpose()?;
    let milligas = match input.gas {
        None => TezlinkOperationGas::MAX_LIMIT,
        Some(gas) => u64::try_from(gas)
            .map_err(|_| "`gas` must not be negative".to_string())?
            .saturating_mul(1000)
            .min(TezlinkOperationGas::MAX_LIMIT),
    };
    // Narrower than L1, which accepts pre-epoch timestamps: the
    // enshrined-view path renders `now` into `X-Tezos-Timestamp`, which
    // the EVM side parses as a u64. Refuse it here, with a clear reason,
    // rather than deep inside a view.
    if let Some(now) = input.now {
        if now < 0 {
            return Err(format!("`now` must not be negative, got {now}"));
        }
    }
    let entrypoint = Entrypoint::try_from(input.entrypoint.as_str())
        .map_err(|e| format!("invalid `entrypoint`: {e:?}"))?;

    Ok(tezos_execution::RunCodeParams {
        script: input.script,
        storage: input.storage,
        input: input.input,
        entrypoint,
        self_address,
        sender,
        payer: input.payer,
        amount,
        balance,
        milligas,
        chain_id: input.chain_id,
        level: tezos_tezlink::enc_wrappers::BlockNumber {
            block_number: input.level.unwrap_or(default_level),
        },
        now: tezos_smart_rollup::types::Timestamp::from(input.now.unwrap_or(default_now)),
    })
}

/// Decode and validate the input, build the block environment, and run
/// the script.
fn run_code_from_input<Host, KS>(
    rk: &mut RuntimeKeyspaces<Host, KS>,
    payload: &[u8],
) -> Result<tezos_execution::RunCodeOutput, tezos_execution::RunCodeError>
where
    Host: StorageV1,
    KS: KeySpace,
{
    use tezos_execution::RunCodeError;

    let input = RunCodeInput::nom_read_exact(payload).map_err(|e| {
        RunCodeError::Execution(format!("cannot decode the input: {e:?}"))
    })?;

    let chain_config = fetch_tezosx_configuration(rk);
    // Reading the block header is durable-storage work: a failure here is
    // infrastructure, not the caller's script being wrong.
    let blueprint_header = read_current_blueprint_header(rk.base()).map_err(|err| {
        RunCodeError::Host(format!("cannot read the blueprint header: {err:?}"))
    })?;

    let params = run_code_params(
        input,
        blueprint_header.timestamp.as_u64() as i64,
        blueprint_header.number.low_u32(),
    )
    .map_err(RunCodeError::Execution)?;

    let registry = chain_config.init_registry();
    let block_in_progress = bip_from_blueprint(
        rk.host(),
        &chain_config,
        blueprint_header.number,
        H256::zero(),
        H256::zero(),
        Blueprint {
            transactions: vec![],
            timestamp: blueprint_header.timestamp,
        },
    );
    let mut block_constants = chain_config
        .constants(
            rk.host_mut(),
            &block_in_progress,
            U256::zero(),
            H160::zero(),
        )
        .map_err(|err| {
            RunCodeError::Host(format!("cannot build block constants: {err:?}"))
        })?;
    // A CRAC into the EVM reads its block from the journal's
    // `outer_block`, so `block.number` / `block.timestamp` must answer
    // with the same pair Michelson sees in `LEVEL` / `NOW`. Only these
    // two: the rest stays gated on the real blueprint level.
    block_constants.evm_runtime_block_constants.number =
        U256::from(params.level.block_number);
    block_constants.evm_runtime_block_constants.timestamp =
        U256::from(params.now.as_u64());
    let crac_id = tezosx_journal::CracId::new(0, 0);
    // There is no operation to seed the identities from, and the run is
    // always reverted: no origination address it derives outlives the call.
    let mut journal = tezosx_journal::TezosXJournal::new(
        crac_id,
        TezosXHashes::zero(),
        block_constants.evm_runtime_block_constants.clone(),
    );

    // The roots an applied Michelson operation snapshots, unnarrowed:
    // nothing here proves the run touches accounts only.
    let mut safe_rk = rk.to_safe_host(
        block_constants
            .michelson_runtime_block_constants
            .safe_roots
            .clone(),
    );
    safe_rk.host_mut().start().map_err(|err| {
        RunCodeError::Host(format!("cannot snapshot the state: {err:?}"))
    })?;

    let result =
        tezos_execution::run_code(&mut safe_rk, &registry, &mut journal, &params);

    match (result, safe_rk.host_mut().revert()) {
        (result, Ok(())) => result,
        (Ok(_), Err(err)) => Err(RunCodeError::Host(format!(
            "cannot revert the simulation: {err:?}"
        ))),
        // The run's own error is the one the caller asked about.
        (Err(run_err), Err(err)) => {
            log!(Error, "Reverting the run_code simulation failed: {:?}", err);
            Err(run_err)
        }
    }
}

/// Query the entrypoints and synthetic views of a contract and write
/// the result through the `/base` keyspace at `result_key`.
///
/// This works for both originated and enshrined smart contracts of
/// the Michelson runtime but is currently only used for enshrined
/// contracts. Synthetic views are returned only for enshrined
/// contracts; for originated contracts the views are already
/// embedded in the on-chain Michelson code and are discovered
/// through `/script` directly.
///
/// Input: binary-encoded contract AddressHash (22 bytes).
fn handle_query_entrypoints_to<Host, R, KS>(
    rk: &mut RuntimeKeyspaces<KernelHost<R, Host>, KS>,
    payload: &[u8],
    result_key: &Key,
) where
    R: StorageV1,
    Host: std::borrow::BorrowMut<R> + std::borrow::Borrow<R>,
    KS: KeySpace,
{
    let address = match mir::ast::AddressHash::try_from(payload) {
        Ok(a) => a,
        Err(err) => {
            log!(
                Error,
                "Tezos X entrypoints: invalid AddressHash payload ({} bytes): {:?}",
                payload.len(),
                err
            );
            return;
        }
    };
    // This entrypoint is only used in the context of the Tezos X Michelson runtime.
    // RPC inspection path: no on-chain operation gas to bill against.
    // Gas::default() is the L1 max per op, but the L2 per-op cap depends
    // on the conversion coefficient and is currently larger, so default
    // would reject RPC calls that would succeed in an operation. Use
    // Gas::unmetered() instead.
    let entrypoints = tezos_execution::get_contract_entrypoint(
        rk.host(),
        &address,
        &mut Gas::unmetered(),
    );
    let views = tezos_execution::get_enshrined_contract_views(rk.host(), &address)
        .unwrap_or_default();
    let result = match encode_entrypoints_result(entrypoints, views) {
        Ok(bytes) => bytes,
        Err(err) => {
            log!(
                Error,
                "Tezos X entrypoints: failed to encode result: {:?}",
                err
            );
            return;
        }
    };
    if let Err(err) = rk.base_mut().set(result_key, result) {
        log!(Error, "Error writing tezos entrypoints result: {:?}", err);
    }
}

/// Serialize the entrypoints and synthetic views of a contract for
/// the `tezosx_michelson_entrypoints` result.
///
/// RLP encoding (outer 1-element list is the canonical-option
/// `Some` wrapper from [`append_option_canonical`]; the empty list
/// is the canonical `None`):
/// - `List []`                  if `None` (contract not found);
/// - `List [[entries, views]]`  if `Some(map)`, where:
///     * `entries` is an RLP list of pairs `[name_bytes,
///       micheline_type_bytes]` — one per entrypoint;
///     * `views` is an RLP list of triples `[name_bytes,
///       parameter_type_bytes, return_type_bytes]` — one per
///       synthetic view (empty for contracts without synthetic
///       views).
///
/// The micheline_type_bytes are treated as bytes, and encoded using
/// data_encoding via MIR.
///
/// **Determinism warning.** The input is a [`HashMap`], whose iteration order
/// depends on a per-process random seed. Iterating it directly into an RLP
/// (or any other byte-level) encoder would produce output that varies across
/// runs and — critically — across the native vs. WASM kernel executions,
/// which would diverge the rollup PVM. Any new kernel code that serializes a
/// `HashMap` to bytes must apply the same sort-before-encode discipline, or
/// better, use [`BTreeMap`] from the start so the ordering invariant is
/// enforced structurally. The `views` list is already a `Vec` from a
/// `match`-based enumeration, so its order is already deterministic; we
/// nevertheless sort it for symmetry and to keep the canonical form stable
/// against future re-ordering of the enumeration source.
fn encode_entrypoints_result(
    entrypoints_opt: Option<HashMap<Entrypoint, Type>>,
    views: Vec<(&'static str, Type, Type)>,
) -> anyhow::Result<Vec<u8>> {
    let parser = Parser::new();
    let mut gas = Gas::default();
    let encode_type = |ty: Type, gas: &mut Gas| -> anyhow::Result<Vec<u8>> {
        // NB: into_micheline_optimized_legacy linearizes right-comb pairs,
        // producing multi-arg pairs (equivalent to L1 normalize_types=true).
        Ok(ty
            .into_micheline_optimized_legacy(&parser.arena, gas)?
            .encode(gas)??)
    };
    // Pre-encode all entries up front so encoding errors can be surfaced
    // to the caller (the RLP stream API does not let us return errors
    // from inside the encode closure).
    let mut encoded_entries: Option<Vec<(Vec<u8>, Vec<u8>)>> = entrypoints_opt
        .map(|entrypoints| {
            entrypoints
                .into_iter()
                .map(|(name, ty)| {
                    let name_bytes = name.to_string().into_bytes();
                    let type_bytes = encode_type(ty, &mut gas)?;
                    Ok((name_bytes, type_bytes))
                })
                .collect::<anyhow::Result<Vec<_>>>()
        })
        .transpose()?;
    // Sort entries by name before encoding: see the "Determinism
    // warning" on the enclosing function. HashMap iteration order is
    // not stable across runs (and differs between native and WASM),
    // so the resulting bytes must be made canonical here.
    if let Some(entries) = encoded_entries.as_mut() {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    }
    let mut encoded_views: Vec<(Vec<u8>, Vec<u8>, Vec<u8>)> = views
        .into_iter()
        .map(|(name, param_ty, return_ty)| {
            let name_bytes = name.as_bytes().to_vec();
            let param_bytes = encode_type(param_ty, &mut gas)?;
            let return_bytes = encode_type(return_ty, &mut gas)?;
            Ok((name_bytes, param_bytes, return_bytes))
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    encoded_views.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
    let mut stream = rlp::RlpStream::new();
    append_option_canonical(&mut stream, &encoded_entries, |s, entries| {
        s.begin_list(2);
        s.begin_list(entries.len());
        for (name_bytes, type_bytes) in entries {
            s.begin_list(2);
            s.append(name_bytes);
            s.append(type_bytes);
        }
        s.begin_list(encoded_views.len());
        for (name_bytes, param_bytes, return_bytes) in &encoded_views {
            s.begin_list(3);
            s.append(name_bytes);
            s.append(param_bytes);
            s.append(return_bytes);
        }
        s
    });
    Ok(stream.out().to_vec())
}

#[cfg(test)]
mod tests {
    use mir::ast::{Entrypoint, Micheline, Type};
    use mir::gas::Gas;

    /// The pinned vectors freeze the wire format shared with the node's
    /// `Run_code` module (`tezos_backend.ml`), which pins the same ones
    /// in `etherlink/bin_node/test/test_run_code_codec.ml` — keep the
    /// two in step, that is what makes them an oracle rather than a
    /// snapshot of this file's own derives.
    mod run_code_codec {
        use super::super::{
            run_code_params, RunCodeInput, RunCodeResult, RunCodeStorage,
        };
        use tezos_data_encoding::enc::BinWriter;
        use tezos_data_encoding::nom::NomReader;
        use tezos_execution::TezlinkOperationGas;
        use tezos_protocol::contract::Contract;

        const KT1: &str = "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye";
        const KT1_SENDER: &str = "KT1Lc9a9E7vqt6XYtkUbrErDGLQ55HztXV5N";
        const TZ1: &str = "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw";

        // Field by field: dynamic bytes (4-byte length prefix) for
        // script / storage / input, dynamic string for the entrypoint,
        // 4 raw bytes of chain id, 22 bytes of contract (`01` + KT1
        // hash + padding), int64 amount, then the six options (`00`
        // absent, `ff` + payload present).
        const FULL_INPUT: &str =
            "000000010200000001030000000200070000000764656661756c74f3d48b2a\
             011f2d825fdd9da219235510335e558520235f4f54000000000000000007\
             ff0183e44ba40860b4042688621bd56fbed9fa12e9c500\
             ff002422090f872dfd3a39471bb23f180e6dfed030f3\
             ff0000000000000063ff00000000000004d2ff0000000000000005ff0000002a";

        const BARE_INPUT: &str =
            "000000010200000001030000000200070000000764656661756c74f3d48b2a\
             011f2d825fdd9da219235510335e558520235f4f54000000000000000007\
             000000000000";

        // Variant tag, then the payload: dynamic bytes for the storage,
        // dynamic string for an error message.
        const SUCCESS_RESULT: &str = "0000000002000c";

        const EXECUTION_ERROR_RESULT: &str = "0100000010696c6c2d747970656420736372697074";

        const HOST_ERROR_RESULT: &str = "020000001273746f7261676520756e7265616461626c65";

        fn contract(b58: &str) -> Contract {
            Contract::from_b58check(b58).expect("valid address")
        }

        fn pkh(b58: &str) -> tezos_crypto_rs::public_key_hash::PublicKeyHash {
            tezos_crypto_rs::public_key_hash::PublicKeyHash::from_b58check(b58)
                .expect("valid pkh")
        }

        fn full_input() -> RunCodeInput {
            RunCodeInput {
                script: vec![0x02],
                storage: vec![0x03],
                // Micheline `int 7`.
                input: vec![0x00, 0x07],
                entrypoint: "default".to_string(),
                chain_id: tezos_crypto_rs::hash::ChainId::from([0xf3, 0xd4, 0x8b, 0x2a]),
                self_address: contract(KT1),
                amount: 7,
                sender: Some(contract(KT1_SENDER)),
                payer: Some(pkh(TZ1)),
                balance: Some(99),
                gas: Some(1234),
                now: Some(5),
                level: Some(42),
            }
        }

        fn bare_input() -> RunCodeInput {
            RunCodeInput {
                sender: None,
                payer: None,
                balance: None,
                gas: None,
                now: None,
                level: None,
                ..full_input()
            }
        }

        fn encode(input: &RunCodeInput) -> Vec<u8> {
            let mut bytes = Vec::new();
            input.bin_write(&mut bytes).expect("the input encodes");
            bytes
        }

        #[test]
        fn input_wire_format_is_pinned() {
            for (input, pinned) in
                [(full_input(), FULL_INPUT), (bare_input(), BARE_INPUT)]
            {
                assert_eq!(hex::encode(encode(&input)), pinned);
                // Against the literal, not against `encode`'s output:
                // decoding is the direction the kernel actually runs.
                let decoded = RunCodeInput::nom_read_exact(
                    &hex::decode(pinned).expect("valid hex"),
                )
                .expect("the pinned input decodes");
                assert_eq!(decoded, input);
            }
        }

        #[test]
        fn truncated_input_does_not_decode() {
            let bytes = encode(&full_input());
            RunCodeInput::nom_read_exact(&bytes[..bytes.len() / 2])
                .expect_err("a truncated input must not decode");
        }

        /// Every input the wire can carry but the runtime cannot honour,
        /// with the reason it is refused here rather than deeper in.
        #[test]
        fn rejects_out_of_range_inputs() {
            let cases: [(&str, RunCodeInput, &str); 5] = [
                (
                    "an implicit `self`",
                    RunCodeInput {
                        self_address: contract(TZ1),
                        ..full_input()
                    },
                    "must be a KT1",
                ),
                (
                    // Narrower than L1, which accepts pre-epoch
                    // timestamps: `now` is rendered into
                    // `X-Tezos-Timestamp`, which the EVM side reads as a
                    // u64.
                    "a negative `now`",
                    RunCodeInput {
                        now: Some(-5),
                        ..full_input()
                    },
                    "must not be negative",
                ),
                (
                    "a negative `amount`",
                    RunCodeInput {
                        amount: -1,
                        ..full_input()
                    },
                    "must not be negative",
                ),
                (
                    "a negative `balance`",
                    RunCodeInput {
                        balance: Some(-1),
                        ..full_input()
                    },
                    "must not be negative",
                ),
                (
                    "a negative `gas`",
                    RunCodeInput {
                        gas: Some(-1),
                        ..full_input()
                    },
                    "must not be negative",
                ),
            ];
            for (label, input, expected) in cases {
                let err = match run_code_params(input, 0, 0) {
                    Ok(_) => panic!("{label} must not validate"),
                    Err(err) => err,
                };
                assert!(
                    err.contains(expected),
                    "{label}: expected an error mentioning {expected:?}, got {err:?}"
                );
            }
        }

        #[test]
        fn clamps_gas_to_the_operation_cap() {
            let params = |gas| {
                run_code_params(
                    RunCodeInput {
                        gas,
                        ..full_input()
                    },
                    0,
                    0,
                )
                .expect("validates")
            };
            assert_eq!(params(None).milligas, TezlinkOperationGas::MAX_LIMIT);
            assert_eq!(
                params(Some(i64::MAX)).milligas,
                TezlinkOperationGas::MAX_LIMIT
            );
            assert_eq!(params(Some(1234)).milligas, 1_234_000);
        }

        /// Absent options must fall back, not zero out — a `0` would
        /// silently run at the epoch / level 0.
        #[test]
        fn absent_options_fall_back_to_the_block() {
            let params = run_code_params(bare_input(), 1234, 42).expect("validates");
            assert_eq!(params.now.as_u64(), 1234);
            assert_eq!(params.level.block_number, 42);
            assert!(params.sender.is_none());
            assert!(params.payer.is_none());
            assert_eq!(params.balance, None);
        }

        #[test]
        fn result_wire_format_is_pinned() {
            let encode = |result: &RunCodeResult| {
                let mut bytes = Vec::new();
                result.bin_write(&mut bytes).expect("the result encodes");
                hex::encode(bytes)
            };
            let pinned = [
                (
                    RunCodeResult::Success(RunCodeStorage {
                        storage: vec![0x00, 0x0c],
                    }),
                    SUCCESS_RESULT,
                ),
                (
                    RunCodeResult::ExecutionError("ill-typed script".to_string()),
                    EXECUTION_ERROR_RESULT,
                ),
                (
                    RunCodeResult::HostError("storage unreadable".to_string()),
                    HOST_ERROR_RESULT,
                ),
            ];
            for (result, expected) in pinned {
                assert_eq!(encode(&result), expected);
            }
        }
    }
    use mir::parser::Parser;
    use std::collections::HashMap;
    use tezos_evm_runtime::runtime_keyspaces::RuntimeKeyspaces;
    use tezos_smart_rollup_keyspace::KeySpace;

    use crate::evm_node_entrypoint::tezosx_michelson_entrypoints_fn;

    use super::{
        encode_entrypoints_result, TEZOSX_ENTRYPOINTS_INPUT_KEY,
        TEZOSX_ENTRYPOINTS_RESULT_KEY,
    };
    use tezos_execution::enshrined_contracts::EnshrinedContracts;

    /// Decodes the output of `encode_entrypoints_result` back into a
    /// `(entrypoints, views)` pair, performing a full roundtrip through
    /// Micheline. The `views` map is keyed by view name and stores
    /// `(parameter_type, return_type)`.
    #[allow(clippy::type_complexity)]
    fn decode_result(
        bytes: &[u8],
    ) -> Option<(HashMap<String, Type>, HashMap<String, (Type, Type)>)> {
        let parser = Parser::new();
        let mut gas = Gas::default();
        let decode_type = |bytes: &[u8], gas: &mut Gas| -> Type {
            let micheline =
                Micheline::decode_raw(&parser.arena, bytes, &mut Gas::default())
                    .expect("unmetered Gas cannot OOG")
                    .expect("decode micheline");
            micheline.parse_ty(gas).expect("parse type")
        };
        let rlp = rlp::Rlp::new(bytes);
        assert!(rlp.is_list(), "expected RLP list");
        match rlp.item_count().expect("item count") {
            0 => None,
            1 => {
                let inner = rlp.at(0).expect("inner list");
                assert!(inner.is_list(), "expected inner RLP list");
                assert_eq!(
                    inner.item_count().expect("inner count"),
                    2,
                    "inner list must contain [entries, views]"
                );
                let entries_rlp = inner.at(0).expect("entries list");
                let views_rlp = inner.at(1).expect("views list");
                let entries_n = entries_rlp.item_count().expect("entries count");
                let mut entries = HashMap::with_capacity(entries_n);
                for i in 0..entries_n {
                    let pair = entries_rlp.at(i).expect("pair");
                    assert!(pair.is_list(), "expected pair to be a list");
                    assert_eq!(pair.item_count().expect("pair count"), 2);
                    let name_bytes: Vec<u8> =
                        pair.at(0).expect("name").as_val().expect("name val");
                    let type_bytes: Vec<u8> =
                        pair.at(1).expect("type").as_val().expect("type val");
                    let name = String::from_utf8(name_bytes).expect("utf8 name");
                    entries.insert(name, decode_type(&type_bytes, &mut gas));
                }
                let views_n = views_rlp.item_count().expect("views count");
                let mut views = HashMap::with_capacity(views_n);
                for i in 0..views_n {
                    let triple = views_rlp.at(i).expect("triple");
                    assert!(triple.is_list(), "expected triple to be a list");
                    assert_eq!(triple.item_count().expect("triple count"), 3);
                    let name_bytes: Vec<u8> =
                        triple.at(0).expect("name").as_val().expect("name val");
                    let param_bytes: Vec<u8> =
                        triple.at(1).expect("param").as_val().expect("param val");
                    let return_bytes: Vec<u8> =
                        triple.at(2).expect("return").as_val().expect("return val");
                    let name = String::from_utf8(name_bytes).expect("utf8 name");
                    views.insert(
                        name,
                        (
                            decode_type(&param_bytes, &mut gas),
                            decode_type(&return_bytes, &mut gas),
                        ),
                    );
                }
                Some((entries, views))
            }
            n => panic!("unexpected outer RLP list length: {n}"),
        }
    }

    /// Encode then decode a map of entrypoints (and no views),
    /// asserting the roundtrip preserves it.
    fn assert_roundtrip(map: HashMap<Entrypoint, Type>) {
        let expected: HashMap<String, Type> = map
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        let encoded = encode_entrypoints_result(Some(map), vec![]).expect("encode ok");
        let (decoded, views) = decode_result(&encoded).expect("roundtrip should be Some");
        assert_eq!(decoded, expected);
        assert!(views.is_empty(), "no views expected");
    }

    #[test]
    fn test_encode_none() {
        let result = encode_entrypoints_result(None, vec![]).expect("encode ok");
        // RLP empty list: 0xc0
        assert_eq!(result, vec![0xc0]);
    }

    #[test]
    fn test_encode_empty_map() {
        let result =
            encode_entrypoints_result(Some(HashMap::new()), vec![]).expect("encode ok");
        // Must be distinct from None
        assert_ne!(
            result,
            encode_entrypoints_result(None, vec![]).expect("encode ok")
        );
        assert_roundtrip(HashMap::new());
    }

    #[test]
    fn test_encode_single_entry() {
        let mut map = HashMap::new();
        map.insert(
            Entrypoint::try_from("default").expect("valid"),
            Type::String,
        );
        assert_roundtrip(map);
    }

    #[test]
    fn test_encode_multiple_entries_same_type() {
        let ty = Type::new_pair(Type::String, Type::new_pair(Type::Bytes, Type::Int));
        let mut map = HashMap::new();
        map.insert(Entrypoint::try_from("transfer").expect("valid"), ty.clone());
        map.insert(Entrypoint::try_from("approve").expect("valid"), ty);
        assert_roundtrip(map);
    }

    #[test]
    fn test_encode_multiple_entries_different_types() {
        let mut map = HashMap::new();
        map.insert(
            Entrypoint::try_from("default").expect("valid"),
            Type::String,
        );
        map.insert(
            Entrypoint::try_from("transfer").expect("valid"),
            Type::new_pair(Type::Bytes, Type::Int),
        );
        map.insert(Entrypoint::try_from("approve").expect("valid"), Type::Nat);
        assert_roundtrip(map);
    }

    /// Regression test for the determinism guarantee documented on
    /// `encode_entrypoints_result`. The same logical input must produce
    /// byte-identical output regardless of `HashMap` insertion order
    /// (and, by extension, of any per-process hash-seed randomization),
    /// because the bytes end up in the rollup PVM state.
    #[test]
    fn test_encode_is_deterministic_regardless_of_insertion_order() {
        let entries = [
            ("alpha", Type::String),
            ("beta", Type::Nat),
            ("gamma", Type::Bytes),
            ("delta", Type::Int),
        ];

        let mut forward = HashMap::new();
        for (name, ty) in entries.iter() {
            forward.insert(Entrypoint::try_from(*name).expect("valid"), ty.clone());
        }

        let mut reversed = HashMap::new();
        for (name, ty) in entries.iter().rev() {
            reversed.insert(Entrypoint::try_from(*name).expect("valid"), ty.clone());
        }

        let encoded_forward =
            encode_entrypoints_result(Some(forward), vec![]).expect("encode ok");
        let encoded_reversed =
            encode_entrypoints_result(Some(reversed), vec![]).expect("encode ok");
        assert_eq!(encoded_forward, encoded_reversed);
    }

    /// Stronger guarantee: the encoded entries are emitted in
    /// lexicographic byte order of their names. Pinning the actual
    /// ordering catches any future change that might preserve
    /// determinism while shifting the canonical key (e.g. a switch to a
    /// non-byte-wise comparator that is invariant on ASCII but differs
    /// on multi-byte UTF-8).
    #[test]
    fn test_encode_emits_entries_in_lexicographic_byte_order() {
        let mut map = HashMap::new();
        map.insert(Entrypoint::try_from("zulu").expect("valid"), Type::Unit);
        map.insert(Entrypoint::try_from("alpha").expect("valid"), Type::Unit);
        map.insert(Entrypoint::try_from("mike").expect("valid"), Type::Unit);

        let encoded = encode_entrypoints_result(Some(map), vec![]).expect("encode ok");

        let outer = rlp::Rlp::new(&encoded);
        let inner = outer.at(0).expect("inner list");
        let entries_rlp = inner.at(0).expect("entries");
        let names: Vec<String> = (0..entries_rlp.item_count().expect("entry count"))
            .map(|i| {
                let pair = entries_rlp.at(i).expect("pair");
                let bytes: Vec<u8> =
                    pair.at(0).expect("name").as_val().expect("name val");
                String::from_utf8(bytes).expect("utf8 name")
            })
            .collect();

        assert_eq!(names, vec!["alpha", "mike", "zulu"]);
    }

    /// Sibling of the entrypoints determinism guarantee: views must
    /// also be emitted in lexicographic byte order of their names so
    /// the encoded bytes are canonical regardless of how the synthetic
    /// view enumeration is ordered at its source.
    #[test]
    fn test_encode_emits_views_in_lexicographic_byte_order() {
        let entries = HashMap::new();
        let views = vec![
            ("zulu", Type::Unit, Type::Unit),
            ("alpha", Type::Unit, Type::Unit),
            ("mike", Type::Unit, Type::Unit),
        ];

        let encoded = encode_entrypoints_result(Some(entries), views).expect("encode ok");

        let outer = rlp::Rlp::new(&encoded);
        let inner = outer.at(0).expect("inner list");
        let views_rlp = inner.at(1).expect("views");
        let names: Vec<String> = (0..views_rlp.item_count().expect("views count"))
            .map(|i| {
                let triple = views_rlp.at(i).expect("triple");
                let bytes: Vec<u8> =
                    triple.at(0).expect("name").as_val().expect("name val");
                String::from_utf8(bytes).expect("utf8 name")
            })
            .collect();

        assert_eq!(names, vec!["alpha", "mike", "zulu"]);
    }

    /// Round-trip a non-empty `views` list through the encoder.
    #[test]
    fn test_encode_views_roundtrip() {
        let entries = HashMap::new();
        let views = vec![(
            "staticcall_evm",
            Type::new_pair(Type::String, Type::Bytes),
            Type::Bytes,
        )];

        let encoded = encode_entrypoints_result(Some(entries), views).expect("encode ok");
        let (decoded_entries, decoded_views) =
            decode_result(&encoded).expect("Some result expected");
        assert!(decoded_entries.is_empty());
        let (param_ty, return_ty) = decoded_views
            .get("staticcall_evm")
            .expect("view must be present");
        assert_eq!(*param_ty, Type::new_pair(Type::String, Type::Bytes));
        assert_eq!(*return_ty, Type::Bytes);
    }

    fn run_entrypoints_query(addr_hash: &[u8]) -> Vec<u8> {
        // The node seeds the input and reads the result through the `/base`
        // keyspace, and `tezosx_michelson_entrypoints_fn` consumes/produces
        // them the same way.
        let mut rk = RuntimeKeyspaces::default();
        rk.base_mut()
            .set(&TEZOSX_ENTRYPOINTS_INPUT_KEY, addr_hash)
            .expect("write input");
        tezosx_michelson_entrypoints_fn(&mut rk);
        rk.base()
            .get(&TEZOSX_ENTRYPOINTS_RESULT_KEY)
            .expect("entrypoints result should have been written")
    }

    #[test]
    fn test_entrypoints_query_gateway() {
        let result = run_entrypoints_query(
            &EnshrinedContracts::TezosXGateway.address_hash_bytes(),
        );
        let (entries, views) = decode_result(&result).expect("gateway has entrypoints");
        assert_eq!(entries.len(), 3);
        assert!(!entries.contains_key("default"));
        assert!(entries.contains_key("call"));
        assert!(entries.contains_key("call_evm"));
        assert!(entries.contains_key("collect_result"));
        assert_eq!(views.len(), 3);

        // Synthetic view `staticcall_evm`: (pair string bytes) -> bytes.
        let (param_ty, return_ty) = views
            .get("staticcall_evm")
            .expect("staticcall_evm view must be exposed");
        assert_eq!(*param_ty, Type::new_pair(Type::String, Type::Bytes));
        assert_eq!(*return_ty, Type::Bytes);

        // Synthetic view `originOf`:
        //   (pair string nat) -> (or unit (or nat (pair nat string))).
        let (param_ty, return_ty) = views
            .get("originOf")
            .expect("originOf view must be exposed");
        assert_eq!(*param_ty, Type::new_pair(Type::String, Type::Nat));
        assert_eq!(
            *return_ty,
            Type::new_or(
                Type::Unit,
                Type::new_or(Type::Nat, Type::new_pair(Type::Nat, Type::String)),
            )
        );

        // Synthetic view `resolveAddress`:
        //   (pair string (pair nat nat)) -> (option (pair nat string)).
        let (param_ty, return_ty) = views
            .get("resolveAddress")
            .expect("resolveAddress view must be exposed");
        assert_eq!(
            *param_ty,
            Type::new_pair(Type::String, Type::new_pair(Type::Nat, Type::Nat))
        );
        assert_eq!(
            *return_ty,
            Type::new_option(Type::new_pair(Type::Nat, Type::String))
        );
    }

    #[test]
    fn test_entrypoints_query_erc20() {
        let result =
            run_entrypoints_query(&EnshrinedContracts::ERC20Wrapper.address_hash_bytes());
        let (entries, views) =
            decode_result(&result).expect("ERC20 wrapper has entrypoints");
        assert_eq!(entries.len(), 2);
        assert!(entries.contains_key("transfer"));
        assert!(entries.contains_key("approve"));
        // ERC-20 wrapper has no synthetic views.
        assert!(views.is_empty(), "ERC-20 wrapper has no synthetic views");
    }

    #[test]
    fn test_entrypoints_query_unknown_contract_returns_none() {
        let unknown_kt1: [u8; 22] =
            hex::decode("01AABBCC000000000000000000000000000000000100")
                .unwrap()
                .try_into()
                .unwrap();
        let result = run_entrypoints_query(&unknown_kt1);
        // RLP empty list: 0xc0
        assert_eq!(result, vec![0xc0], "unknown contract should encode as None");
    }
}
