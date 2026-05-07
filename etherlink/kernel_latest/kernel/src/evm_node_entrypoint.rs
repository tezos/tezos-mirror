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
    apply::{ExecutionResult, RuntimeExecutionInfo, WITHDRAWAL_OUTBOX_QUEUE},
    block::bip_from_blueprint,
    blueprint::Blueprint,
    blueprint_storage::read_current_blueprint_header,
    chains::{
        self, ChainConfigTrait, EvmChainConfig, TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
    },
    configuration::fetch_pure_evm_config,
    delayed_inbox::DelayedInbox,
    storage::read_chain_id,
    sub_block,
    transaction::Transaction,
};
use mir::ast::{Entrypoint, IntoMicheline, Type};
use mir::parser::Parser;
use primitive_types::{H160, H256, U256};
use rlp::{Rlp, RlpStream};
use std::collections::HashMap;
use tezos_data_encoding::enc::BinWriter;
use tezos_ethereum::rlp_helpers::{
    append_option_canonical, decode_field, decode_field_bool, next, FromRlpBytes,
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::KernelHost;
use tezos_execution::context::Context as _;
use tezos_smart_rollup::outbox::OutboxQueue;
use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};
use tezosx_tezos_runtime::context::TezosRuntimeContext;

#[cfg(target_arch = "wasm32")]
use tezos_smart_rollup_core::rollup_host::RollupHost;

const DELAYED_INPUT_PATH: RefPath = RefPath::assert_from(b"/base/__delayed_input");

const TEZOSX_SIMULATION_INPUT: RefPath =
    RefPath::assert_from(b"/base/__simulation/input");
const TEZOSX_SIMULATION_RESULT: RefPath =
    RefPath::assert_from(b"/base/__simulation/result");

pub(crate) const TEZOSX_ENTRYPOINTS_INPUT: RefPath =
    RefPath::assert_from(b"/base/tezosx_entrypoints/input");
pub(crate) const TEZOSX_ENTRYPOINTS_RESULT: RefPath =
    RefPath::assert_from(b"/base/tezosx_entrypoints/result");

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn populate_delayed_inbox() {
    let mut sdk_host = unsafe { RollupHost::new() };
    populate_delayed_inbox_with_durable_storage(&mut sdk_host);
}

#[allow(dead_code)]
pub fn populate_delayed_inbox_with_durable_storage<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let payload = host.store_read_all(&DELAYED_INPUT_PATH).unwrap();
    let transaction = Transaction::from_rlp_bytes(&payload).unwrap().into();
    let mut delayed_inbox = DelayedInbox::new(&mut host).unwrap();
    delayed_inbox
        .save_transaction(&mut host, transaction, 0.into(), 0u32)
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
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let tx_input = match sub_block::read_single_tx_execution_input(&mut host) {
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
    match sub_block::handle_run_transaction(&mut host, tx_input) {
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
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let assemble_block_input = match sub_block::read_assemble_block_input(&mut host) {
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
    match sub_block::assemble_block(&mut host, assemble_block_input) {
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
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let input = match host.store_read_all(&TEZOSX_SIMULATION_INPUT) {
        Ok(bytes) => bytes,
        Err(err) => {
            log!(Error, "Error reading Tezos X simulation input: {:?}", err);
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

    // Build context: chain config, block constants, outbox queue.
    let eth_chain_id = match read_chain_id(&host) {
        Ok(id) => id,
        Err(err) => {
            log!(
                Error,
                "Tezos X simulation: failed to read chain id: {:?}",
                err
            );
            return;
        }
    };
    let evm_config = fetch_pure_evm_config(&mut host, eth_chain_id);
    let blueprint_header = match read_current_blueprint_header(&host) {
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
    let registry = evm_config.init_registry();

    // Parse the transaction bytes as a Tezos X transaction (blueprint
    // version 1 format: tag byte + raw bytes).
    let transaction = match EvmChainConfig::transaction_from_bytes(&transaction_bytes, 1)
    {
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
        &host,
        &evm_config,
        blueprint_header.number,
        H256::zero(),
        H256::zero(),
        Blueprint {
            transactions: vec![],
            timestamp: blueprint_header.timestamp,
        },
    );

    let block_constants = match evm_config.constants(
        &mut host,
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

    let skip_fees_check = true;
    // For Tezos transactions, call apply_tezos_operation directly with an
    // external journal so we can capture HTTP traces.  For Ethereum
    // transactions, go through the normal apply_transaction path.
    let mut trace_journal =
        tezosx_journal::TezosXJournal::new(tezosx_journal::CracId::new(0, 0));
    let execution_result = match transaction {
        chains::TezosXTransaction::Tezos(operation) => {
            let enable_gas_refund = evm_config
                .experimental_features
                .is_michelson_gas_refund_enabled();
            chains::apply_tezos_operation(
                &evm_config.michelson_chain_config().chain_id,
                &block_in_progress,
                &mut host,
                &registry,
                &block_constants.michelson_runtime_block_constants,
                operation,
                None,
                skip_signature_check,
                skip_fees_check,
                Some(&outbox_queue),
                Some(&block_constants.evm_runtime_block_constants),
                &mut trace_journal,
                enable_gas_refund,
            )
        }
        _ => evm_config.apply_transaction(
            &block_in_progress,
            &mut host,
            &registry,
            &outbox_queue,
            &block_constants,
            transaction,
            0,
            None,
            None,
            skip_signature_check,
            skip_fees_check,
            // This is the [tezosx_simulate] entrypoint, not the replay path:
            // simulation writes its own aggregate traces via
            // [store_simulation_http_traces], so the per-tx [http_trace*]
            // capture is not wanted here.
            false,
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
    let traces = trace_journal.into_http_traces();
    if let Err(err) = crate::storage::store_simulation_http_traces(&mut host, &traces) {
        log!(
            Error,
            "Tezos X simulation: failed to store HTTP traces: {:?}",
            err
        );
    }

    let applied_operation = match execution_result {
        ExecutionResult::Valid(RuntimeExecutionInfo::Tezos {
            op,
            cross_runtime_effects: _,
            consumed_milligas: _,
        }) => op,
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
    if let Err(err) = host.store_write_all(&TEZOSX_SIMULATION_RESULT, &stream.out()) {
        log!(Error, "Error writing Tezos X simulation result: {:?}", err);
    }
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn tezosx_michelson_entrypoints() {
    let mut sdk_host = unsafe { RollupHost::new() };
    tezosx_michelson_entrypoints_fn(&mut sdk_host);
}

#[allow(dead_code)]
pub fn tezosx_michelson_entrypoints_fn<Host>(host: &mut Host)
where
    Host: tezos_smart_rollup_host::runtime::Runtime,
{
    let mut host: KernelHost<Host, &mut Host> = KernelHost::init(host);
    let input = match host.store_read_all(&TEZOSX_ENTRYPOINTS_INPUT) {
        Ok(bytes) => bytes,
        Err(err) => {
            log!(Error, "Error reading tezosx entrypoints input: {:?}", err);
            return;
        }
    };
    handle_query_entrypoints_to(&mut host, &input, &TEZOSX_ENTRYPOINTS_RESULT);
}

/// Query the entrypoints of a contract and write the result to `result_path`.
///
/// This works for both originated and enshrined smart contracts of the
/// Michelson runtime but is currently only used for enshrined contracts.
///
/// Input: binary-encoded contract AddressHash (22 bytes).
fn handle_query_entrypoints_to<Host, R>(
    host: &mut KernelHost<R, Host>,
    payload: &[u8],
    result_path: &RefPath,
) where
    R: tezos_smart_rollup_host::runtime::Runtime,
    Host: std::borrow::BorrowMut<R> + std::borrow::Borrow<R>,
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
    let context =
        match TezosRuntimeContext::from_root(&TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH) {
            Ok(c) => c,
            Err(err) => {
                log!(Error, "Tezos X entrypoints: context error: {:?}", err);
                return;
            }
        };
    let entrypoints =
        tezos_execution::get_contract_entrypoint(&*host, &context, &address);
    let result = match encode_entrypoints_result(entrypoints) {
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
    if let Err(err) = host.store_write_all(result_path, &result) {
        log!(Error, "Error writing tezos entrypoints result: {:?}", err);
    }
}

/// Serialize the entrypoints of a contract for the `tezosx_michelson_entrypoints`
/// result.
///
/// RLP encoding:
/// - `List []`        if `None` (contract not found)
/// - `List [entries]` if `Some(map)`, where `entries` is an RLP list of pairs
///   `[name_bytes, micheline_type_bytes]`.
///
/// The micheline_type_bytes are treated as bytes, and encoded using data_encoding
/// via MIR.
///
/// **Determinism warning.** The input is a [`HashMap`], whose iteration order
/// depends on a per-process random seed. Iterating it directly into an RLP
/// (or any other byte-level) encoder would produce output that varies across
/// runs and — critically — across the native vs. WASM kernel executions,
/// which would diverge the rollup PVM. Any new kernel code that serializes a
/// `HashMap` to bytes must apply the same sort-before-encode discipline, or
/// better, use [`BTreeMap`] from the start so the ordering invariant is
/// enforced structurally.
fn encode_entrypoints_result(
    entrypoints_opt: Option<HashMap<Entrypoint, Type>>,
) -> Result<Vec<u8>, tezos_data_encoding::enc::BinError> {
    let parser = Parser::new();
    // Pre-encode all entries up front so encoding errors can be surfaced
    // to the caller (the RLP stream API does not let us return errors
    // from inside the encode closure).
    let mut encoded_entries: Option<Vec<(Vec<u8>, Vec<u8>)>> = entrypoints_opt
        .map(|entrypoints| {
            entrypoints
                .into_iter()
                .map(|(name, ty)| {
                    let name_bytes = name.to_string().into_bytes();
                    // NB: into_micheline_optimized_legacy linearizes right-comb pairs,
                    // producing multi-arg pairs (equivalent to L1 normalize_types=true).
                    let type_bytes =
                        ty.into_micheline_optimized_legacy(&parser.arena).encode()?;
                    Ok((name_bytes, type_bytes))
                })
                .collect::<Result<Vec<_>, tezos_data_encoding::enc::BinError>>()
        })
        .transpose()?;
    // Sort entries by name before encoding: see the "Determinism
    // warning" on the enclosing function. HashMap iteration order is
    // not stable across runs (and differs between native and WASM),
    // so the resulting bytes must be made canonical here.
    if let Some(entries) = encoded_entries.as_mut() {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
    }
    let mut stream = rlp::RlpStream::new();
    append_option_canonical(&mut stream, &encoded_entries, |s, entries| {
        s.begin_list(entries.len());
        for (name_bytes, type_bytes) in entries {
            s.begin_list(2);
            s.append(name_bytes);
            s.append(type_bytes);
        }
        s
    });
    Ok(stream.out().to_vec())
}

#[cfg(test)]
mod tests {
    use mir::ast::{Entrypoint, Micheline, Type};
    use mir::gas::Gas;
    use mir::parser::Parser;
    use std::collections::HashMap;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::MockHost;

    use crate::evm_node_entrypoint::{
        tezosx_michelson_entrypoints_fn, TEZOSX_ENTRYPOINTS_INPUT,
        TEZOSX_ENTRYPOINTS_RESULT,
    };

    use super::encode_entrypoints_result;
    use tezos_execution::enshrined_contracts::EnshrinedContracts;

    /// Decodes the output of `encode_entrypoints_result` back into a map of
    /// entrypoint name -> Type, performing a full roundtrip through Micheline.
    fn decode_result(bytes: &[u8]) -> Option<HashMap<String, Type>> {
        let rlp = rlp::Rlp::new(bytes);
        assert!(rlp.is_list(), "expected RLP list");
        match rlp.item_count().expect("item count") {
            0 => None,
            1 => {
                let inner = rlp.at(0).expect("inner list");
                assert!(inner.is_list(), "expected inner RLP list");
                let n = inner.item_count().expect("entry count");
                let mut map = HashMap::with_capacity(n);
                let parser = Parser::new();
                let mut gas = Gas::default();
                for i in 0..n {
                    let pair = inner.at(i).expect("pair");
                    assert!(pair.is_list(), "expected pair to be a list");
                    assert_eq!(pair.item_count().expect("pair count"), 2);
                    let name_bytes: Vec<u8> =
                        pair.at(0).expect("name").as_val().expect("name val");
                    let type_bytes: Vec<u8> =
                        pair.at(1).expect("type").as_val().expect("type val");
                    let name = String::from_utf8(name_bytes).expect("utf8 name");
                    let micheline = Micheline::decode_raw(&parser.arena, &type_bytes)
                        .expect("decode micheline");
                    let ty = micheline.parse_ty(&mut gas).expect("parse type");
                    map.insert(name, ty);
                }
                Some(map)
            }
            n => panic!("unexpected outer RLP list length: {n}"),
        }
    }

    /// Encode then decode a map, asserting the roundtrip preserves it.
    fn assert_roundtrip(map: HashMap<Entrypoint, Type>) {
        let expected: HashMap<String, Type> = map
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        let encoded = encode_entrypoints_result(Some(map)).expect("encode ok");
        let decoded = decode_result(&encoded).expect("roundtrip should be Some");
        assert_eq!(decoded, expected);
    }

    #[test]
    fn test_encode_none() {
        let result = encode_entrypoints_result(None).expect("encode ok");
        // RLP empty list: 0xc0
        assert_eq!(result, vec![0xc0]);
    }

    #[test]
    fn test_encode_empty_map() {
        let result = encode_entrypoints_result(Some(HashMap::new())).expect("encode ok");
        // Must be distinct from None
        assert_ne!(result, encode_entrypoints_result(None).expect("encode ok"));
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
            encode_entrypoints_result(Some(forward)).expect("encode ok");
        let encoded_reversed =
            encode_entrypoints_result(Some(reversed)).expect("encode ok");
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

        let encoded = encode_entrypoints_result(Some(map)).expect("encode ok");

        let outer = rlp::Rlp::new(&encoded);
        let inner = outer.at(0).expect("inner list");
        let names: Vec<String> = (0..inner.item_count().expect("entry count"))
            .map(|i| {
                let pair = inner.at(i).expect("pair");
                let bytes: Vec<u8> =
                    pair.at(0).expect("name").as_val().expect("name val");
                String::from_utf8(bytes).expect("utf8 name")
            })
            .collect();

        assert_eq!(names, vec!["alpha", "mike", "zulu"]);
    }

    fn run_entrypoints_query(host: &mut MockHost, addr_hash: &[u8]) -> Vec<u8> {
        host.store_write_all(&TEZOSX_ENTRYPOINTS_INPUT, addr_hash)
            .expect("write input");
        tezosx_michelson_entrypoints_fn(host);
        host.store_read_all(&TEZOSX_ENTRYPOINTS_RESULT)
            .expect("entrypoints result should have been written")
    }

    #[test]
    fn test_entrypoints_query_gateway() {
        let mut host = MockHost::default();
        let result = run_entrypoints_query(
            &mut host,
            &EnshrinedContracts::TezosXGateway.address_hash_bytes(),
        );
        let decoded = decode_result(&result).expect("gateway has entrypoints");
        assert_eq!(decoded.len(), 4);
        assert!(decoded.contains_key("default"));
        assert!(decoded.contains_key("call"));
        assert!(decoded.contains_key("call_evm"));
        assert!(decoded.contains_key("collect_result"));
    }

    #[test]
    fn test_entrypoints_query_erc20() {
        let mut host = MockHost::default();
        let result = run_entrypoints_query(
            &mut host,
            &EnshrinedContracts::ERC20Wrapper.address_hash_bytes(),
        );
        let decoded = decode_result(&result).expect("ERC20 wrapper has entrypoints");
        assert_eq!(decoded.len(), 2);
        assert!(decoded.contains_key("transfer"));
        assert!(decoded.contains_key("approve"));
    }

    #[test]
    fn test_entrypoints_query_unknown_contract_returns_none() {
        let mut host = MockHost::default();
        let unknown_kt1: [u8; 22] =
            hex::decode("01AABBCC000000000000000000000000000000000100")
                .unwrap()
                .try_into()
                .unwrap();
        let result = run_entrypoints_query(&mut host, &unknown_kt1);
        // RLP empty list: 0xc0
        assert_eq!(result, vec![0xc0], "unknown contract should encode as None");
    }
}
