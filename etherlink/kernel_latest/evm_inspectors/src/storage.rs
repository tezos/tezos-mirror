// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::error::InspectorError;
use super::struct_logger::StructLog;

use revm::primitives::B256;
use tezos_evm_logging::{log, Level::Debug};
use tezos_indexable_storage::KeyspaceIndexableStorage;
use tezos_smart_rollup_keyspace::{extensions::KeySpaceExtNum, Key, KeyError, KeySpace};

/// Root of the trace output in the `/evm/world_state` keyspace.
const TRACE_KEY: Key = Key::from_static(b"/trace");
const CALL_TRACE_SEGMENT: &[u8] = b"/call_trace";
const STRUCT_LOGS_SEGMENT: &[u8] = b"/struct_logs";
const GAS_SEGMENT: &[u8] = b"/gas";
const FAILED_SEGMENT: &[u8] = b"/failed";
const RETURN_VALUE_SEGMENT: &[u8] = b"/return_value";

/// Key of `segment` in the trace of `hash`, or in the hash-less root when
/// `hash` is `None`.
pub fn trace_tx_key(hash: &Option<B256>, segment: &[u8]) -> Result<Key, KeyError> {
    let trace_tx_key = match hash {
        None => TRACE_KEY,
        Some(hash) => TRACE_KEY.concat(format!("/{}", hex::encode(hash)))?,
    };
    trace_tx_key.concat(segment)
}

/// Call traces of `hash`, in the `/evm/world_state` keyspace.
pub fn call_trace_storage(
    hash: &Option<B256>,
) -> Result<KeyspaceIndexableStorage, KeyError> {
    Ok(KeyspaceIndexableStorage::new(trace_tx_key(
        hash,
        CALL_TRACE_SEGMENT,
    )?))
}

/// Struct logs of `hash`, in the `/evm/world_state` keyspace.
pub fn struct_logs_storage(
    hash: &Option<B256>,
) -> Result<KeyspaceIndexableStorage, KeyError> {
    Ok(KeyspaceIndexableStorage::new(trace_tx_key(
        hash,
        STRUCT_LOGS_SEGMENT,
    )?))
}

pub fn flush_call_traces(
    evm_world_state: &mut impl KeySpace,
    traces: &[impl rlp::Encodable],
    hash: &Option<B256>,
) -> Result<(), InspectorError> {
    let encoded: Vec<Vec<u8>> = traces.iter().map(|t| rlp::encode(t).to_vec()).collect();
    call_trace_storage(hash)?.push_values(evm_world_state, &encoded)?;
    Ok(())
}

pub fn store_trace_gas(
    evm_world_state: &mut impl KeySpace,
    gas: u64,
    hash: &Option<B256>,
) -> Result<(), InspectorError> {
    evm_world_state.store_le(&trace_tx_key(hash, GAS_SEGMENT)?, gas)?;
    Ok(())
}

pub fn store_trace_failed(
    evm_world_state: &mut impl KeySpace,
    is_success: bool,
    hash: &Option<B256>,
) -> Result<(), InspectorError> {
    evm_world_state.set(
        &trace_tx_key(hash, FAILED_SEGMENT)?,
        [u8::from(!is_success)],
    )?;
    log!(Debug, "Store trace info: is_success {is_success}");
    Ok(())
}

pub fn store_return_value(
    evm_world_state: &mut impl KeySpace,
    value: &[u8],
    hash: &Option<B256>,
) -> Result<(), InspectorError> {
    evm_world_state.set(&trace_tx_key(hash, RETURN_VALUE_SEGMENT)?, value)?;
    log!(Debug, "Store trace info: value {value:?}");
    Ok(())
}

pub fn store_struct_log(
    evm_world_state: &mut impl KeySpace,
    struct_log: &StructLog,
    hash: &Option<B256>,
) -> Result<(), InspectorError> {
    let encoded_struct_log = rlp::encode(struct_log);
    struct_logs_storage(hash)?.push_value(evm_world_state, &encoded_struct_log)?;
    log!(Debug, "Store struct log: {encoded_struct_log:?}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_evm_runtime::{
        runtime::MockKernelHost, runtime_keyspaces::RuntimeKeyspaces,
    };
    use tezos_smart_rollup_host::{path::RefPath, storage::StorageV1};

    /// The trace output resolves to `/evm/world_state/trace[/<hash>]/...`,
    /// where the node reads it.
    #[test]
    fn trace_keys_keep_their_durable_paths() {
        let mut host = MockKernelHost::default();
        let mut rk = RuntimeKeyspaces::init(&mut host).unwrap();
        let hash = Some(B256::repeat_byte(0xab));
        let struct_log =
            StructLog::minimal_structure(7, 0x60, 21, 1, None, None, None, None);

        flush_call_traces(rk.evm_world_state_mut(), &[b"frame".to_vec()], &hash).unwrap();
        store_trace_gas(rk.evm_world_state_mut(), 21000, &hash).unwrap();
        store_trace_failed(rk.evm_world_state_mut(), false, &hash).unwrap();
        store_return_value(rk.evm_world_state_mut(), b"out", &hash).unwrap();
        store_struct_log(rk.evm_world_state_mut(), &struct_log, &hash).unwrap();
        flush_call_traces(rk.evm_world_state_mut(), &[b"root".to_vec()], &None).unwrap();

        let read = |path: &str| {
            rk.host()
                .store_read_all(&RefPath::assert_from(path.as_bytes()))
                .unwrap()
        };
        let prefix = format!("/evm/world_state/trace/{}", "ab".repeat(32));
        assert_eq!(
            read(&format!("{prefix}/call_trace/length")),
            1u64.to_le_bytes()
        );
        assert_eq!(
            read(&format!("{prefix}/call_trace/0")),
            rlp::encode(&b"frame".to_vec()).to_vec()
        );
        assert_eq!(read(&format!("{prefix}/gas")), 21000u64.to_le_bytes());
        assert_eq!(read(&format!("{prefix}/failed")), [1u8]);
        assert_eq!(read(&format!("{prefix}/return_value")), b"out");
        assert_eq!(
            read(&format!("{prefix}/struct_logs/length")),
            1u64.to_le_bytes()
        );
        assert_eq!(
            read(&format!("{prefix}/struct_logs/0")),
            rlp::encode(&struct_log).to_vec()
        );
        assert_eq!(
            read("/evm/world_state/trace/call_trace/length"),
            1u64.to_le_bytes()
        );
        assert_eq!(
            read("/evm/world_state/trace/call_trace/0"),
            rlp::encode(&b"root".to_vec()).to_vec()
        );
    }
}
