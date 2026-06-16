// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::struct_logger::StructLog;
use crate::error::EvmKernelError;

use revm::primitives::B256;
use tezos_evm_logging::{log, Level::Debug};
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    storage::StorageV1,
};

const EVM_TRACE: RefPath = RefPath::assert_from(b"/evm/trace");
const CALL_TRACE: RefPath = RefPath::assert_from(b"/call_trace");
const STRUCT_LOGS: RefPath = RefPath::assert_from(b"/struct_logs");
const GAS: RefPath = RefPath::assert_from(b"/gas");
const FAILED: RefPath = RefPath::assert_from(b"/failed");
const RETURN_VALUE: RefPath = RefPath::assert_from(b"/return_value");

pub fn trace_tx_path(
    hash: &Option<B256>,
    field: &RefPath,
) -> Result<OwnedPath, EvmKernelError> {
    let trace_tx_path = match hash {
        None => EVM_TRACE.into(),
        Some(hash) => {
            let hash = hex::encode(hash);
            let raw_tx_path: Vec<u8> = format!("/{}", &hash).into();
            let tx_path = OwnedPath::try_from(raw_tx_path)?;
            concat(&EVM_TRACE, &tx_path)?
        }
    };
    Ok(concat(&trace_tx_path, field)?)
}

pub fn flush_call_traces(
    host: &mut impl StorageV1,
    traces: &[impl rlp::Encodable],
    hash: &Option<B256>,
) -> Result<(), EvmKernelError> {
    let path = trace_tx_path(hash, &CALL_TRACE)?;
    let call_trace_storage = IndexableStorage::new_owned_path(path);

    let encoded: Vec<Vec<u8>> = traces.iter().map(|t| rlp::encode(t).to_vec()).collect();
    call_trace_storage.push_values(host, &encoded)?;

    Ok(())
}

pub fn store_trace_gas(
    host: &mut impl StorageV1,
    gas: u64,
    hash: &Option<B256>,
) -> Result<(), EvmKernelError> {
    let path = trace_tx_path(hash, &GAS)?;
    host.store_write(&path, gas.to_le_bytes().as_slice(), 0)?;
    Ok(())
}

pub fn store_trace_failed<Host>(
    host: &mut Host,
    is_success: bool,
    hash: &Option<B256>,
) -> Result<(), EvmKernelError>
where
    Host: StorageV1,
{
    let path = trace_tx_path(hash, &FAILED)?;
    host.store_write(&path, &[u8::from(!is_success)], 0)?;
    log!(Debug, "Store trace info: is_success {is_success}");
    Ok(())
}

pub fn store_return_value<Host>(
    host: &mut Host,
    value: &[u8],
    hash: &Option<B256>,
) -> Result<(), EvmKernelError>
where
    Host: StorageV1,
{
    let path = trace_tx_path(hash, &RETURN_VALUE)?;
    host.store_write(&path, value, 0)?;
    log!(Debug, "Store trace info: value {value:?}");
    Ok(())
}

pub fn store_struct_log<Host>(
    host: &mut Host,
    struct_log: &StructLog,
    hash: &Option<B256>,
) -> Result<(), EvmKernelError>
where
    Host: StorageV1,
{
    let encoded_strug_log = rlp::encode(struct_log);

    let path = trace_tx_path(hash, &STRUCT_LOGS)?;
    let struct_logs_storage = IndexableStorage::new_owned_path(path);

    struct_logs_storage.push_value(host, &encoded_strug_log)?;

    log!(Debug, "Store struct log: {encoded_strug_log:?}");
    Ok(())
}
