// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::{call_tracer::CallTrace, struct_logger::StructLog};
use crate::{helpers::storage::concat, Error};

use revm::primitives::B256;
use tezos_evm_logging::{log, Level::Debug};
use tezos_evm_runtime::runtime::Runtime;
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};

const EVM_TRACE: RefPath = RefPath::assert_from(b"/evm/trace");
const CALL_TRACE: RefPath = RefPath::assert_from(b"/call_trace");
const STRUCT_LOGS: RefPath = RefPath::assert_from(b"/struct_logs");
const GAS: RefPath = RefPath::assert_from(b"/gas");
const FAILED: RefPath = RefPath::assert_from(b"/failed");
const RETURN_VALUE: RefPath = RefPath::assert_from(b"/return_value");

pub fn trace_tx_path(hash: &Option<B256>, field: &RefPath) -> Result<OwnedPath, Error> {
    let trace_tx_path = match hash {
        None => EVM_TRACE.into(),
        Some(hash) => {
            let hash = hex::encode(hash);
            let raw_tx_path: Vec<u8> = format!("/{}", &hash).into();
            let tx_path = OwnedPath::try_from(raw_tx_path)
                .map_err(|err| Error::Custom(err.to_string()))?;
            concat(&EVM_TRACE, &tx_path)?
        }
    };
    concat(&trace_tx_path, field)
}

pub fn store_call_trace<Host: Runtime>(
    host: &mut Host,
    call_trace: &CallTrace,
    hash: &Option<B256>,
) -> Result<(), Error> {
    let encoded_call_trace = rlp::encode(call_trace);

    let path = trace_tx_path(hash, &CALL_TRACE)?;
    let call_trace_storage = IndexableStorage::new_owned_path(path);

    call_trace_storage
        .push_value(host, &encoded_call_trace)
        .map_err(|err| Error::Custom(err.to_string()))?;

    log!(host, Debug, "Store call trace: {:?}", call_trace);
    Ok(())
}

pub fn store_trace_gas<Host: Runtime>(
    host: &mut Host,
    gas: u64,
    hash: &Option<B256>,
) -> Result<(), Error> {
    let path = trace_tx_path(hash, &GAS)?;
    host.store_write_all(&path, gas.to_le_bytes().as_slice())?;
    Ok(())
}

pub fn store_trace_failed<Host: Runtime>(
    host: &mut Host,
    is_success: bool,
    hash: &Option<B256>,
) -> Result<(), Error> {
    let path = trace_tx_path(hash, &FAILED)?;
    host.store_write_all(&path, &[u8::from(!is_success)])?;
    log!(host, Debug, "Store trace info: is_success {is_success}");
    Ok(())
}

pub fn store_return_value<Host: Runtime>(
    host: &mut Host,
    value: &[u8],
    hash: &Option<B256>,
) -> Result<(), Error> {
    let path = trace_tx_path(hash, &RETURN_VALUE)?;
    host.store_write_all(&path, value)?;
    log!(host, Debug, "Store trace info: value {value:?}");
    Ok(())
}

pub fn store_struct_log<Host: Runtime>(
    host: &mut Host,
    struct_log: &StructLog,
    hash: &Option<B256>,
) -> Result<(), Error> {
    let encoded_strug_log = rlp::encode(struct_log);

    let path = trace_tx_path(hash, &STRUCT_LOGS)?;
    let struct_logs_storage = IndexableStorage::new_owned_path(path);

    struct_logs_storage
        .push_value(host, &encoded_strug_log)
        .map_err(|err| Error::Custom(err.to_string()))?;

    log!(host, Debug, "Store struct log: {encoded_strug_log:?}");
    Ok(())
}
