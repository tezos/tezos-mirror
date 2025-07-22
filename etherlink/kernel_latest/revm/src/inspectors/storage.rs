// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::call_tracer::CallTrace;
use crate::{helpers::concat, Error};

use revm::primitives::B256;
use tezos_evm_logging::{log, Level::Debug};
use tezos_evm_runtime::runtime::Runtime;
use tezos_indexable_storage::IndexableStorage;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};

const EVM_TRACE: RefPath = RefPath::assert_from(b"/evm/trace");
const CALL_TRACE: RefPath = RefPath::assert_from(b"/call_trace");

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
