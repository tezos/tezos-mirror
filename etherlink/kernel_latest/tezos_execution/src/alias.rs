// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{AddressHash, ByteReprTrait};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::RuntimeId;

// Path where aliases are stored for tezos execution context
// built from tezosx-tezos-runtime::account::TEZOS_ACCOUNTS_PATH but copied here
// for now to avoid a circular dependency.
const TEZLINK_ALIASES_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts/tezos/aliases");

fn alias_path(
    source: &AddressHash,
    runtime: RuntimeId,
) -> Result<OwnedPath, TransferError> {
    let address_bytes = source.to_bytes_vec();
    let suffix = OwnedPath::try_from(format!(
        "/{}/{:x}",
        hex::encode(&address_bytes),
        <RuntimeId as Into<u8>>::into(runtime)
    ))
    .map_err(|e| TransferError::GatewayError(e.to_string()))?;
    concat(&TEZLINK_ALIASES_PATH, &suffix)
        .map_err(|e| TransferError::GatewayError(e.to_string()))
}

pub fn store_alias<Host: Runtime>(
    host: &mut Host,
    source: &AddressHash,
    runtime: RuntimeId,
    alias: &[u8],
) -> Result<(), TransferError> {
    let path = alias_path(source, runtime)?;

    host.store_write_all(&path, alias)
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;

    Ok(())
}

pub fn get_alias<Host: Runtime>(
    host: &mut Host,
    source: &AddressHash,
    runtime: RuntimeId,
) -> Result<Option<Vec<u8>>, TransferError> {
    let path = alias_path(source, runtime)?;

    match host.store_read_all(&path) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(tezos_smart_rollup_host::runtime::RuntimeError::PathNotFound) => Ok(None),
        Err(e) => Err(TransferError::GatewayError(e.to_string())),
    }
}
