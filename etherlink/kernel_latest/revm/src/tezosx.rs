use revm::primitives::Address;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezosx_interfaces::RuntimeId;

use crate::Error;

// Path where accounts informations are stored.
const ACCOUNTS_PATH: RefPath = crate::storage::world_state_handler::EVM_ACCOUNTS_PATH;

pub fn store_alias(
    host: &mut impl Runtime,
    native_address: &Address,
    runtime_id: RuntimeId,
    alias: &[u8],
) -> Result<(), Error> {
    let address_bytes = native_address.0;
    let path = OwnedPath::try_from(format!(
        "/aliases/{:x}/{:x}",
        address_bytes,
        <RuntimeId as Into<u8>>::into(runtime_id)
    ))
    .map_err(|e| Error::Custom(e.to_string()))?;
    host.store_write_all(
        &concat(&ACCOUNTS_PATH, &path).map_err(|e| Error::Custom(e.to_string()))?,
        alias,
    )?;
    Ok(())
}

pub fn get_alias(
    host: &impl Runtime,
    native_address: &Address,
    runtime_id: RuntimeId,
) -> Result<Option<Vec<u8>>, Error> {
    let address_bytes = native_address.0;
    let path = OwnedPath::try_from(format!(
        "/aliases/{:x}/{:x}",
        address_bytes,
        <RuntimeId as Into<u8>>::into(runtime_id)
    ))
    .map_err(|e| Error::Custom(e.to_string()))?;
    match host.store_read_all(
        &concat(&ACCOUNTS_PATH, &path).map_err(|e| Error::Custom(e.to_string()))?,
    ) {
        Ok(bytes) => Ok(Some(bytes)),
        Err(tezos_smart_rollup_host::runtime::RuntimeError::PathNotFound) => Ok(None),
        Err(e) => Err(Error::Runtime(e)),
    }
}
