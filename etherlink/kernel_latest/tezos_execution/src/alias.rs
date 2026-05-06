// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::{AddressHash, ByteReprTrait};
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_tezlink::operation_result::TransferError;
use tezosx_interfaces::RuntimeId;

// Path where aliases are stored for tezos execution context
// built from tezosx-tezos-runtime::account::TEZOS_ACCOUNTS_PATH but copied here
// for now to avoid a circular dependency.
const TEZLINK_ALIASES_PATH_STR: &str = "/tez/tez_accounts/tezosx/aliases";

/// Maximum length of the full alias path: prefix + "/" + up to 44 hex
/// digits (`AddressHash::BYTE_SIZE = 22`) + "/" + up to 2 hex digits
/// (runtime tag, fits in a byte).
const ALIAS_PATH_MAX_LEN: usize = TEZLINK_ALIASES_PATH_STR.len() + 1 + 44 + 1 + 2;

const HEX: &[u8; 16] = b"0123456789abcdef";

/// Append the lowercase hex representation of `b` (two characters) to `s`.
fn push_hex_byte(s: &mut String, b: u8) {
    s.push(char::from(HEX[(b >> 4) as usize]));
    s.push(char::from(HEX[(b & 0xf) as usize]));
}

fn alias_path(
    source: &AddressHash,
    runtime: RuntimeId,
) -> Result<OwnedPath, TransferError> {
    let mut s = String::with_capacity(ALIAS_PATH_MAX_LEN);
    s.push_str(TEZLINK_ALIASES_PATH_STR);
    s.push('/');
    for b in source.to_bytes_vec() {
        push_hex_byte(&mut s, b);
    }
    s.push('/');
    push_hex_byte(&mut s, runtime.into());
    OwnedPath::try_from(s).map_err(|e| TransferError::GatewayError(e.to_string()))
}

pub fn store_alias(
    host: &mut impl StorageV1,
    source: &AddressHash,
    runtime: RuntimeId,
    alias: &str,
) -> Result<(), TransferError> {
    let path = alias_path(source, runtime)?;

    host.store_write_all(&path, alias.as_bytes())
        .map_err(|e| TransferError::GatewayError(e.to_string()))?;

    Ok(())
}

pub fn get_alias(
    host: &mut impl StorageV1,
    source: &AddressHash,
    runtime: RuntimeId,
) -> Result<Option<String>, TransferError> {
    let path = alias_path(source, runtime)?;

    match host.store_read_all(&path) {
        Ok(bytes) => Ok(Some(
            String::from_utf8(bytes)
                .map_err(|e| TransferError::GatewayError(e.to_string()))?,
        )),
        Err(tezos_smart_rollup_host::runtime::RuntimeError::PathNotFound) => Ok(None),
        Err(e) => Err(TransferError::GatewayError(e.to_string())),
    }
}
