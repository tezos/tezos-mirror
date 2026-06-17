// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::TezosXRuntimeError;

pub use tezos_execution::account_storage::{narith_to_u256, u256_to_narith};

pub use tezos_execution::account_storage::TezosAccountInfo;

pub use tezos_execution::account_storage::{
    path_to_implicit_account_prefix, path_to_tezos_account,
};

pub use tezos_execution::account_storage::{
    get_tezos_account_info, get_tezos_account_info_or_init, set_tezos_account_info,
};

pub use tezos_execution::account_storage::{get_origin_at, set_origin_at};

pub use tezos_execution::account_storage::TezosImplicitAccount;

/// Seed the shared alias implementation with the canonical forwarder code if
/// the slot is empty, leaving an already-populated slot untouched.
///
/// Idempotent, so it is safe to call from both seeding points: the Michelson
/// runtime activation (fresh networks) and the storage migration
/// (already-deployed networks).
///
/// The slot itself lives in `tezos_execution` (see
/// [`tezos_execution::account_storage::read_alias_implementation`]); this
/// wrapper supplies the canonical forwarder code, which is Michelson-runtime
/// specific and so belongs in this crate rather than in the generic execution
/// layer.
pub fn init_alias_implementation(
    host: &mut impl StorageV1,
) -> Result<(), TezosXRuntimeError> {
    use tezos_execution::account_storage::{
        read_alias_implementation, write_alias_implementation,
    };
    // Storage failures keep their structured `Storage` classification through
    // the existing `From<tezos_storage::error::Error>` impl; only the hex decode
    // of the compile-time-constant forwarder string needs a custom message.
    let seeded = read_alias_implementation(host)?;
    if seeded.is_none() {
        let code = crate::alias_forwarder::forwarder_code().map_err(|e| {
            TezosXRuntimeError::Custom(format!(
                "decoding forwarder code from hex failed: {e}"
            ))
        })?;
        write_alias_implementation(host, &code)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn alias_implementation_seed_and_roundtrip() {
        use crate::account::init_alias_implementation;
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{
            read_alias_implementation, write_alias_implementation,
        };

        let mut host = MockKernelHost::default();

        // Absent before seeding.
        assert!(read_alias_implementation(&host).unwrap().is_none());

        // Seeding installs the canonical forwarder code.
        init_alias_implementation(&mut host).unwrap();
        let forwarder = crate::alias_forwarder::forwarder_code().unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(forwarder.clone())
        );

        // Seeding again is a no-op: an already-populated slot is untouched.
        write_alias_implementation(&mut host, b"already here").unwrap();
        init_alias_implementation(&mut host).unwrap();
        assert_eq!(
            read_alias_implementation(&host).unwrap(),
            Some(b"already here".to_vec())
        );

        // An explicit write overwrites — the O(1) upgrade primitive.
        write_alias_implementation(&mut host, &forwarder).unwrap();
        assert_eq!(read_alias_implementation(&host).unwrap(), Some(forwarder));
    }
}
