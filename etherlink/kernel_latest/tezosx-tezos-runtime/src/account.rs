// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::public_key_hash::PublicKeyHash;
use tezos_data_encoding::types::Narith;
use tezos_execution::account_storage::{
    Manager, TezlinkAccount, TezosImplicitAccountTrait,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::path::OwnedPath;
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

pub struct TezosImplicitAccount {
    pub(crate) pkh: PublicKeyHash,
    pub(crate) path: OwnedPath,
}

impl TezlinkAccount for TezosImplicitAccount {
    fn path(&self) -> &OwnedPath {
        &self.path
    }

    fn contract(&self) -> Contract {
        Contract::Implicit(self.pkh.clone())
    }

    fn balance(
        &self,
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(u256_to_narith(&info.balance)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::TcError(format!("{e}"))),
        }
    }

    fn set_balance(
        &self,
        host: &mut impl StorageV1,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        info.balance = narith_to_u256(balance)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
    }
}

impl TezosImplicitAccountTrait for TezosImplicitAccount {
    fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }

    fn counter(
        &self,
        host: &impl StorageV1,
    ) -> Result<Narith, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(Narith::from(info.nonce)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }

    fn set_counter(
        &self,
        host: &mut impl StorageV1,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.nonce = counter
            .0
            .clone()
            .try_into()
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))
    }

    fn manager(
        &self,
        host: &impl StorageV1,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let info = get_tezos_account_info(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        match info {
            Some(info) => match info.pub_key {
                Some(pk) => Ok(Manager::Revealed(pk)),
                None => Ok(Manager::NotRevealed(self.pkh.clone())),
            },
            None => Ok(Manager::NotRevealed(self.pkh.clone())),
        }
    }

    fn set_manager_pk_hash_internal(
        &self,
        _host: &mut impl StorageV1,
        _public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        // In TezosX, we do not need this function which is used in Tezlink
        // only for backward compatibility.
        Ok(())
    }

    fn set_manager_public_key(
        &self,
        host: &mut impl StorageV1,
        public_key: &tezos_smart_rollup::types::PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        info.pub_key = Some(public_key.clone());
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        Ok(())
    }

    fn allocated(
        &self,
        host: &impl StorageV1,
    ) -> Result<bool, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(_)) => Ok(true),
            Ok(None) => Ok(false),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }
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

    #[test]
    fn set_manager_public_key_reveals_manager() {
        use crate::account::TezosImplicitAccount;
        use tezos_crypto_rs::{public_key::PublicKey, public_key_hash::PublicKeyHash};
        use tezos_evm_runtime::runtime::MockKernelHost;
        use tezos_execution::account_storage::{Manager, TezosImplicitAccountTrait};

        let mut host = MockKernelHost::default();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let public_key = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .unwrap();
        let path = crate::account::path_to_tezos_account(&pkh).unwrap();
        let account = TezosImplicitAccount {
            pkh: pkh.clone(),
            path,
        };

        // Reveal records the public key as the account's manager. It writes no
        // `/origin` record — an implicit account is Native by construction.
        account
            .set_manager_public_key(&mut host, &public_key)
            .unwrap();
        assert_eq!(
            account.manager(&host).unwrap(),
            Manager::Revealed(public_key)
        );
    }
}
