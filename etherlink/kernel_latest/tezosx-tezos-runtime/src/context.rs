// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::AddressHash;
use tezos_crypto_rs::{hash::ContractKt1Hash, public_key_hash::PublicKeyHash};
use tezos_execution::{
    account_storage::{TezlinkAccount, TezlinkOriginatedAccount},
    context::Context,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError};
use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Origin;

use crate::account::{
    get_origin_at, get_origin_for_implicit, path_to_tezos_account, set_origin_at,
    TezosImplicitAccount,
};

pub struct TezosRuntimeContext {
    path: OwnedPath,
}

impl Context for TezosRuntimeContext {
    type ImplicitAccountType = TezosImplicitAccount;

    fn implicit_from_public_key_hash(
        &self,
        pkh: &PublicKeyHash,
    ) -> Result<Self::ImplicitAccountType, tezos_storage::error::Error> {
        let path = path_to_tezos_account(pkh)
            .map_err(|e| tezos_storage::error::Error::NomReadError(format!("{e}")))?;
        Ok(TezosImplicitAccount {
            path,
            pkh: pkh.clone(),
        })
    }

    // For now we do not have a special originated account in TezosX
    type OriginatedAccountType = TezlinkOriginatedAccount;

    fn originated_from_kt1(
        &self,
        kt1: &ContractKt1Hash,
    ) -> Result<Self::OriginatedAccountType, tezos_storage::error::Error> {
        let index = tezos_execution::context::contracts::index(self)?;
        let contract = Contract::Originated(kt1.clone());
        let path = concat(
            &index,
            &tezos_execution::context::account::account_path(&contract)?,
        )?;
        Ok(TezlinkOriginatedAccount {
            path,
            kt1: kt1.clone(),
        })
    }

    fn record_native_origin(
        &self,
        host: &mut impl StorageV1,
        kt1: &ContractKt1Hash,
    ) -> Result<(), tezos_storage::error::Error> {
        // Origination is the only writer of the origin path for a
        // freshly created KT1, so write Native unconditionally.
        let originated = self.originated_from_kt1(kt1)?;
        let path = originated.path().clone();
        set_origin_at(host, &path, &Origin::Native)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
    }

    fn read_origin_for_address(
        &self,
        host: &impl StorageV1,
        address: &AddressHash,
    ) -> Result<Option<Origin>, tezos_storage::error::Error> {
        match address {
            AddressHash::Implicit(pkh) => get_origin_for_implicit(host, pkh)
                .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}"))),
            AddressHash::Kt1(kt1) => {
                let originated = self.originated_from_kt1(kt1)?;
                let path = originated.path().clone();
                get_origin_at(host, &path)
                    .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
            }
            AddressHash::Sr1(_) => Ok(None),
        }
    }

    fn from_root(root: &impl Path) -> Result<Self, PathError> {
        Ok(Self {
            path: root.to_owned().into(),
        })
    }

    fn path(&self) -> OwnedPath {
        self.path.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account::{get_origin_at, set_origin_for_implicit};
    use tezos_crypto_rs::blake2b;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
    use tezosx_interfaces::{AliasInfo, RuntimeId};

    #[test]
    fn record_native_origin_writes_native_for_kt1() {
        let mut host = MockKernelHost::default();
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"some-test-seed"));

        let originated = context.originated_from_kt1(&kt1).unwrap();
        let path = originated.path().clone();

        // Before origination, no classification.
        assert!(get_origin_at(&host, &path).unwrap().is_none());

        context.record_native_origin(&mut host, &kt1).unwrap();

        // After origination, Native.
        assert_eq!(get_origin_at(&host, &path).unwrap(), Some(Origin::Native));
    }

    #[test]
    fn read_origin_for_address_implicit_round_trips_classification() {
        let mut host = MockKernelHost::default();
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let address = AddressHash::Implicit(pkh.clone());

        // Unrecorded: returns None.
        assert!(context
            .read_origin_for_address(&host, &address)
            .unwrap()
            .is_none());

        // Native: returns Native.
        set_origin_for_implicit(&mut host, &pkh, &Origin::Native).unwrap();
        assert_eq!(
            context.read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
        );

        // Alias: returns Alias with the same payload.
        let alias = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: b"0xabcdef".to_vec(),
        });
        set_origin_for_implicit(&mut host, &pkh, &alias).unwrap();
        assert_eq!(
            context.read_origin_for_address(&host, &address).unwrap(),
            Some(alias),
        );
    }

    #[test]
    fn read_origin_for_address_kt1_round_trips_classification() {
        let mut host = MockKernelHost::default();
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"kt1-test-seed"));
        let address = AddressHash::Kt1(kt1.clone());

        // Unrecorded: returns None.
        assert!(context
            .read_origin_for_address(&host, &address)
            .unwrap()
            .is_none());

        // Native: returns Native.
        context.record_native_origin(&mut host, &kt1).unwrap();
        assert_eq!(
            context.read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
        );
    }
}
