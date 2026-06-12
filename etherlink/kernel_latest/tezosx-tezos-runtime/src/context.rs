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
    get_origin_at, path_to_tezos_account, set_origin_at, TezosImplicitAccount,
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

    fn record_origin(
        &self,
        host: &mut impl StorageV1,
        kt1: &ContractKt1Hash,
        origin: &Origin,
    ) -> Result<(), tezos_storage::error::Error> {
        // Origination is the only writer of the origin path for a
        // freshly created KT1, so write the given origin unconditionally.
        let originated = self.originated_from_kt1(kt1)?;
        let path = originated.path().clone();
        set_origin_at(host, &path, origin)
            .map_err(|e| tezos_storage::error::Error::TcError(format!("{e}")))
    }

    fn read_origin_for_address(
        &self,
        host: &impl StorageV1,
        address: &AddressHash,
    ) -> Result<Option<Origin>, tezos_storage::error::Error> {
        match address {
            // A tz1/2/3 is a public-key hash: it is intrinsically Tezos-native
            // and can never be an alias (aliases are materialized as KT1
            // forwarders). Its classification is therefore [Origin::Native] by
            // construction, with no durable read and no stored `/origin` record.
            AddressHash::Implicit(_) => Ok(Some(Origin::Native)),
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
    use crate::account::get_origin_at;
    use tezos_crypto_rs::blake2b;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_evm_runtime::safe_storage::ETHERLINK_SAFE_STORAGE_ROOT_PATH;
    use tezosx_interfaces::{AliasInfo, RuntimeId};

    #[test]
    fn record_origin_writes_given_origin_for_kt1() {
        let mut host = MockKernelHost::default();
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"some-test-seed"));

        let originated = context.originated_from_kt1(&kt1).unwrap();
        let path = originated.path().clone();

        // Before origination, no classification.
        assert!(get_origin_at(&host, &path).unwrap().is_none());

        context
            .record_origin(&mut host, &kt1, &Origin::Native)
            .unwrap();

        // After origination, Native.
        assert_eq!(get_origin_at(&host, &path).unwrap(), Some(Origin::Native));

        // record_origin with Alias overwrites with Alias.
        let alias = Origin::Alias(AliasInfo {
            runtime: RuntimeId::Ethereum,
            native_address: b"0xfeedface".to_vec(),
        });
        context.record_origin(&mut host, &kt1, &alias).unwrap();
        assert_eq!(get_origin_at(&host, &path).unwrap(), Some(alias));
    }

    #[test]
    fn read_origin_for_address_implicit_is_native_by_construction() {
        let host = MockKernelHost::default();
        let context =
            TezosRuntimeContext::from_root(&ETHERLINK_SAFE_STORAGE_ROOT_PATH).unwrap();
        let pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let address = AddressHash::Implicit(pkh);

        // An implicit account is Native by construction: classification needs
        // no durable read and stores no `/origin` record.
        assert_eq!(
            context.read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
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
        context
            .record_origin(&mut host, &kt1, &Origin::Native)
            .unwrap();
        assert_eq!(
            context.read_origin_for_address(&host, &address).unwrap(),
            Some(Origin::Native),
        );
    }

    /// End-to-end check that the entrypoints RPC (`get_contract_entrypoint`),
    /// which goes through `code()`, resolves a code-less alias KT1 to the
    /// *real* shared forwarder and exposes its `default : unit` entrypoint.
    ///
    /// Uses `alias_forwarder::forwarder_code()` rather than a stand-in script,
    /// so it pins the actual entrypoint surface that ships.
    #[test]
    fn alias_entrypoints_resolve_to_real_forwarder() {
        use crate::alias_forwarder::forwarder_code;
        use mir::ast::{Entrypoint, Type};
        use mir::gas::Gas;
        use tezos_crypto_rs::blake2b;
        use tezos_execution::account_storage::write_alias_implementation;
        use tezos_execution::get_contract_entrypoint;
        use tezosx_interfaces::{AliasInfo, RuntimeId};

        let mut host = MockKernelHost::default();
        // Root at the production Michelson accounts path: aliases resolve under
        // `/tez/tez_accounts` (lib.rs:626) and the slot path is absolute, so this
        // pins the real composition rather than a mixed layout that never ships.
        let context = TezosRuntimeContext::from_root(
            &crate::TEZ_TEZ_ACCOUNTS_SAFE_STORAGE_ROOT_PATH,
        )
        .unwrap();

        // Seed the shared slot with the real forwarder code.
        write_alias_implementation(&mut host, &forwarder_code().unwrap()).unwrap();

        // A code-less KT1 classified as an alias (no /data/code written).
        let kt1 = ContractKt1Hash::from(blake2b::digest_160(b"alias-entrypoint-test"));
        context
            .record_origin(
                &mut host,
                &kt1,
                &Origin::Alias(AliasInfo {
                    runtime: RuntimeId::Ethereum,
                    native_address: b"0xabc".to_vec(),
                }),
            )
            .unwrap();

        // The entrypoints RPC resolves through code() to the shared forwarder.
        let entrypoints = get_contract_entrypoint(
            &host,
            &context,
            &AddressHash::Kt1(kt1),
            &mut Gas::default(),
        )
        .expect("alias entrypoints resolve to the shared forwarder");
        assert_eq!(entrypoints.get(&Entrypoint::default()), Some(&Type::Unit));
    }

    /// Upgrading the real shipped forwarder to a superset that keeps
    /// `default : unit` and `storage string` is accepted. Pins the production
    /// storage type and default surface against the upgrade invariants (the
    /// `upgrade_*` tests in `tezos_execution` use toy `storage unit` scripts;
    /// this lives here because `tezos_execution` cannot reach `forwarder_code`).
    #[test]
    fn upgrade_real_forwarder_to_superset_is_accepted() {
        use crate::alias_forwarder::forwarder_code;
        use mir::gas::Gas;
        use tezos_execution::account_storage::{
            read_alias_implementation, write_alias_implementation,
        };
        use tezos_execution::upgrade_alias_implementation;

        let mut host = MockKernelHost::default();
        write_alias_implementation(&mut host, &forwarder_code().unwrap()).unwrap();

        // Keep `default : unit` (annotated `%default`) and `storage string`,
        // add a `foo` entrypoint.
        let superset = mir::parser::Parser::new()
            .parse_top_level(
                "parameter (or (unit %default) (string %foo)); storage string; \
                 code { CDR; NIL operation; PAIR }",
            )
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();

        upgrade_alias_implementation(&mut host, &superset, &mut Gas::default())
            .expect("a superset of the real forwarder must be accepted");
        assert_eq!(read_alias_implementation(&host).unwrap(), Some(superset));
    }
}
