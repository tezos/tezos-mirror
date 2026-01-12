// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::{hash::ContractKt1Hash, public_key_hash::PublicKeyHash};
use tezos_execution::{account_storage::TezlinkOriginatedAccount, context::Context};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError};

use crate::account::{path_to_tezos_account, TezosImplicitAccount};

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

    fn from_root(root: &impl Path) -> Result<Self, PathError> {
        Ok(Self {
            path: root.to_owned().into(),
        })
    }

    fn path(&self) -> OwnedPath {
        self.path.clone()
    }
}
