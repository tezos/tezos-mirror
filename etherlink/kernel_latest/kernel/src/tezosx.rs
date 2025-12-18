use mir::ast::PublicKeyHash;
use revm_etherlink::tezosx::{
    get_tezos_account_info, get_tezos_account_info_or_init, path_to_tezos_account,
    set_tezos_account_info,
};
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_data_encoding::types::Narith;
use tezos_evm_runtime::runtime::Runtime;
use tezos_execution::{
    account_storage::{
        Manager, TezlinkAccount, TezlinkOriginatedAccount,
        TezosImplicitAccount as TezosImplicitAccountTrait,
    },
    context::Context,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_host::path::{concat, OwnedPath, Path, PathError};
use tezos_storage::error::Error;

pub const TEZOS_RUNTIME_TAG: u8 = 0x01;

pub const ETHEREUM_RUNTIME_TAG: u8 = 0x02;

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

    fn balance(&self, host: &impl Runtime) -> Result<Narith, Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(u256_to_narith(&info.balance)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(Error::NomReadError(format!("{e}"))),
        }
    }

    fn set_balance(
        &self,
        host: &mut impl Runtime,
        balance: &Narith,
    ) -> Result<(), Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
        info.balance = narith_to_u256(balance)?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| Error::NomReadError(format!("{e}")))
    }
}

impl TezosImplicitAccountTrait for TezosImplicitAccount {
    fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }

    fn counter(&self, host: &impl Runtime) -> Result<Narith, Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(info)) => Ok(Narith::from(info.nonce)),
            Ok(None) => Ok(Narith::from(0u64)),
            Err(e) => Err(Error::NomReadError(format!("{e}"))),
        }
    }

    fn set_counter(
        &self,
        host: &mut impl Runtime,
        counter: &Narith,
    ) -> Result<(), Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
        info.nonce = counter
            .0
            .clone()
            .try_into()
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| Error::NomReadError(format!("{e}")))
    }

    fn manager(&self, host: &impl Runtime) -> Result<Manager, Error> {
        let info = get_tezos_account_info(host, &self.pkh)
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
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
        _host: &mut impl Runtime,
        _public_key_hash: &PublicKeyHash,
    ) -> Result<(), Error> {
        // In TezosX, we do not need this function which is used in Tezlink
        // only for backward compatibility.
        Ok(())
    }

    fn set_manager_public_key(
        &self,
        host: &mut impl Runtime,
        public_key: &tezos_smart_rollup::types::PublicKey,
    ) -> Result<(), Error> {
        let mut info = get_tezos_account_info_or_init(host, &self.pkh)
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
        info.pub_key = Some(public_key.clone());
        set_tezos_account_info(host, &self.pkh, info)
            .map_err(|e| Error::NomReadError(format!("{e}")))
    }

    fn allocated(
        &self,
        host: &impl Runtime,
    ) -> Result<bool, tezos_storage::error::Error> {
        match get_tezos_account_info(host, &self.pkh) {
            Ok(Some(_)) => Ok(true),
            Ok(None) => Ok(false),
            Err(e) => Err(tezos_storage::error::Error::NomReadError(format!("{e}"))),
        }
    }
}

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
            .map_err(|e| Error::NomReadError(format!("{e}")))?;
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

pub fn narith_to_u256(narith: &Narith) -> Result<primitive_types::U256, Error> {
    let bytes = narith.0.to_bytes_be();
    if bytes.len() <= 32 {
        Ok(primitive_types::U256::from_big_endian(&bytes))
    } else {
        Err(Error::NomReadError(
            "Narith value too large to fit in U256".to_string(),
        ))
    }
}

pub fn u256_to_narith(value: &primitive_types::U256) -> Narith {
    let mut bytes = [0u8; 32];
    value.to_big_endian(&mut bytes);
    Narith(num_bigint::BigUint::from_bytes_be(&bytes))
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::tezosx::narith_to_u256;
    use primitive_types::U256;
    use tezos_data_encoding::types::Narith;

    #[test]
    fn convert_narith_u256() {
        let narith = Narith::from(123456789u64);
        let u256_value = narith_to_u256(&narith).unwrap();
        assert_eq!(u256_value.low_u64(), 123456789u64);
    }

    #[test]
    fn convert_u256_narith() {
        let big_value = U256::from_dec_str("123456789012345678901234567890").unwrap();
        let narith_value = super::u256_to_narith(&big_value);
        let expected_narith = Narith(
            num_bigint::BigUint::from_str("123456789012345678901234567890").unwrap(),
        );
        assert_eq!(narith_value, expected_narith);
    }

    #[test]
    fn narith_u256_overflow() {
        let u256_value = U256::MAX;
        let narith_value = super::u256_to_narith(&u256_value);
        let narith_value = Narith(narith_value.0 + num_bigint::BigUint::from(1u64));
        let _ = narith_to_u256(&narith_value).expect_err("Should error on overflow");
    }
}
