// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos account state and storage

use crate::context;
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::{
    host::ValueType,
    types::{Contract, PublicKey, PublicKeyHash},
};
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_storage::{read_nom_value, read_optional_nom_value, store_bin};

#[derive(Debug, PartialEq)]
pub struct TezlinkImplicitAccount {
    path: OwnedPath,
}

impl From<OwnedPath> for TezlinkImplicitAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

// This enum is inspired of `src/proto_alpha/lib_protocol/manager_repr.ml`
// A manager can be:
//  - a public key, it means that the account is revealed
//  - a public key hash, account is not yet revealed but the
//    reveal public key will match this public key hash
#[derive(Debug, PartialEq, Eq, BinWriter, NomReader)]
#[encoding(tags = "u8")]
pub enum Manager {
    #[encoding(tag = 0)]
    NotRevealed(PublicKeyHash),
    #[encoding(tag = 1)]
    Revealed(PublicKey),
}

const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");

const MANAGER_PATH: RefPath = RefPath::assert_from(b"/manager");

fn account_path(contract: &Contract) -> Result<OwnedPath, tezos_storage::error::Error> {
    // uses the same encoding as in the octez node's representation of the context
    // see `octez-codec describe alpha.contract binary schema`
    let mut contract_encoded = Vec::new();
    contract
        .bin_write(&mut contract_encoded)
        .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;

    let path_string = alloc::format!("/{}", hex::encode(&contract_encoded));
    Ok(OwnedPath::try_from(path_string)?)
}

impl TezlinkImplicitAccount {
    // We must provide the context object to get the full path in the durable storage
    #[allow(dead_code)]
    pub fn from_contract(
        context: &context::Context,
        contract: &Contract,
    ) -> Result<Self, tezos_storage::error::Error> {
        let index = context::contracts::index(context)?;
        let path = concat(&index, &account_path(contract)?)?;
        Ok(path.into())
    }

    /// Get the **counter** for the Tezlink account.
    #[allow(dead_code)]
    pub fn counter(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = concat(&self.path, &COUNTER_PATH)?;
        Ok(read_optional_nom_value(host, &path)?.unwrap_or(0_u64.into()))
    }

    /// Set the **counter** for the Tezlink account.
    #[allow(dead_code)]
    pub fn set_counter(
        &mut self,
        host: &mut impl Runtime,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &COUNTER_PATH)?;
        store_bin(counter, host, &path)
    }

    /// Get the **balance** of an account in Mutez held by the account.
    #[allow(dead_code)]
    pub fn balance(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        Ok(read_optional_nom_value(host, &path)?.unwrap_or(0_u64.into()))
    }

    /// Set the **balance** of an account in Mutez held by the account.
    #[allow(dead_code)]
    pub fn set_balance(
        &mut self,
        host: &mut impl Runtime,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        store_bin(balance, host, &path)
    }

    #[allow(dead_code)]
    pub fn manager(
        &self,
        host: &impl Runtime,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let path = concat(&self.path, &MANAGER_PATH)?;
        let manager: Manager = read_nom_value(host, &path)?;
        Ok(manager)
    }

    /// This function updates the manager with a public key hash in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    pub fn set_manager_public_key_hash(
        &mut self,
        host: &mut impl Runtime,
        public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &MANAGER_PATH)?;
        // The tag for public key hash is 0 (see the Manager enum above)
        let mut buffer = vec![0_u8];
        public_key_hash
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// This function updates the manager with the public key in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    pub fn set_manager_public_key(
        &mut self,
        host: &mut impl Runtime,
        public_key: &PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(&self.path, &MANAGER_PATH)?;
        // The tag for public key hash is 1 (see the Manager enum above)
        let mut buffer = vec![1_u8];
        public_key
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// Verify if an account is allocated by attempting to read its balance
    pub fn allocated(
        &self,
        host: &impl Runtime,
    ) -> Result<bool, tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        Ok(Some(ValueType::Value) == host.store_has(&path)?)
    }

    /// Allocate an account in the durable storage. Does nothing if account was
    /// already allocated.
    pub fn allocate(
        host: &mut impl Runtime,
        context: &context::Context,
        contract: &Contract,
    ) -> Result<(), tezos_storage::error::Error> {
        let mut account = Self::from_contract(context, contract)?;
        if account.allocated(host)? {
            return Ok(());
        }
        account.set_balance(host, &0_u64.into())?;
        // Only implicit accounts have counter and manager keys
        if let Contract::Implicit(pkh) = contract {
            // TODO: use a global counter instead of initializing counter at 0
            account.set_counter(host, &0u64.into())?;
            account.set_manager_public_key_hash(host, pkh)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tezos_crypto_rs::PublicKeyWithHash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::host::Runtime;

    // Read test use hard coded path on purpose to verify the Tezos compatibility.
    // These paths comes from the context.json generated by the create mockup command
    // of octez-client.
    #[test]
    fn test_read_balance() {
        let mut host = MockKernelHost::default();

        let balance: Narith = 2944_u64.into();
        // octez-codec decode alpha.contract from '000002298c03ed7d454a101eb7022bc95f7e5f41ac78'
        // Result: "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
        let path = RefPath::assert_from(b"/tezlink/context/contracts/index/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/balance");
        store_bin(&balance, &mut host, &path).expect("Store balance should have succeed");

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Public key hash in b58 for 000002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeed");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 3u64.into();
        // octez-codec decode alpha.contract from '000002298c03ed7d454a101eb7022bc95f7e5f41ac78'
        // Result: "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"
        let path = RefPath::assert_from(b"/tezlink/context/contracts/index/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/counter");
        store_bin(&counter, &mut host, &path)
            .expect("Store counter should have succeeded");

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Public key hash in b58 for 0002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeed");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_set_and_read_balance() {
        let mut host = MockKernelHost::default();

        let balance = 4579_u64.into();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Public key hash in b58 for 000002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let () = account
            .set_balance(&mut host, &balance)
            .expect("set_balance should succeed");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeed");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_set_and_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 6u64.into();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Public key hash in b58 for 0002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let () = account
            .set_counter(&mut host, &counter)
            .expect("set_counter should have succeed");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeed");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_read_manager() {
        let mut host = MockKernelHost::default();

        // Set the manager in a hard coded way
        // For the public key I used the rust code to have it
        // PublicKey::from_b58check(
        //     "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        // )
        // .unwrap()
        // Result: 01004798d2cc98473d7e250c898885718afd2e4efbcb1a1595ab9730761ed830de0f
        let path = RefPath::assert_from(b"/tezlink/context/contracts/index/000002298c03ed7d454a101eb7022bc95f7e5f41ac78/manager");
        let public_key_hexa = hex::decode(
            "01004798d2cc98473d7e250c898885718afd2e4efbcb1a1595ab9730761ed830de0f",
        )
        .unwrap();
        host.store_write_all(&path, &public_key_hexa).unwrap();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Public key hash in b58 for 0002298c03ed7d454a101eb7022bc95f7e5f41ac78
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let manager = account.manager(&host).expect("Can't read manager");

        let pkh = match manager {
            Manager::NotRevealed(pkh) => panic!(
                "Manager should be revealed (manager key is a pkh: {:?})",
                pkh
            ),
            Manager::Revealed(pk) => pk.pk_hash(),
        };

        assert_eq!(contract, Contract::Implicit(pkh));
    }

    #[test]
    fn test_set_read_manager_public_key() {
        let mut host = MockKernelHost::default();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
        let contract = Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("Contract base58 conversion should succeed");

        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let public_key = PublicKey::from_b58check(
            "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        )
        .unwrap();

        let () = account
            .set_manager_public_key(&mut host, &public_key)
            .expect("set_manager_public_key shoud have succeed");

        let manager = Manager::Revealed(public_key);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeed");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_set_read_manager_public_key_hash() {
        let mut host = MockKernelHost::default();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
        let pkh = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash base58 conversion should succeed");

        let contract = Contract::Implicit(pkh);
        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeed");

        let pkh = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash base58 conversion should succeed");

        let () = account
            .set_manager_public_key_hash(&mut host, &pkh)
            .expect("set_manager_public_key_hash shoud have succeed");

        let manager = Manager::NotRevealed(pkh);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeed");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_account_initialization() {
        let mut host = MockKernelHost::default();

        // Initalize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
        let pkh = PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let mut account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let exist = account
            .allocated(&host)
            .expect("Exist account should have succeeded");

        assert!(!exist);

        TezlinkImplicitAccount::allocate(&mut host, &context, &contract)
            .expect("Account initialization should have succeeded");

        let exist = account
            .allocated(&host)
            .expect("Exist account should have succeeded");

        assert!(exist);

        let test_balance = 1999_u64.into();

        account
            .set_balance(&mut host, &test_balance)
            .expect("Set balance should have succeeded");

        // Calling init on a contract already initialized will do nothing
        // So the balance should not change and still be 1999
        TezlinkImplicitAccount::allocate(&mut host, &context, &contract)
            .expect("Account initialization should have succeeded");

        let read_balance = account
            .balance(&host)
            .expect("Read balance should have succeeded");

        assert_eq!(test_balance, read_balance);
    }
}
