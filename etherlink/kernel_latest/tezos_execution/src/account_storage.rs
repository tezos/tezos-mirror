// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos account state and storage

use crate::context;
use num_traits::ops::checked::CheckedSub;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_data_encoding::{enc::BinWriter, nom::NomReader, types::Narith};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup::{
    host::ValueType,
    types::{Contract, PublicKey, PublicKeyHash},
};
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_storage::{read_nom_value, read_optional_nom_value, store_bin};

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
    // Tag 1 is for public key before the Seoul
    // protocol and therefore is deprecated
    #[encoding(tag = 2)]
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

pub trait TezlinkAccount {
    fn path(&self) -> &OwnedPath;
    fn contract(&self) -> Contract;

    /// Get the **balance** of an account in Mutez held by the account.
    fn balance(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = concat(self.path(), &BALANCE_PATH)?;
        Ok(read_optional_nom_value(host, &path)?.unwrap_or(0_u64.into()))
    }

    /// Set the **balance** of an account in Mutez held by the account.
    fn set_balance(
        &self,
        host: &mut impl Runtime,
        balance: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &BALANCE_PATH)?;
        store_bin(balance, host, &path)
    }
}

#[derive(Debug, PartialEq)]
pub struct TezlinkImplicitAccount {
    path: OwnedPath,
    pkh: PublicKeyHash,
}

impl TezlinkAccount for TezlinkImplicitAccount {
    #[inline]
    fn path(&self) -> &OwnedPath {
        &self.path
    }
    fn contract(&self) -> Contract {
        Contract::Implicit(self.pkh.clone())
    }
}

impl TezlinkImplicitAccount {
    pub fn pkh(&self) -> &PublicKeyHash {
        &self.pkh
    }
    // We must provide the context object to get the full path in the durable storage
    pub fn from_contract(
        context: &context::Context,
        contract: &Contract,
    ) -> Result<Self, tezos_storage::error::Error> {
        match contract {
            Contract::Implicit(pkh) => Self::from_public_key_hash(context, pkh),
            _ => Err(tezos_storage::error::Error::OriginatedToImplicit),
        }
    }

    pub fn from_public_key_hash(
        context: &context::Context,
        pkh: &PublicKeyHash,
    ) -> Result<Self, tezos_storage::error::Error> {
        let index = context::contracts::index(context)?;
        let contract = Contract::Implicit(pkh.clone());
        let path = concat(&index, &account_path(&contract)?)?;
        Ok(TezlinkImplicitAccount {
            path,
            pkh: pkh.clone(),
        })
    }

    /// Get the **counter** for the Tezlink account.
    pub fn counter(
        &self,
        host: &impl Runtime,
    ) -> Result<Narith, tezos_storage::error::Error> {
        let path = concat(self.path(), &COUNTER_PATH)?;
        Ok(read_optional_nom_value(host, &path)?.unwrap_or(0_u64.into()))
    }

    /// Set the **counter** for the Tezlink account.
    pub fn set_counter(
        &self,
        host: &mut impl Runtime,
        counter: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &COUNTER_PATH)?;
        store_bin(counter, host, &path)
    }

    /// Set the **counter** for the Tezlink account to the successor of the current value.
    pub fn increment_counter(
        &self,
        host: &mut impl Runtime,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_counter(host, &Narith(self.counter(host)?.0 + 1u64))
    }

    pub fn manager(
        &self,
        host: &impl Runtime,
    ) -> Result<Manager, tezos_storage::error::Error> {
        let path = concat(self.path(), &MANAGER_PATH)?;
        let manager: Manager = read_nom_value(host, &path)?;
        Ok(manager)
    }

    pub fn set_manager_public_key_hash(
        &self,
        host: &mut impl Runtime,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(host, self.pkh())
    }

    /// This function updates the manager with a public key hash in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    fn set_manager_pk_hash_internal(
        &self,
        host: &mut impl Runtime,
        public_key_hash: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &MANAGER_PATH)?;
        // The tag for public key hash is 0 (see the Manager enum above)
        let mut buffer = vec![0_u8];
        public_key_hash
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// This function is used to test a situation in which we have an
    /// inconsistent manager pkh for an implicit account.
    #[cfg(test)]
    pub fn force_set_manager_public_key_hash(
        &self,
        host: &mut impl Runtime,
        pkh: &PublicKeyHash,
    ) -> Result<(), tezos_storage::error::Error> {
        self.set_manager_pk_hash_internal(host, pkh)
    }

    /// This function updates the manager with the public key in parameter.
    /// Most of the time, we're dealing with references so this function is here to avoid cloning
    /// the public key hash to build a [Manager] object
    pub fn set_manager_public_key(
        &self,
        host: &mut impl Runtime,
        public_key: &PublicKey,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &MANAGER_PATH)?;
        // The tag for public key is 2 (see the Manager enum above)
        let mut buffer = vec![2_u8];
        public_key
            .bin_write(&mut buffer)
            .map_err(|_| tezos_smart_rollup::host::RuntimeError::DecodingError)?;
        host.store_write_all(&path, &buffer)?;
        Ok(())
    }

    /// Allocate an account in the durable storage. Does nothing if account was
    /// already allocated.
    pub fn allocate(
        &self,
        host: &mut impl Runtime,
    ) -> Result<bool, tezos_storage::error::Error> {
        if self.allocated(host)? {
            return Ok(true);
        }
        self.set_balance(host, &0_u64.into())?;
        // TODO: use a global counter instead of initializing counter at 0
        self.set_counter(host, &0u64.into())?;
        self.set_manager_public_key_hash(host)?;
        Ok(false)
    }

    // Below this comment is multiple functions useful for validate an operation

    /// Verify if an account is allocated by attempting to read its balance
    pub fn allocated(
        &self,
        host: &impl Runtime,
    ) -> Result<bool, tezos_storage::error::Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        Ok(Some(ValueType::Value) == host.store_has(&path)?)
    }

    pub fn simulate_spending(
        &self,
        host: &impl Runtime,
        amount: &Narith,
    ) -> Result<Option<Narith>, tezos_storage::error::Error> {
        let balance = self.balance(host)?;
        let new_balance = balance
            .0
            .checked_sub(&amount.0)
            .map(|balance| balance.into());
        Ok(new_balance)
    }
}

const CODE_PATH: RefPath = RefPath::assert_from(b"/data/code");

const STORAGE_PATH: RefPath = RefPath::assert_from(b"/data/storage");

const CODE_SIZE_PATH: RefPath = RefPath::assert_from(b"/len/code");

const STORAGE_SIZE_PATH: RefPath = RefPath::assert_from(b"/len/storage");

const PAID_BYTES: RefPath = RefPath::assert_from(b"/paid_bytes");

const USED_BYTES: RefPath = RefPath::assert_from(b"/used_bytes");

#[derive(Debug, PartialEq)]
pub struct TezlinkOriginatedAccount {
    path: OwnedPath,
    kt1: ContractKt1Hash,
}

impl TezlinkAccount for TezlinkOriginatedAccount {
    #[inline]
    fn path(&self) -> &OwnedPath {
        &self.path
    }
    fn contract(&self) -> Contract {
        Contract::Originated(self.kt1.clone())
    }
}

impl TezlinkOriginatedAccount {
    pub fn kt1(&self) -> &ContractKt1Hash {
        &self.kt1
    }
    pub fn from_kt1(
        context: &context::Context,
        kt1: &ContractKt1Hash,
    ) -> Result<Self, tezos_storage::error::Error> {
        let index = context::contracts::index(context)?;
        let contract = Contract::Originated(kt1.clone());
        let path = concat(&index, &account_path(&contract)?)?;
        Ok(TezlinkOriginatedAccount {
            path,
            kt1: kt1.clone(),
        })
    }

    pub fn from_contract(
        context: &context::Context,
        contract: &Contract,
    ) -> Result<Self, tezos_storage::error::Error> {
        match contract {
            Contract::Originated(kt1) => Self::from_kt1(context, kt1),
            _ => Err(tezos_storage::error::Error::ImplicitToOriginated),
        }
    }

    pub fn code(
        &self,
        host: &impl Runtime,
    ) -> Result<Vec<u8>, tezos_storage::error::Error> {
        let path = concat(self.path(), &CODE_PATH)?;
        Ok(host.store_read_all(&path)?)
    }

    fn set_code_size(
        &self,
        host: &mut impl Runtime,
        len: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &CODE_SIZE_PATH)?;
        store_bin(len, host, &path)
    }

    pub fn set_code(
        &self,
        host: &mut impl Runtime,
        data: &[u8],
    ) -> Result<u64, tezos_storage::error::Error> {
        let path = concat(self.path(), &CODE_PATH)?;
        host.store_write_all(&path, data)?;
        let code_size = data.len() as u64;
        self.set_code_size(host, &code_size.into())?;
        Ok(code_size)
    }

    pub fn storage(
        &self,
        host: &impl Runtime,
    ) -> Result<Vec<u8>, tezos_storage::error::Error> {
        let path = concat(self.path(), &STORAGE_PATH)?;
        Ok(host.store_read_all(&path)?)
    }

    fn set_storage_size(
        &self,
        host: &mut impl Runtime,
        len: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &STORAGE_SIZE_PATH)?;
        store_bin(len, host, &path)
    }

    pub fn set_storage(
        &self,
        host: &mut impl Runtime,
        data: &[u8],
    ) -> Result<u64, tezos_storage::error::Error> {
        let path = concat(self.path(), &STORAGE_PATH)?;
        host.store_write_all(&path, data)?;
        let storage_size = data.len() as u64;
        self.set_storage_size(host, &storage_size.into())?;
        Ok(storage_size)
    }

    fn set_paid_bytes(
        &self,
        host: &mut impl Runtime,
        paid: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &PAID_BYTES)?;
        store_bin(paid, host, &path)
    }

    fn set_used_bytes(
        &self,
        host: &mut impl Runtime,
        used: &Narith,
    ) -> Result<(), tezos_storage::error::Error> {
        let path = concat(self.path(), &USED_BYTES)?;
        store_bin(used, host, &path)
    }

    pub fn init(
        &self,
        host: &mut impl Runtime,
        code: &[u8],
        storage: &[u8],
    ) -> Result<Narith, tezos_storage::error::Error> {
        // Set the smart contract code and its size
        let code_size = self.set_code(host, code)?;

        // Set the smart contract storage and its size
        let storage_size = self.set_storage(host, storage)?;

        // TODO: Set the lazy_storage
        let lazy_storage_size = 0u64;

        // TODO: Set real value for those two fields
        self.set_paid_bytes(host, &0u64.into())?;
        self.set_used_bytes(host, &0u64.into())?;

        let total_size = code_size + storage_size + lazy_storage_size;

        Ok(total_size.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tezos_crypto_rs::PublicKeyWithHash;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup::host::Runtime;
    use tezos_smart_rollup_host::path::Path;

    /// obtained by `octez-client show address bootstrap1` in mockup mode
    const BOOTSTRAP1_PKH: &str = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    const BOOTSTRAP1_PK: &str = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";

    /// obtained by `octez-codec encode alpha.contract from '"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"'`
    const BOOTSTRAP1_CONTRACT: RefPath =
        RefPath::assert_from(b"/000002298c03ed7d454a101eb7022bc95f7e5f41ac78");

    /// obtained by `PublicKey::from_b58check("edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")`
    const BOOTSTRAP1_PUBLIC_KEY_HEX: &str =
        "02004798d2cc98473d7e250c898885718afd2e4efbcb1a1595ab9730761ed830de0f";

    /// Set a key of bootstrap1 account according to the structure of a Tezos context
    /// We don't use function from [TezlinkImplicitAccount] on purpose to verify that
    /// it reads at the right path
    fn set_bootstrap1_key(host: &mut impl Runtime, value_path: &impl Path, value: &[u8]) {
        let index = RefPath::assert_from(b"/tezlink/context/contracts/index");
        let contract = concat(&index, &BOOTSTRAP1_CONTRACT)
            .expect("Concatenation should have succeeded");
        let contract_path =
            concat(&contract, value_path).expect("Concatenation should have succeeded");
        host.store_write_all(&contract_path, value).unwrap();
    }

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
        store_bin(&balance, &mut host, &path)
            .expect("Store balance should have succeeded");

        let mut balance_array = vec![];
        balance.bin_write(&mut balance_array).unwrap();
        set_bootstrap1_key(
            &mut host,
            &RefPath::assert_from(b"/balance"),
            &balance_array,
        );

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 3u64.into();

        let mut bytes = vec![];
        counter.bin_write(&mut bytes).unwrap();
        set_bootstrap1_key(&mut host, &RefPath::assert_from(b"/counter"), &bytes);

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_set_and_read_balance() {
        let mut host = MockKernelHost::default();

        let balance = 4579_u64.into();

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let () = account
            .set_balance(&mut host, &balance)
            .expect("set_balance should succeeded");

        let read_balance = account
            .balance(&host)
            .expect("read_balance should have succeeded");

        assert_eq!(balance, read_balance);
    }

    #[test]
    fn test_set_and_read_counter() {
        let mut host = MockKernelHost::default();

        let counter: Narith = 6u64.into();

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let () = account
            .set_counter(&mut host, &counter)
            .expect("set_counter should have succeeded");

        let read_counter = account
            .counter(&host)
            .expect("read_counter should have succeeded");

        assert_eq!(counter, read_counter);
    }

    #[test]
    fn test_read_manager() {
        let mut host = MockKernelHost::default();

        let public_key_hexa = hex::decode(BOOTSTRAP1_PUBLIC_KEY_HEX).unwrap();

        set_bootstrap1_key(
            &mut host,
            &RefPath::assert_from(b"/manager"),
            &public_key_hexa,
        );

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let manager = account.manager(&host).expect("Can't read manager");

        let pkh = match manager {
            Manager::NotRevealed(pkh) => {
                panic!("Manager should be revealed (manager key is a pkh: {pkh:?})")
            }
            Manager::Revealed(pk) => pk.pk_hash(),
        };

        assert_eq!(contract, Contract::Implicit(pkh));
    }

    #[test]
    fn test_set_read_manager_public_key() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for bootstrap1
        let contract = Contract::from_b58check(BOOTSTRAP1_PKH)
            .expect("Contract base58 conversion should succeeded");

        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let public_key = PublicKey::from_b58check(BOOTSTRAP1_PK).unwrap();

        let () = account
            .set_manager_public_key(&mut host, &public_key)
            .expect("set_manager_public_key should have succeeded");

        let manager = Manager::Revealed(public_key);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_set_read_manager_public_key_hash() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let () = account
            .set_manager_public_key_hash(&mut host)
            .expect("set_manager_public_key_hash should have succeeded");

        let manager = Manager::NotRevealed(pkh);

        let read_manager = account
            .manager(&host)
            .expect("read_manager should have succeeded");

        assert_eq!(manager, read_manager);
    }

    #[test]
    fn test_account_initialization() {
        let mut host = MockKernelHost::default();

        // Initialize path for Tezlink context at /tezlink/context
        let context = context::Context::init_context();

        // Create an account for bootstrap1
        let pkh = PublicKeyHash::from_b58check(BOOTSTRAP1_PKH)
            .expect("PublicKeyHash base58 conversion should succeeded");

        let contract = Contract::Implicit(pkh);
        let account = TezlinkImplicitAccount::from_contract(&context, &contract)
            .expect("Account creation should have succeeded");

        let exist = account
            .allocated(&host)
            .expect("Exist account should have succeeded");

        assert!(!exist);

        account
            .allocate(&mut host)
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
        account
            .allocate(&mut host)
            .expect("Account initialization should have succeeded");

        let read_balance = account
            .balance(&host)
            .expect("Read balance should have succeeded");

        assert_eq!(test_balance, read_balance);
    }
}
