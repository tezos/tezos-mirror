// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Transaction kernel storage
//!
//! The TX kernel allows for batching operations into transactions. If any
//! operation in a transaction fails, the effect of the entire transaction
//! is rolled back. This module allows for both dealing with transactions
//! and the updates to accounts that happen within said transactions.

use crypto::hash::ContractTz1Hash;
use crypto::hash::HashType;
use crypto::hash::PublicKeyEd25519;
use num_bigint::{BigInt, TryFromBigIntError};
use tezos_smart_rollup_encoding::michelson::ticket::StringTicket;
use tezos_smart_rollup_encoding::michelson::ticket::{TicketHash, TicketHashError};
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_smart_rollup_host::runtime::RuntimeError::*;
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};
use tezos_smart_rollup_host::Error::*;
use tezos_smart_rollup_storage::storage::Storage;
use thiserror::Error;

extern crate alloc;

/// The sub-path for holding the account id value.
const ID_PATH: RefPath = RefPath::assert_from(b"/id");

/// All errors that may happen as result of using this account storage
/// interface.
#[derive(Error, Debug)]
pub enum AccountStorageError {
    /// Tried to take amount of ticket from an account, but the account
    /// does not hold enough funds for this operation.
    #[error("Account does not hold enough funds {0:?}: stored={1}, required={2}")]
    NotEnoughFunds(OwnedPath, u64, u64),
    /// Some runtime error happened while using the hosts durable storage.
    #[error("Runtime error")]
    RuntimeError(tezos_smart_rollup_host::runtime::RuntimeError),
    /// Some error happened when constructing the path to some resource
    /// associated with an account.
    #[error("Path error")]
    PathError(tezos_smart_rollup_host::path::PathError),
    /// Some value (amount or counter) was kept in a malformed format in
    /// durable storage.
    #[error("Malformed 64 bit integer in storage")]
    MalformedValue,
    /// Some signature in storage had a malformed format.
    #[error("Malformed compressed BLS signature")]
    MalformedSignature,
    /// Some error happened in the storage API when creating the account
    /// storage object.
    #[error("Storage API error")]
    StorageError(tezos_smart_rollup_storage::StorageError),
    /// Adding a ticket to an account overflowed an unsigned 64 bit
    /// integer
    #[error("Ticket amount overflow")]
    AmountOverflow,
    /// A 64 bit counter for an account overflowed.
    #[error("Counter overflow")]
    CounterOverflow,
    /// An account counter value was expected of certain value, but another
    /// value was found.
    #[error("Counter expected to be {0}, but {1} was found")]
    CounterMismatch(i64, i64),
    /// Failed to identify a string ticket, ie, could not generate the hash
    /// of a ticket string representation.
    #[error("Could not generate ticket hash")]
    TicketIdentity(TicketHashError),
    /// Invalid Ticket repr
    #[error("ticket amount out of range {0}")]
    InvalidAmount(TryFromBigIntError<BigInt>),
}

impl From<tezos_smart_rollup_storage::StorageError> for AccountStorageError {
    fn from(error: tezos_smart_rollup_storage::StorageError) -> Self {
        AccountStorageError::StorageError(error)
    }
}

impl From<tezos_smart_rollup_host::path::PathError> for AccountStorageError {
    fn from(error: tezos_smart_rollup_host::path::PathError) -> Self {
        AccountStorageError::PathError(error)
    }
}

impl From<tezos_smart_rollup_host::runtime::RuntimeError> for AccountStorageError {
    fn from(error: tezos_smart_rollup_host::runtime::RuntimeError) -> Self {
        AccountStorageError::RuntimeError(error)
    }
}

impl From<TicketHashError> for AccountStorageError {
    fn from(error: TicketHashError) -> Self {
        AccountStorageError::TicketIdentity(error)
    }
}

impl From<TryFromBigIntError<BigInt>> for AccountStorageError {
    fn from(error: TryFromBigIntError<BigInt>) -> Self {
        AccountStorageError::InvalidAmount(error)
    }
}

/// A TX kernel account
///
/// The account is stored in `path` held by the Account structure (private
/// field). Sub paths include:
/// - the `counter` field. A 64 bit signed integer
/// - the `public_key` field. A 48 byte comressed BLS public key.
/// - a sub path for each ticket held by the account. Ticket paths are
///   named by the hex encoded hash of the ticket. Each such path holds
///   the amount of such tickets held by the account encoded as unsigned
///   64 bit integers.
///
/// If a path is absent (counter- or ticket- path), then the value defaults
/// to zero. If the public key path doesn't contain anything, it is given as
/// `None` - otherwise it will contain "Some" compressed BLS public key.
#[derive(Debug, PartialEq)]
pub struct Account {
    path: OwnedPath,
}

/// The sub-path for holding the account counter value.
const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");

/// The sub-path for the accounts public key.
const PUBLIC_KEY_PATH: RefPath = RefPath::assert_from(b"/public.key");

/// Find the path where account information is stored from the account address.
pub fn account_path(address: &ContractTz1Hash) -> Result<OwnedPath, AccountStorageError> {
    let address_name: Vec<u8> = alloc::format!("/{}", address).into();
    OwnedPath::try_from(address_name).map_err(AccountStorageError::from)
}

impl Account {
    /// Get the full path to where the amount of a specific ticket for the account
    /// is stored.
    fn ticket_path(&self, ticket: u64) -> Result<OwnedPath, AccountStorageError> {
        let sth_path = OwnedPath::try_from(format!("/{ticket}"))?;

        concat(&self.path, &sth_path).map_err(AccountStorageError::from)
    }

    /// Get amount of given ticket held by the account.
    ///
    /// If there is not value in durable storage for this account and the
    /// ticket identified by the hash, then the value defaults to zero.
    pub fn ticket_amount<Host: Runtime>(
        &self,
        host: &Host,
        ticket: u64,
    ) -> Result<Option<u64>, AccountStorageError> {
        let path = self.ticket_path(ticket)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => {
                let value = u64::from_le_bytes(buffer);
                Ok(if value != 0_u64 { Some(value) } else { None })
            }
            Err(PathNotFound | HostErr(StoreNotAValue)) => Ok(None),
            _ => Err(AccountStorageError::MalformedValue),
        }
    }

    /// Get or set the account id
    pub fn get_or_set_id(&mut self, host: &mut impl Runtime) -> Result<u16, AccountStorageError> {
        let path = concat(&self.path, &ID_PATH)?;

        let mut buffer = [0_u8; 2];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(2) => {
                let value = u16::from_le_bytes(buffer);
                Ok(value)
            }
            _ => {
                let next_id = next_account_id(host)?;
                host.store_write(&path, &next_id.to_le_bytes(), 0)?;
                Ok(next_id)
            }
        }
    }

    /// Take number of tickets from account
    ///
    /// This function is made to match the use case of transactions. TX kernel
    /// transactions are given as unsigned 64 bit integers for the amount transferred,
    /// so such transactions require two calls: one for the debit and one for the credit).
    /// This function is for the credit.
    pub fn remove_ticket<Host: Runtime>(
        &mut self,
        host: &mut Host,
        ticket: u64,
        amount: u64,
    ) -> Result<u64, AccountStorageError> {
        let path = self.ticket_path(ticket)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => {
                let old_amount = u64::from_le_bytes(buffer);

                match old_amount.checked_sub(amount) {
                    Some(0) => {
                        host.store_delete(&path)
                            .map_err(AccountStorageError::from)?;
                        Ok(0)
                    }
                    Some(new_amount) => {
                        host.store_write(&path, &new_amount.to_le_bytes(), 0)
                            .map_err(AccountStorageError::from)?;
                        Ok(new_amount)
                    }
                    None => Err(AccountStorageError::NotEnoughFunds(
                        path.clone(),
                        old_amount,
                        amount,
                    )),
                }
            }
            Err(PathNotFound | HostErr(StoreNotAValue)) => {
                if amount != 0 {
                    Err(AccountStorageError::NotEnoughFunds(
                        path.clone(),
                        0_u64,
                        amount,
                    ))
                } else {
                    Ok(0)
                }
            }
            Err(error) => Err(AccountStorageError::from(error)),
            Ok(_) => Err(AccountStorageError::MalformedValue),
        }
    }

    /// Give number of tickets to the account
    ///
    /// This function is made to match the use case of transactions. TX kernel
    /// transactions are given as unsigned 64 bit integers for the amount transferred,
    /// so such transactions require two calls: one for the debit and one for the
    /// credit). This function is for the debit.
    pub fn add_ticket<Host: Runtime>(
        &mut self,
        host: &mut Host,
        ticket_index: u64,
        amount: u64,
    ) -> Result<u64, AccountStorageError> {
        let path = self.ticket_path(ticket_index)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => {
                let old_amount = u64::from_le_bytes(buffer);

                if let Some(new_value) = old_amount.checked_add(amount) {
                    host.store_write(&path, &new_value.to_le_bytes(), 0)
                        .map_err(AccountStorageError::from)?;
                    Ok(new_value)
                } else {
                    Err(AccountStorageError::AmountOverflow)
                }
            }
            Err(PathNotFound | HostErr(StoreNotAValue)) => {
                host.store_write(&path, &amount.to_le_bytes(), 0)
                    .map_err(AccountStorageError::from)?;
                Ok(amount)
            }
            Ok(_) => Err(AccountStorageError::MalformedValue),
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }

    /// Get the current counter value for the account
    ///
    /// If the store doesn't hold a counter value for the account,
    /// then the counter value is assumed to be zero.
    pub fn counter<Host: Runtime>(&self, host: &Host) -> Result<i64, AccountStorageError> {
        let path = concat(&self.path, &COUNTER_PATH)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => Ok(i64::from_le_bytes(buffer)),
            Ok(_) => Err(AccountStorageError::MalformedValue),
            Err(PathNotFound | HostErr(StoreNotAValue)) => Ok(0_i64),
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }

    /// Increment the counter for the account
    ///
    /// If the store doesn't hold a value for the counter it is assumed to be
    /// zero before increment, so it will be stored as one after this call. Otherwise
    /// increment the value stored by one. This may overflow, at least in theory.
    pub fn increment_counter<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &COUNTER_PATH)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => {
                let old_value = i64::from_le_bytes(buffer);
                if let Some(new_value) = old_value.checked_add(1) {
                    host.store_write(&path, &new_value.to_le_bytes(), 0)
                        .map_err(AccountStorageError::from)
                } else {
                    Err(AccountStorageError::CounterOverflow)
                }
            }
            Ok(_) => Err(AccountStorageError::MalformedValue),
            Err(PathNotFound | HostErr(StoreNotAValue)) => host
                .store_write(&path, &(1_i64).to_le_bytes(), 0)
                .map_err(AccountStorageError::from),
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }

    /// Check counter and increment it, if check passes
    ///
    /// This is a use case function, that both increments and checks the counter.
    /// It only increments the counter if the check passes. This is a use case for
    /// the TX kernel.
    pub fn check_and_inc_counter<Host: Runtime>(
        &mut self,
        host: &mut Host,
        counter: i64,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &COUNTER_PATH)?;

        let mut buffer = [0_u8; 8];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(8) => {
                let old_value = i64::from_le_bytes(buffer);

                if old_value != counter {
                    return Err(AccountStorageError::CounterMismatch(counter, old_value));
                }

                if let Some(new_value) = old_value.checked_add(1) {
                    host.store_write(&path, &new_value.to_le_bytes(), 0)
                        .map_err(AccountStorageError::from)
                } else {
                    Err(AccountStorageError::CounterOverflow)
                }
            }
            Ok(_) => Err(AccountStorageError::MalformedValue),
            Err(PathNotFound | HostErr(StoreNotAValue)) => {
                if counter != 0_i64 {
                    return Err(AccountStorageError::CounterMismatch(counter, 0_i64));
                }

                host.store_write(&path, &(1_i64).to_le_bytes(), 0)
                    .map_err(AccountStorageError::from)
            }
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }

    /// Link a public key to the account
    ///
    /// In case the account already has a key, the new key will replace it.
    /// No check is performed to see if the public key corresponds to the
    /// account address.
    pub fn link_public_key<Host: Runtime>(
        &mut self,
        host: &mut Host,
        pk: &PublicKeyEd25519,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &PUBLIC_KEY_PATH)?;

        host.store_write(&path, &pk.0, 0)
            .map_err(AccountStorageError::from)
    }

    /// Disassociate a public key from an account
    ///
    /// In case the account has no key associated with it, nothing happens.
    /// Othwerise any previously created link will be removed.
    pub fn unlink_public_key<Host: Runtime>(
        &mut self,
        host: &mut Host,
    ) -> Result<(), AccountStorageError> {
        let path = concat(&self.path, &PUBLIC_KEY_PATH)?;

        match host.store_delete(&path) {
            Ok(_) => Ok(()),
            Err(PathNotFound | HostErr(StoreNotAValue)) => Ok(()),
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }

    /// Get the public key for an account if it exists
    ///
    /// An account doesn't necessarily have a public key associated with it.
    pub fn public_key<Host: Runtime>(
        &self,
        host: &Host,
    ) -> Result<Option<PublicKeyEd25519>, AccountStorageError> {
        const PK_SIZE: usize = HashType::PublicKeyEd25519.size();
        let path = concat(&self.path, &PUBLIC_KEY_PATH)?;

        let mut buffer = [0_u8; PK_SIZE];

        match host.store_read_slice(&path, 0, &mut buffer) {
            Ok(PK_SIZE) => Ok(Some(PublicKeyEd25519(buffer.to_vec()))),
            Ok(_) => Err(AccountStorageError::MalformedSignature),
            Err(PathNotFound | HostErr(StoreNotAValue)) => Ok(None),
            Err(error) => Err(AccountStorageError::from(error)),
        }
    }
}

impl From<OwnedPath> for Account {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

/// Record ticket
pub fn get_or_set_ticket_id(
    host: &mut impl Runtime,
    ticket: &TicketHash,
) -> Result<u64, AccountStorageError> {
    const SIZE: usize = std::mem::size_of::<u64>();
    let path = OwnedPath::try_from(format!("/tickets/{ticket}/id"))?;

    let mut buffer = [0_u8; SIZE];

    match host.store_read_slice(&path, 0, &mut buffer) {
        Ok(SIZE) => {
            let value = u64::from_le_bytes(buffer);
            Ok(value)
        }
        _ => {
            let next_id = next_ticket_id(host)?;
            host.store_write(&path, &next_id.to_le_bytes(), 0)?;
            Ok(next_id)
        }
    }
}

/// Path where world state of accounts are stored
const ACCOUNTS_PATH: RefPath = RefPath::assert_from(b"/accounts");

/// The type of storage used for TX kernel accounts.
pub type AccountStorage = Storage<Account>;

/// Get the initial storage interface for accounts
pub fn init_account_storage() -> Result<AccountStorage, AccountStorageError> {
    Storage::<Account>::init(&ACCOUNTS_PATH).map_err(AccountStorageError::from)
}

/// Deposit a ticket to the Rollup
///
/// Besides keeping track of how many of each ticket each account holds, we also
/// keep track of how many of each ticket the rollup holds as a whole. The sum
/// of tickets held by accounts should equal the number of tickets held by the
/// entire rollup.
///
/// This function both deposits the ticket to the account _and_ adds the amount
/// to the total rollup ledger (last part is still TODO).
pub fn deposit_ticket_to_storage<Host: Runtime>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
    destination: &ContractTz1Hash,
    ticket: &StringTicket,
) -> Result<(), AccountStorageError> {
    let path = account_path(destination)?;

    let mut account = account_storage.get_or_create(host, &path)?;
    let ticket_hash = ticket.hash()?;
    let amount: u64 = ticket.amount_as()?;

    let ticket_id = get_or_set_ticket_id(host, &ticket_hash)?;

    account.add_ticket(host, ticket_id, amount)?;
    account.get_or_set_id(host)?;

    #[cfg(feature = "debug")]
    tezos_smart_rollup_debug::debug_msg!(host, "Depositing {ticket:?} to {destination}\n");

    Ok(())
}

/// Current acount counter
fn next_account_id(host: &mut impl Runtime) -> Result<u16, RuntimeError> {
    const NEXT_ID: RefPath = RefPath::assert_from(b"/next_account_id");

    let mut buffer = [0_u8; 2];

    let _ = host.store_read_slice(&NEXT_ID, 0, &mut buffer);
    let value = u16::from_le_bytes(buffer);
    host.store_write(&NEXT_ID, &(value + 1).to_le_bytes(), 0)?;
    Ok(value)
}

/// Current ticket counter
fn next_ticket_id(host: &mut impl Runtime) -> Result<u64, RuntimeError> {
    const NEXT_ID: RefPath = RefPath::assert_from(b"/next_ticket_id");

    let mut buffer = [0_u8; 8];

    let _ = host.store_read_slice(&NEXT_ID, 0, &mut buffer);
    let value = u64::from_le_bytes(buffer);
    host.store_write(&NEXT_ID, &(value + 1).to_le_bytes(), 0)?;
    Ok(value)
}

/// A name space for DAL storage related functions
pub mod dal {
    use super::concat;
    use super::Error;
    use super::OwnedPath;
    use super::RefPath;
    use super::Runtime;
    use tezos_smart_rollup_host::runtime::ValueType;

    /// All errors that may happen as result of using this DAL storage
    /// interface.
    #[derive(Error, Debug)]
    pub enum StorageError {
        /// Some runtime error happened while using the hosts durable storage.
        #[error("Runtime error")]
        RuntimeError(tezos_smart_rollup_host::runtime::RuntimeError),
        /// Some error happened when constructing the path to some resource
        /// associated with an account.
        #[error("Path error")]
        PathError(tezos_smart_rollup_host::path::PathError),
    }

    impl From<tezos_smart_rollup_host::path::PathError> for StorageError {
        fn from(error: tezos_smart_rollup_host::path::PathError) -> Self {
            StorageError::PathError(error)
        }
    }

    impl From<tezos_smart_rollup_host::runtime::RuntimeError> for StorageError {
        fn from(error: tezos_smart_rollup_host::runtime::RuntimeError) -> Self {
            StorageError::RuntimeError(error)
        }
    }

    const BASE_PATH: RefPath<'static> = RefPath::assert_from(b"/dal/parameters");

    fn slot_index_path() -> OwnedPath {
        let suffix = RefPath::assert_from(b"/slot_index");
        concat(&BASE_PATH, &suffix).expect("slot index path too long")
    }

    fn number_of_slots_path() -> OwnedPath {
        let suffix = RefPath::assert_from(b"/number_of_slots");
        concat(&BASE_PATH, &suffix).expect("number of slots path too long")
    }

    fn page_size_path() -> OwnedPath {
        let suffix = RefPath::assert_from(b"/page_size");
        concat(&BASE_PATH, &suffix).expect("page size path too long")
    }

    fn slot_size_path() -> OwnedPath {
        let suffix = RefPath::assert_from(b"/slot_size");
        concat(&BASE_PATH, &suffix).expect("slot size path too long")
    }

    fn attestation_lag_path() -> OwnedPath {
        let suffix = RefPath::assert_from(b"/attestation_lag");
        concat(&BASE_PATH, &suffix).expect("attestation lag path too long")
    }

    /// Set
    fn set<T, U>(host: &mut impl Runtime, path: &OwnedPath, value: T) -> Result<(), StorageError>
    where
        T: num_traits::ops::bytes::ToBytes<Bytes = U>,
        U: TryInto<Vec<u8>>,
        <U as TryInto<Vec<u8>>>::Error: std::fmt::Debug,
    {
        host.store_write(
            path,
            &value
                .to_le_bytes()
                .try_into()
                .expect("Error while writing DAL parameter"),
            0,
        )?;
        Ok(())
    }

    /// Get
    fn get<T, U>(host: &mut impl Runtime, path: &OwnedPath) -> Result<T, StorageError>
    where
        U: TryFrom<Vec<u8>>,
        <U as TryFrom<Vec<u8>>>::Error: std::fmt::Debug,
        T: num_traits::ops::bytes::FromBytes<Bytes = U>,
    {
        host.store_read(path, 0, std::mem::size_of::<T>())
            .map(|vec| {
                T::from_le_bytes(&(vec.try_into().expect("Error while reading DAL parameter")))
            })
            .map_err(StorageError::from)
    }

    /// Set slot index
    pub fn set_slot_index(host: &mut impl Runtime, slot_index: u8) -> Result<(), StorageError> {
        set(host, &slot_index_path(), slot_index)
    }

    /// Get slot index
    pub fn get_or_set_slot_index(host: &mut impl Runtime, default: u8) -> Result<u8, StorageError> {
        let path = slot_index_path();
        if let Ok(Some(ValueType::Value)) = host.store_has(&path) {
            get(host, &path)
        } else {
            set(host, &path, default)?;
            Ok(default)
        }
    }

    /// Set number of slots
    pub fn set_number_of_slots(
        host: &mut impl Runtime,
        number_of_slots: u16,
    ) -> Result<(), StorageError> {
        set(host, &number_of_slots_path(), number_of_slots)
    }

    /// Get number of slots
    pub fn get_number_of_slots(host: &mut impl Runtime) -> Result<u16, StorageError> {
        get(host, &number_of_slots_path())
    }

    /// Set page size
    pub fn set_page_size(host: &mut impl Runtime, page_size: u16) -> Result<(), StorageError> {
        set(host, &page_size_path(), page_size)
    }

    /// Get page size
    pub fn get_page_size(host: &mut impl Runtime) -> Result<u16, StorageError> {
        get(host, &page_size_path())
    }

    /// Set slot size
    pub fn set_slot_size(host: &mut impl Runtime, slot_size: u32) -> Result<(), StorageError> {
        set(host, &slot_size_path(), slot_size)
    }

    /// Get slot size
    pub fn get_slot_size(host: &mut impl Runtime) -> Result<u32, StorageError> {
        get(host, &slot_size_path())
    }

    /// Attestation lag
    pub fn set_attestation_lag(
        host: &mut impl Runtime,
        attestation_lag: u16,
    ) -> Result<(), StorageError> {
        set(host, &attestation_lag_path(), attestation_lag)
    }

    /// Get attestation lag
    pub fn get_attestation_lag(host: &mut impl Runtime) -> Result<u16, StorageError> {
        get(host, &attestation_lag_path())
    }
}

#[cfg(test)]
mod test {
    use crate::inbox::external::testing::gen_ed25519_keys;
    use crate::storage::init_account_storage;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_mock::MockHost;

    #[test]
    fn test_account_counter_update() {
        let mut host = MockHost::default();
        let mut storage = init_account_storage().expect("Could not create accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        assert_eq!(
            a1.counter(&host)
                .expect("Could not get counter for account"),
            0_i64
        );

        a1.increment_counter(&mut host)
            .expect("Could not increment counter");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.counter(&host)
                .expect("Could not get counter for account"),
            1_i64
        );
    }

    #[test]
    fn test_account_ticket_deposit() {
        let mut host = MockHost::default();
        let mut storage = init_account_storage().expect("Could not create accounts storage API");

        let a1_path = RefPath::assert_from(b"/kjfds");
        let ticket_id = 5;

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists in storage");

        assert_eq!(
            a1.ticket_amount(&host, ticket_id)
                .expect("Could not get ticket amount"),
            None
        );

        a1.add_ticket(&mut host, ticket_id, 10)
            .expect("Could not give ticket to account");

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert
        let a1 = storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("Account does not exist");

        assert_eq!(
            a1.ticket_amount(&host, ticket_id)
                .expect("Could not get ticket amount"),
            Some(10)
        );
    }

    #[test]
    fn test_account_ticket_fund_limit() {
        let mut host = MockHost::default();
        let mut storage = init_account_storage().expect("Could not create accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let a2_path = RefPath::assert_from(b"/fdsa");
        let ticket_id = 10;

        // Act
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        let mut a1 = storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        let mut a2 = storage
            .create_new(&mut host, &a2_path)
            .expect("Could not create new account")
            .expect("Account already exists");

        a1.add_ticket(&mut host, ticket_id, 10)
            .expect("Could not give 10 tickets to account");
        a2.add_ticket(&mut host, ticket_id, 20)
            .expect("Could not give 20 tickets to account");

        // Assert
        a1.remove_ticket(&mut host, ticket_id, 11).expect_err(
            "Wasn't supposed to be able to take 11 tickets from an account that has only 10",
        );

        // Act some more...
        storage
            .rollback_transaction(&mut host)
            .expect("Could not rollback transaction");

        // Assert again
        let a1 = storage.get(&host, &a1_path).expect("Could not get account");

        assert_eq!(a1, None);

        let a2 = storage.get(&host, &a2_path).expect("Could not get account");

        assert_eq!(a2, None);
    }

    #[test]
    fn test_public_key() {
        let mut host = MockHost::default();
        let mut storage = init_account_storage().expect("Could not create accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let (a1_public_key, _) = gen_ed25519_keys();

        // Make sure there is an account
        storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create account")
            .expect("No account created")
            .increment_counter(&mut host)
            .expect("Could not increment counter of new account");

        // Assert that there is no public key initially
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Error when getting public key for account"),
            None
        );

        // Associate a public key with the account
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("No account found")
            .link_public_key(&mut host, &a1_public_key)
            .expect("Could not link public key to account");

        // Assert that public key has been updated
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Could not get public key from storage"),
            Some(a1_public_key.clone())
        );

        storage
            .commit_transaction(&mut host)
            .expect("Could not commit transaction");

        // Assert that public key update has been committed
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Could not get public key from storage"),
            Some(a1_public_key.clone())
        );
    }

    #[test]
    fn test_public_key_rollback() {
        let mut host = MockHost::default();
        let mut storage = init_account_storage().expect("Could not create accounts storage API");

        let a1_path = RefPath::assert_from(b"/asdf");
        let (a1_public_key, _) = gen_ed25519_keys();

        // Make sure there is an account
        storage
            .create_new(&mut host, &a1_path)
            .expect("Could not create account")
            .expect("No account created")
            .increment_counter(&mut host)
            .expect("Could not increment counter of new account");

        // Assert that there is no public key initially
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Error when getting public key for account"),
            None
        );

        // Associate a public key with the account
        storage
            .begin_transaction(&mut host)
            .expect("Could not begin transaction");

        storage
            .get(&host, &a1_path)
            .expect("Could not get account")
            .expect("No account found")
            .link_public_key(&mut host, &a1_public_key)
            .expect("Could not link public key to account");

        // Assert that public key has been updated
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Could not get public key from storage"),
            Some(a1_public_key.clone())
        );

        storage
            .rollback_transaction(&mut host)
            .expect("Could not rollback transaction");

        // Assert that public key update has been committed
        assert_eq!(
            storage
                .get(&host, &a1_path)
                .expect("Could not read account")
                .expect("No account found")
                .public_key(&host)
                .expect("Could not get public key from storage"),
            None
        );
    }
}
