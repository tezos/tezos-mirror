// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023,2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, B256, KECCAK_EMPTY, U256},
    state::{AccountInfo, Bytecode},
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{OwnedPath, RefPath},
    runtime::{RuntimeError, ValueType},
};
use tezos_smart_rollup_storage::storage::Storage;

use crate::{
    code_storage::CodeStorage,
    helpers::legacy::FaDepositWithProxy,
    helpers::{
        concat, read_b256_be_default, read_u256_be_default, read_u256_le_default,
        read_u64_le_default, write_u256_le,
    },
    Error,
};

/// Path where EVM accounts are stored.
pub const EVM_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts");

/// Path where the L1 address of our withdrawals ticketer is stored.
pub(crate) const WITHDRAWALS_TICKETER_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/ticketer");

/// Path where an account nonce is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const NONCE_PATH: RefPath = RefPath::assert_from(b"/nonce");

/// Path where an account balance, ether held, is stored. This should be prefixed with the path to
/// where the account is stored for the world state or for the current transaction.
const BALANCE_PATH: RefPath = RefPath::assert_from(b"/balance");

/// "Internal" accounts - accounts with contract code have a contract code hash.
/// This value is computed when the code is stored and kept for future queries. This
/// path should be prefixed with the path to where the account is stored for the world
/// state or for the current transaction.
const CODE_HASH_PATH: RefPath = RefPath::assert_from(b"/code.hash");

/// "Internal" accounts - accounts with contract code, have their code stored here.
/// This path should be prefixed with the path to where the account is stored for the
/// world state or for the current transaction.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// The contracts of "internal" accounts have their own storage area. The account
/// location prefixed to this path gives the root path (prefix) to where such storage
/// values are kept. Each index in durable storage gives one complete path to one
/// such 256 bit integer value in storage.
const STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/storage");

/// Path where global ticket table is stored.
const TICKET_STORAGE_PATH: RefPath = RefPath::assert_from(b"/ticket_table");

/// Path where global deposit table is stored.
const DEPOSIT_QUEUE_TABLE: RefPath = RefPath::assert_from(b"/deposits_table");

/// If a contract tries to read a value from storage and it has previously not written
/// anything to this location or if it wrote the default value, then it gets this
/// value back.
const STORAGE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default balance value for an account.
const BALANCE_DEFAULT_VALUE: U256 = U256::ZERO;

/// Default nonce value for an account.
const NONCE_DEFAULT_VALUE: u64 = 0;

pub fn account_path(address: &Address) -> Result<OwnedPath, Error> {
    let path_string = format!("/{:x}", address);
    OwnedPath::try_from(path_string).map_err(|err| Error::Custom(err.to_string()))
}

pub fn path_from_u256(index: &U256) -> Result<OwnedPath, Error> {
    let path_string = format!("/{}", hex::encode::<[u8; 32]>(index.to_be_bytes()));
    OwnedPath::try_from(path_string).map_err(|err| Error::Custom(err.to_string()))
}

struct Ticket {
    path: OwnedPath,
    balance: U256,
}

pub struct StorageAccount {
    path: OwnedPath,
}

impl StorageAccount {
    pub fn from_address(address: &Address) -> Result<Self, Error> {
        let path = concat(&EVM_ACCOUNTS_PATH, &account_path(address)?)?;
        Ok(path.into())
    }

    pub fn balance(&self, host: &impl Runtime) -> Result<U256, Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        Ok(read_u256_le_default(host, &path, BALANCE_DEFAULT_VALUE)?)
    }

    pub fn set_balance(
        &mut self,
        host: &mut impl Runtime,
        new_balance: U256,
    ) -> Result<(), Error> {
        let path = concat(&self.path, &BALANCE_PATH)?;
        Ok(host.store_write_all(&path, &new_balance.to_le_bytes::<{ U256::BYTES }>())?)
    }

    pub fn nonce(&self, host: &impl Runtime) -> Result<u64, Error> {
        let path = concat(&self.path, &NONCE_PATH)?;
        read_u64_le_default(host, &path, NONCE_DEFAULT_VALUE)
    }

    pub fn set_nonce(
        &mut self,
        host: &mut impl Runtime,
        nonce: u64,
    ) -> Result<(), Error> {
        let path = concat(&self.path, &NONCE_PATH)?;

        let value_bytes: [u8; 8] = nonce.to_le_bytes();

        Ok(host.store_write_all(&path, &value_bytes)?)
    }

    pub fn code_hash(&self, host: &impl Runtime) -> Result<B256, Error> {
        let path = concat(&self.path, &CODE_HASH_PATH)?;
        Ok(read_b256_be_default(host, &path, KECCAK_EMPTY)?)
    }

    pub fn code_exists(&self, host: &impl Runtime) -> Result<bool, Error> {
        let path = concat(&self.path, &CODE_HASH_PATH)?;

        match host.store_has(&path) {
            Ok(Some(ValueType::Value | ValueType::ValueWithSubtree)) => Ok(true),
            Ok(Some(ValueType::Subtree) | None) => Ok(false),
            Err(err) => Err(Error::Runtime(err)),
        }
    }

    pub fn code(&self, host: &impl Runtime) -> Result<Option<Bytecode>, Error> {
        let path = concat(&self.path, &CODE_PATH)?;

        match host.store_read_all(&path) {
            Ok(bytes) => {
                // NB: [new_raw_checked] here is great as it's backward-compatible
                // but also future proof. It will check if the decoded bytes is
                // a legacy code, an EIP-7702 EOA's code or an EOF code.
                Ok(Some(
                    Bytecode::new_raw_checked(Bytes::from(bytes))
                        .map_err(|_| RuntimeError::DecodingError)?,
                ))
            }
            Err(RuntimeError::PathNotFound) => {
                let code_hash = self.code_hash(host)?;
                let code_storage = CodeStorage::new(&code_hash)?;
                Ok(Some(code_storage.get_code(host)?))
            }
            Err(err) => Err(Error::Runtime(err)),
        }
    }

    pub fn set_code(
        &mut self,
        host: &mut impl Runtime,
        code: Option<Bytecode>,
    ) -> Result<(), Error> {
        if self.code_exists(host)? {
            Ok(())
        } else {
            let code_hash = match code {
                // There is nothing to store if there's no code or if the legacy analyzed
                // bytecode is just the STOP instruction (0x00).
                None => return Ok(()),
                Some(Bytecode::LegacyAnalyzed(bytecode))
                    if bytecode.bytecode() == &Bytes::from_static(&[0]) =>
                {
                    return Ok(())
                }
                Some(code) => CodeStorage::add(host, code.original_byte_slice())?,
            };
            let code_hash_bytes: [u8; 32] = code_hash.into();
            let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;
            Ok(host.store_write_all(&code_hash_path, &code_hash_bytes)?)
        }
    }

    pub fn info(&self, host: &impl Runtime) -> Result<AccountInfo, Error> {
        Ok(AccountInfo {
            balance: self.balance(host)?,
            nonce: self.nonce(host)?,
            code_hash: self.code_hash(host)?,
            code: self.code(host)?,
        })
    }

    pub fn set_info(
        &mut self,
        host: &mut impl Runtime,
        new_infos: AccountInfo,
    ) -> Result<(), Error> {
        let AccountInfo {
            balance,
            nonce,
            code,
            ..
        } = new_infos;

        self.set_balance(host, balance)?;
        self.set_nonce(host, nonce)?;
        self.set_code(host, code)?;

        Ok(())
    }

    fn delete_code(
        &mut self,
        host: &mut impl Runtime,
        code_hash: &B256,
    ) -> Result<(), Error> {
        if code_hash != &KECCAK_EMPTY {
            CodeStorage::delete(host, code_hash)?;
            let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;
            if host.store_has(&code_hash_path)?.is_some() {
                host.store_delete(&code_hash_path)?
            }
        }
        Ok(())
    }

    pub fn clear_info(
        &mut self,
        host: &mut impl Runtime,
        code_hash: &B256,
    ) -> Result<(), Error> {
        // If nothing was ever stored state-wise, we have nothing
        // to clear, it means the storage account was created and
        // destructed within the same transaction.
        if host.store_has(&self.path)?.is_some() {
            self.set_balance(host, U256::ZERO)?;
            self.set_nonce(host, 0)?;
            self.delete_code(host, code_hash)?;
        }
        Ok(())
    }

    pub fn storage_path(&self, index: &U256) -> Result<OwnedPath, Error> {
        let storage_path = concat(&self.path, &STORAGE_ROOT_PATH)?;
        let index_path = path_from_u256(index)?;
        concat(&storage_path, &index_path)
    }

    pub fn get_storage(&self, host: &impl Runtime, index: &U256) -> Result<U256, Error> {
        let path = self.storage_path(index)?;
        Ok(read_u256_be_default(host, &path, STORAGE_DEFAULT_VALUE)?)
    }

    pub fn set_storage(
        &mut self,
        host: &mut impl Runtime,
        index: &U256,
        value: &U256,
    ) -> Result<(), Error> {
        let path = self.storage_path(index)?;
        let value_bytes = value.to_be_bytes::<{ U256::BYTES }>();

        Ok(host.store_write_all(&path, &value_bytes)?)
    }

    pub fn clear_storage(&mut self, host: &mut impl Runtime) -> Result<(), Error> {
        let path = concat(&self.path, &STORAGE_ROOT_PATH)?;
        if host.store_has(&path)?.is_some() {
            host.store_delete(&path)?
        }
        Ok(())
    }

    fn read_ticket_balance(
        &self,
        host: &impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<Ticket, Error> {
        let path = concat(
            &self.path,
            &concat(
                &TICKET_STORAGE_PATH,
                &concat(&path_from_u256(ticket_hash)?, &account_path(owner)?)?,
            )?,
        )?;
        let balance = read_u256_le_default(host, &path, U256::ZERO)?;
        Ok(Ticket { path, balance })
    }

    pub fn ticket_balance_add(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error> {
        if amount.is_zero() {
            return Ok(false);
        }

        let Ticket { path, balance } =
            self.read_ticket_balance(host, ticket_hash, owner)?;
        if let Some(new_balance) = balance.checked_add(amount) {
            write_u256_le(host, &path, new_balance)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn ticket_balance_remove(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<bool, Error> {
        if amount.is_zero() {
            return Ok(false);
        }

        let Ticket { path, balance } =
            self.read_ticket_balance(host, ticket_hash, owner)?;
        if let Some(new_balance) = balance.checked_sub(amount) {
            write_u256_le(host, &path, new_balance)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn deposit_path(&self, withdrawal_id: &U256) -> Result<OwnedPath, Error> {
        concat(
            &concat(&self.path, &DEPOSIT_QUEUE_TABLE)?,
            &RefPath::assert_from(format!("/{}", withdrawal_id).as_bytes()),
        )
    }

    pub(crate) fn read_deposit_from_queue(
        &self,
        host: &impl Runtime,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, Error> {
        let deposit_path = self.deposit_path(deposit_id)?;
        let raw_deposit = host.store_read_all(&deposit_path)?;
        FaDepositWithProxy::from_raw(raw_deposit)
    }

    pub(crate) fn remove_deposit_from_queue(
        &self,
        host: &mut impl Runtime,
        deposit_id: &U256,
    ) -> Result<(), Error> {
        let deposit_path = self.deposit_path(deposit_id)?;
        Ok(host.store_delete(&deposit_path)?)
    }
}

impl From<OwnedPath> for StorageAccount {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

pub type WorldStateHandler = Storage<StorageAccount>;

pub fn new_world_state_handler() -> Result<WorldStateHandler, Error> {
    Storage::<StorageAccount>::init(&EVM_ACCOUNTS_PATH)
        .map_err(|err| Error::Custom(err.to_string()))
}
