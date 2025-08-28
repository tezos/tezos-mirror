// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023,2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{hex::FromHex, Address, Bytes, FixedBytes, B256, KECCAK_EMPTY, U256},
    state::{AccountInfo, Bytecode},
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{OwnedPath, RefPath},
    runtime::{RuntimeError, ValueType},
};
use tezos_smart_rollup_storage::storage::Storage;

use super::code::CodeStorage;
use crate::{
    custom,
    helpers::{
        legacy::FaDepositWithProxy,
        storage::{
            concat, read_b256_be_default, read_u256_be_default, read_u256_le_default,
            read_u64_le_default, write_u256_le,
        },
    },
    precompiles::constants::{
        FA_WITHDRAWAL_SOL_ADDR, FA_WITHDRAWAL_SOL_CODE_HASH, FA_WITHDRAWAL_SOL_CONTRACT,
        INTERNAL_FORWARDER_SOL_CODE_HASH, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_ADDR, WITHDRAWAL_SOL_CODE_HASH, WITHDRAWAL_SOL_CONTRACT,
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
    let path_string = format!("/{address:x}");
    OwnedPath::try_from(path_string).map_err(|err| Error::Custom(err.to_string()))
}

pub fn path_from_u256(index: &U256) -> Result<OwnedPath, Error> {
    let path_string = format!("/{}", hex::encode::<[u8; 32]>(index.to_be_bytes()));
    OwnedPath::try_from(path_string).map_err(|err| Error::Custom(err.to_string()))
}

#[inline]
fn bytecode_from_hex_str(hex_str: &str) -> Result<Bytecode, Error> {
    Ok(Bytecode::new_legacy(
        Bytes::from_hex(hex_str).map_err(custom)?,
    ))
}

struct CodeInfo {
    code: Option<Bytecode>,
    code_hash: FixedBytes<32>,
}

pub(crate) struct Ticket {
    pub(crate) path: OwnedPath,
    pub(crate) balance: U256,
}

pub struct StorageAccount {
    path: OwnedPath,
}

impl StorageAccount {
    pub fn get_account(
        host: &impl Runtime,
        world_state_handler: &mut Storage<Self>,
        address: Address,
    ) -> Result<Option<StorageAccount>, Error> {
        world_state_handler
            .get(host, &account_path(&address)?)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn get_or_create_account(
        host: &impl Runtime,
        world_state_handler: &Storage<Self>,
        address: Address,
    ) -> Result<StorageAccount, Error> {
        world_state_handler
            .get_or_create(host, &account_path(&address)?)
            .map_err(|err| Error::Custom(err.to_string()))
    }

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
                if B256::from(code_hash) == KECCAK_EMPTY {
                    return Ok(Some(Bytecode::new()));
                };
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
        let code_hash = match code {
            // There is nothing to store if there's no code or if the legacy analyzed
            // bytecode is just the STOP instruction (0x00).
            None => return Ok(()),
            Some(Bytecode::LegacyAnalyzed(bytecode))
                if bytecode.bytecode() == &Bytes::from_static(&[0]) =>
            {
                return Ok(())
            }
            Some(code) => {
                if !self.code_exists(host)? {
                    CodeStorage::add(host, code.original_byte_slice())?
                } else {
                    match self.code(host)? {
                        None => CodeStorage::add(host, code.original_byte_slice())?,
                        Some(current_code) => {
                            if code == current_code {
                                // Nothing to do.
                                return Ok(());
                            }
                            if !code.is_eip7702() {
                                // Replacement code isn't EIP-7702 and there's actively some
                                // code under `self` we can't set code.
                                return Err(Error::Custom(
                                    "Can't reset code for a non-[EIP-7702] transaction."
                                        .to_string(),
                                ));
                            }
                            if !current_code.is_eip7702() {
                                // Current code isn't EIP-7702, it's regular smart contract
                                // bytecode, we can't reset its code.
                                return Err(Error::Custom(
                                    "Can't reset code from a regular smart contract bytecode."
                                        .to_string(),
                                ));
                            }
                            // If current code is EIP-7702 and the replacement code
                            // is a re-delegation (also EIP-7702) then we delete the
                            // current code before adding the new one.
                            CodeStorage::delete(host, &self.code_hash(host)?)?;
                            CodeStorage::add(host, code.original_byte_slice())?
                        }
                    }
                }
            }
        };
        let code_hash_bytes: [u8; 32] = code_hash.into();
        let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;
        Ok(host.store_write_all(&code_hash_path, &code_hash_bytes)?)
    }

    fn fetch_optimised_code_info(&self, host: &impl Runtime) -> Result<CodeInfo, Error> {
        let raw_path = self.path.to_string();

        let code_info = if raw_path.contains(&WITHDRAWAL_SOL_ADDR.to_string()[2..]) {
            CodeInfo {
                code: Some(bytecode_from_hex_str(WITHDRAWAL_SOL_CONTRACT)?),
                code_hash: WITHDRAWAL_SOL_CODE_HASH,
            }
        } else if raw_path.contains(&FA_WITHDRAWAL_SOL_ADDR.to_string()[2..]) {
            CodeInfo {
                code: Some(bytecode_from_hex_str(FA_WITHDRAWAL_SOL_CONTRACT)?),
                code_hash: FA_WITHDRAWAL_SOL_CODE_HASH,
            }
        } else if raw_path.contains(&Address::ZERO.to_string()[2..]) {
            CodeInfo {
                code: Some(bytecode_from_hex_str(INTERNAL_FORWARDER_SOL_CONTRACT)?),
                code_hash: INTERNAL_FORWARDER_SOL_CODE_HASH,
            }
        } else {
            let code_hash = self.code_hash(host)?;
            let code = if B256::from(code_hash) == KECCAK_EMPTY {
                Some(Bytecode::new())
            } else {
                self.code(host)?
            };
            CodeInfo { code, code_hash }
        };

        Ok(code_info)
    }

    pub fn info(&self, host: &impl Runtime) -> Result<AccountInfo, Error> {
        // Optimisation: we can infer some fields of the account info based on the
        // targeted address.
        let CodeInfo { code, code_hash } = self.fetch_optimised_code_info(host)?;

        Ok(AccountInfo {
            balance: self.balance(host)?,
            nonce: self.nonce(host)?,
            code_hash,
            code,
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

    pub(crate) fn read_ticket_balance(
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

    pub fn write_ticket_balance(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<(), Error> {
        let Ticket { path, balance: _ } =
            self.read_ticket_balance(host, ticket_hash, owner)?;
        write_u256_le(host, &path, amount)?;
        Ok(())
    }

    fn deposit_path(&self, withdrawal_id: &U256) -> Result<OwnedPath, Error> {
        concat(
            &concat(&self.path, &DEPOSIT_QUEUE_TABLE)?,
            &RefPath::assert_from(format!("/{withdrawal_id}").as_bytes()),
        )
    }

    pub(crate) fn read_deposit_from_queue(
        &self,
        host: &impl Runtime,
        deposit_id: &U256,
    ) -> Result<Option<FaDepositWithProxy>, Error> {
        let deposit_path = self.deposit_path(deposit_id)?;
        let raw_deposit = host.store_read_all(&deposit_path);

        match raw_deposit {
            Ok(bytes) => FaDepositWithProxy::from_raw(bytes).map(Some),
            Err(RuntimeError::PathNotFound) => Ok(None),
            Err(err) => Err(Error::Custom(err.to_string())),
        }
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

#[cfg(test)]
mod test {
    use crate::precompiles::constants::{
        FA_WITHDRAWAL_SOL_ADDR, FA_WITHDRAWAL_SOL_CODE_HASH, FA_WITHDRAWAL_SOL_CONTRACT,
        INTERNAL_FORWARDER_SOL_CODE_HASH, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_ADDR, WITHDRAWAL_SOL_CODE_HASH, WITHDRAWAL_SOL_CONTRACT,
    };

    use super::{bytecode_from_hex_str, CodeInfo, StorageAccount};
    use revm::{
        primitives::{Address, FixedBytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};

    fn check_account_code_info_fetching(
        host: &impl Runtime,
        storage_account: StorageAccount,
        code_voucher: Bytecode,
        code_hash_voucher: FixedBytes<32>,
    ) {
        let CodeInfo { code, code_hash } =
            storage_account.fetch_optimised_code_info(host).unwrap();

        assert_eq!(Some(code_voucher), code);
        assert_eq!(code_hash_voucher, code_hash);
    }

    #[test]
    fn check_withdrawal_code_info_fetching() {
        let host = MockKernelHost::default();
        let storage_account = StorageAccount::from_address(&WITHDRAWAL_SOL_ADDR).unwrap();
        let code_voucher = bytecode_from_hex_str(WITHDRAWAL_SOL_CONTRACT).unwrap();

        check_account_code_info_fetching(
            &host,
            storage_account,
            code_voucher,
            WITHDRAWAL_SOL_CODE_HASH,
        );
    }

    #[test]
    fn check_fa_withdrawal_code_info_fetching() {
        let host = MockKernelHost::default();
        let storage_account =
            StorageAccount::from_address(&FA_WITHDRAWAL_SOL_ADDR).unwrap();
        let code_voucher = bytecode_from_hex_str(FA_WITHDRAWAL_SOL_CONTRACT).unwrap();

        check_account_code_info_fetching(
            &host,
            storage_account,
            code_voucher,
            FA_WITHDRAWAL_SOL_CODE_HASH,
        );
    }

    #[test]
    fn check_internal_forwarder_code_info_fetching() {
        let host = MockKernelHost::default();
        let storage_account = StorageAccount::from_address(&Address::ZERO).unwrap();
        let code_voucher =
            bytecode_from_hex_str(INTERNAL_FORWARDER_SOL_CONTRACT).unwrap();

        check_account_code_info_fetching(
            &host,
            storage_account,
            code_voucher,
            INTERNAL_FORWARDER_SOL_CODE_HASH,
        );
    }

    #[test]
    fn check_empty_account_code_info_fetching() {
        let host = MockKernelHost::default();
        let storage_account =
            StorageAccount::from_address(&Address(FixedBytes::new([1; 20]))).unwrap();
        let code_voucher = Bytecode::new();

        check_account_code_info_fetching(
            &host,
            storage_account,
            code_voucher,
            KECCAK_EMPTY,
        );
    }
}
