// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025-2026 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, B256, KECCAK_EMPTY, U256},
    state::{AccountInfo, Bytecode},
};
use rlp::{Decodable, Encodable, Rlp};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{OwnedPath, RefPath},
    runtime::RuntimeError,
};

use crate::{
    custom,
    helpers::{
        legacy::FaDepositWithProxy,
        storage::{
            concat, read_b256_be_default, read_u256_be_default, read_u256_le_default,
            read_u64_le_default, write_u256_le,
        },
    },
    storage::code::CodeStorage,
    Error,
};

/// Path where EVM accounts are stored.
pub const EVM_ACCOUNTS_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_accounts");

/// Path where the L1 address of our withdrawals ticketer is stored.
pub const NATIVE_TOKEN_TICKETER_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/ticketer");

/// Size of contract hash encoded in base58.
pub const KT1_B58_SIZE: usize = 36;

/// Path where a possible waiting sequencer upgrade triggered by a precompile
/// is store.
pub const SEQUENCER_KEY_CHANGE_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/sequencer_key_change");

/// Path where waiting governance sequencer upgrades are stored. (Must be read through `internal_store_read_all`)
pub const GOVERNANCE_SEQUENCER_UPGRADE_PATH: RefPath =
    RefPath::assert_from(b"/evm/sequencer_upgrade");

/// Path where the sequencer key is stored. (Must be read through `internal_store_read_all`)
pub const SEQUENCER_KEY_PATH: RefPath = RefPath::assert_from(b"/evm/sequencer");

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

/// Path where all the infos of a contract are stored in the same key.
/// This path must contains balance, nonce and code hash. This is the new
/// format for saving the accounts infos that overrides the previous one.
const INFO_PATH: RefPath = RefPath::assert_from(b"/info");

/// The contracts of "internal" accounts have their own storage area. The account
/// location prefixed to this path gives the root path (prefix) to where such storage
/// values are kept. Each index in durable storage gives one complete path to one
/// such 256 bit integer value in storage.
const STORAGE_ROOT_PATH: RefPath = RefPath::assert_from(b"/storage");

/// Path where global counter is stored.
const GLOBAL_COUNTER_PATH: RefPath = RefPath::assert_from(b"/withdrawal_counter");

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
    OwnedPath::try_from(path_string).map_err(custom)
}

pub fn path_from_u256(index: &U256) -> Result<OwnedPath, Error> {
    let path_string = format!("/{}", hex::encode::<[u8; 32]>(index.to_be_bytes()));
    OwnedPath::try_from(path_string).map_err(custom)
}

pub struct StorageAccount {
    path: OwnedPath,
}

// Used as a value for the durable storage, can't use REVM `AccountInfo`
// because we need to implement `RlpEncodable` `RlpDecodable`
// TODO: Remove pub when `evm_execution` doesn't use it anymore.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq)]
pub struct AccountInfoInternal {
    pub balance: U256,
    pub nonce: u64,
    pub code_hash: B256,
}

impl AccountInfoInternal {
    // This value is used for optimizing reads in the durable storage,
    // don't change it without changing StorageAccount::info.
    const RLP_SIZE: usize = 77;
}

impl Encodable for AccountInfoInternal {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(3);
        s.append(&self.balance.to_le_bytes::<32>().as_slice());
        s.append(&self.nonce.to_le_bytes().as_slice());
        s.append(&self.code_hash.0.as_slice());
    }
}

impl Decodable for AccountInfoInternal {
    fn decode(rlp: &Rlp) -> Result<Self, rlp::DecoderError> {
        if !rlp.is_list() {
            Err(rlp::DecoderError::RlpExpectedToBeList)
        } else {
            let mut it = rlp.iter();
            let balance: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;
            let nonce: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;
            let code_hash: Vec<u8> = it
                .next()
                .ok_or(rlp::DecoderError::RlpExpectedToBeList)?
                .as_val()?;

            Ok(Self {
                balance: U256::from_le_bytes::<32>(
                    balance.try_into().map_err(|_| {
                        rlp::DecoderError::Custom("Invalid balance length")
                    })?,
                ),
                nonce: u64::from_le_bytes(
                    nonce
                        .try_into()
                        .map_err(|_| rlp::DecoderError::Custom("Invalid nonce length"))?,
                ),
                code_hash: B256::from_slice(&code_hash),
            })
        }
    }
}

impl From<AccountInfoInternal> for AccountInfo {
    fn from(info: AccountInfoInternal) -> Self {
        AccountInfo {
            balance: info.balance,
            nonce: info.nonce,
            code_hash: info.code_hash,
            account_id: None,
            code: None,
        }
    }
}

impl From<AccountInfo> for AccountInfoInternal {
    fn from(info: AccountInfo) -> Self {
        AccountInfoInternal {
            balance: info.balance,
            nonce: info.nonce,
            code_hash: info.code_hash,
        }
    }
}

impl StorageAccount {
    pub fn from_address(address: &Address) -> Result<Self, Error> {
        let path = concat(&EVM_ACCOUNTS_PATH, &account_path(address)?)?;
        Ok(path.into())
    }

    pub fn from_path(path: OwnedPath) -> Self {
        Self { path }
    }

    pub fn info(&self, host: &mut impl Runtime) -> Result<AccountInfo, Error> {
        let path = concat(&self.path, &INFO_PATH)?;
        match host.store_read(&path, 0, AccountInfoInternal::RLP_SIZE) {
            Ok(bytes) => {
                let account_info = AccountInfoInternal::decode(&Rlp::new(&bytes))
                    .map_err(|_| RuntimeError::DecodingError)?;
                Ok(account_info.into())
            }
            Err(RuntimeError::PathNotFound) => {
                // If we don't have the informations inside of `INFO_PATH` it's either :
                // - We don't have the account created yet
                // - The account is stored in the old format (each field in a different key)
                // In the last case we need to migrate the account to the new format
                // We are not verifying if the keys where existing before for code readibility,
                // this code will run only once for an old address so the overhead is minimal.

                let balance_path = concat(&self.path, &BALANCE_PATH)?;
                let nonce_path = concat(&self.path, &NONCE_PATH)?;
                let code_hash_path = concat(&self.path, &CODE_HASH_PATH)?;
                let code_path = concat(&self.path, &CODE_PATH)?;

                let info = AccountInfoInternal {
                    balance: read_u256_le_default(
                        host,
                        &balance_path,
                        BALANCE_DEFAULT_VALUE,
                    )?,
                    nonce: read_u64_le_default(host, &nonce_path, NONCE_DEFAULT_VALUE)?,
                    code_hash: read_b256_be_default(host, &code_hash_path, KECCAK_EMPTY)?,
                };

                if info == AccountInfoInternal::default() {
                    // Account doesn't exist
                    return Ok(AccountInfo::default());
                }

                // Write migration
                match host.store_read_all(&code_path) {
                    Ok(bytes) => {
                        CodeStorage::add(
                            host,
                            Bytecode::new_raw_checked(Bytes::from(bytes))
                                .map_err(|_| RuntimeError::DecodingError)?
                                .original_byte_slice(),
                            Some(info.code_hash),
                        )?;
                    }
                    Err(RuntimeError::PathNotFound) => (),
                    Err(err) => return Err(Error::Runtime(err)),
                };
                host.store_write(&path, &info.rlp_bytes(), 0)?;

                // Delete legacy account entries
                for path in &[balance_path, nonce_path, code_hash_path, code_path] {
                    match host.store_delete(path) {
                        Ok(()) | Err(RuntimeError::PathNotFound) => (),
                        Err(err) => return Err(Error::Runtime(err)),
                    };
                }

                Ok(info.into())
            }
            Err(err) => Err(Error::Runtime(err)),
        }
    }

    pub fn info_without_migration(
        &self,
        host: &impl Runtime,
    ) -> Result<Option<AccountInfo>, Error> {
        let path = concat(&self.path, &INFO_PATH)?;
        match host.store_read(&path, 0, AccountInfoInternal::RLP_SIZE) {
            Ok(bytes) => {
                let account_info = AccountInfoInternal::decode(&Rlp::new(&bytes))
                    .map_err(|_| RuntimeError::DecodingError)?;
                Ok(Some(account_info.into()))
            }
            Err(RuntimeError::PathNotFound) => Ok(None),
            Err(err) => Err(Error::Runtime(err)),
        }
    }

    pub fn set_info(
        &mut self,
        host: &mut impl Runtime,
        mut new_infos: AccountInfo,
    ) -> Result<(), Error> {
        let path = concat(&self.path, &INFO_PATH)?;
        if let Some(code) = new_infos.code.take() {
            CodeStorage::add(
                host,
                code.original_byte_slice(),
                Some(new_infos.code_hash),
            )?;
        }
        let value = AccountInfoInternal::from(new_infos).rlp_bytes();

        host.store_write(&path, &value, 0)?;
        Ok(())
    }

    pub fn set_info_without_code(
        &mut self,
        host: &mut impl Runtime,
        new_infos: AccountInfo,
    ) -> Result<(), Error> {
        let path = concat(&self.path, &INFO_PATH)?;
        let value = AccountInfoInternal::from(new_infos).rlp_bytes();

        host.store_write(&path, &value, 0)?;
        Ok(())
    }

    pub fn delete_info(&mut self, host: &mut impl Runtime) -> Result<(), Error> {
        let path = concat(&self.path, &INFO_PATH)?;
        match host.store_delete(&path) {
            Ok(()) | Err(RuntimeError::PathNotFound) => (),
            Err(err) => return Err(Error::Runtime(err)),
        };
        Ok(())
    }

    // In the future we might want to optimize reading to not use `info`.
    pub fn add_balance(
        &mut self,
        host: &mut impl Runtime,
        amount: U256,
    ) -> Result<(), Error> {
        let mut info = self.info(host)?;
        info.balance = info
            .balance
            .checked_add(amount)
            .ok_or(Error::Custom("Balance overflow".to_string()))?;
        self.set_info(host, info)
    }

    // In the future we might want to optimize reading to not use `info`.
    pub fn sub_balance(
        &mut self,
        host: &mut impl Runtime,
        amount: U256,
    ) -> Result<(), Error> {
        let mut info = self.info(host)?;
        info.balance = info
            .balance
            .checked_sub(amount)
            .ok_or(Error::Custom("Balance underflow".to_string()))?;
        self.set_info(host, info)
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

        Ok(host.store_write(&path, &value_bytes, 0)?)
    }

    pub(crate) fn read_global_counter(&self, host: &impl Runtime) -> Result<U256, Error> {
        let path = concat(&self.path, &GLOBAL_COUNTER_PATH)?;
        Ok(read_u256_le_default(host, &path, U256::ZERO)?)
    }

    pub(crate) fn write_global_counter(
        &self,
        host: &mut impl Runtime,
        value: U256,
    ) -> Result<(), Error> {
        let path = concat(&self.path, &GLOBAL_COUNTER_PATH)?;
        Ok(write_u256_le(host, &path, value)?)
    }

    fn ticket_path(
        &self,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<OwnedPath, Error> {
        concat(
            &self.path,
            &concat(
                &TICKET_STORAGE_PATH,
                &concat(&path_from_u256(ticket_hash)?, &account_path(owner)?)?,
            )?,
        )
    }

    pub fn read_ticket_balance(
        &self,
        host: &impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
    ) -> Result<U256, Error> {
        let path = self.ticket_path(ticket_hash, owner)?;
        Ok(read_u256_le_default(host, &path, U256::ZERO)?)
    }

    pub fn write_ticket_balance(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &U256,
        owner: &Address,
        amount: U256,
    ) -> Result<(), Error> {
        let path = self.ticket_path(ticket_hash, owner)?;
        write_u256_le(host, &path, amount)?;
        Ok(())
    }

    fn deposit_path(&self, withdrawal_id: &U256) -> Result<OwnedPath, Error> {
        concat(
            &concat(&self.path, &DEPOSIT_QUEUE_TABLE)?,
            &RefPath::assert_from(format!("/{withdrawal_id}").as_bytes()),
        )
    }

    pub(crate) fn write_deposit(
        &mut self,
        host: &mut impl Runtime,
        deposit_id: &U256,
        deposit: &FaDepositWithProxy,
    ) -> Result<(), Error> {
        let deposit_path = self.deposit_path(deposit_id)?;
        Ok(host.store_write_all(&deposit_path, &deposit.rlp_bytes())?)
    }

    pub(crate) fn read_deposit_from_queue(
        &self,
        host: &impl Runtime,
        deposit_id: &U256,
    ) -> Result<FaDepositWithProxy, Error> {
        let deposit_path = self.deposit_path(deposit_id)?;
        let bytes = host.store_read_all(&deposit_path)?;
        FaDepositWithProxy::from_raw(bytes)
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

#[cfg(test)]
mod test {
    use crate::{
        precompiles::constants::{
            FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
            XTZ_BRIDGE_SOL_CONTRACT,
        },
        storage::code::CodeStorage,
        Error,
    };

    use revm::{
        primitives::{Bytes, FixedBytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use rlp::Encodable;
    use tezos_evm_runtime::runtime::{MockKernelHost, Runtime};

    fn bytecode_from_static(bytes: &'static [u8]) -> Result<Bytecode, Error> {
        Ok(Bytecode::new_legacy(Bytes::from_static(bytes)))
    }

    fn check_account_code_info_fetching(
        host: &mut impl Runtime,
        code_voucher: Bytecode,
        code_hash_voucher: FixedBytes<32>,
    ) {
        let bytecode = CodeStorage::new(&code_hash_voucher)
            .unwrap()
            .get_code(host)
            .unwrap()
            .unwrap_or_default();

        assert_eq!(code_voucher, bytecode);
    }

    #[test]
    fn check_withdrawal_code_info_fetching() {
        let mut host = MockKernelHost::default();
        let code_voucher = bytecode_from_static(XTZ_BRIDGE_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            &mut host,
            code_voucher,
            XTZ_BRIDGE_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_fa_withdrawal_code_info_fetching() {
        let mut host = MockKernelHost::default();
        let code_voucher = bytecode_from_static(FA_BRIDGE_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            &mut host,
            code_voucher,
            FA_BRIDGE_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_internal_forwarder_code_info_fetching() {
        let mut host = MockKernelHost::default();
        let code_voucher =
            bytecode_from_static(INTERNAL_FORWARDER_SOL_CONTRACT.code).unwrap();

        check_account_code_info_fetching(
            &mut host,
            code_voucher,
            INTERNAL_FORWARDER_SOL_CONTRACT.code_hash,
        );
    }

    #[test]
    fn check_empty_account_code_info_fetching() {
        let mut host = MockKernelHost::default();
        let code_voucher = Bytecode::new();

        check_account_code_info_fetching(&mut host, code_voucher, KECCAK_EMPTY);
    }

    #[test]
    fn account_info_size_constant() {
        use crate::storage::world_state_handler::AccountInfoInternal;

        let rlp_size = AccountInfoInternal::default().rlp_bytes().len();
        assert_eq!(rlp_size, AccountInfoInternal::RLP_SIZE);
        assert_eq!(rlp_size, 77);
    }
}
