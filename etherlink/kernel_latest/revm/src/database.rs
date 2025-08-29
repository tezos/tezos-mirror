// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    helpers::legacy::FaDepositWithProxy,
    journal::PrecompileStateChanges,
    precompiles::send_outbox_message::Withdrawal,
    storage::{
        block::{get_block_hash, BLOCKS_STORED},
        code::CodeStorage,
        world_state_handler::{
            StorageAccount, WorldStateHandler, WITHDRAWALS_TICKETER_PATH,
        },
    },
    Error,
};
use revm::{
    primitives::{Address, HashMap, StorageKey, StorageValue, B256, KECCAK_EMPTY, U256},
    state::{Account, AccountInfo, Bytecode, EvmStorage, EvmStorageSlot},
    Database, DatabaseCommit,
};
use tezos_crypto_rs::hash::{ContractKt1Hash, HashTrait};
use tezos_ethereum::block::BlockConstants;
use tezos_evm_logging::{log, Level::Error as LogError};
use tezos_evm_runtime::runtime::Runtime;

pub struct EtherlinkVMDB<'a, Host: Runtime> {
    /// Runtime host
    pub host: &'a mut Host,
    /// EVM world state handler
    world_state_handler: &'a mut WorldStateHandler,
    /// Constants for the current block
    block: &'a BlockConstants,
    /// Commit guard, the `DatabaseCommit` trait and in particular
    /// its `commit` function does NOT return errors.
    /// We need this guard to change if there's an unrecoverable
    /// error and we need to revert the changes made to the durable
    /// storage.
    commit_status: bool,
    /// Withdrawals accumulated by the current execution and consumed at the end of it
    withdrawals: Vec<Withdrawal>,
    /// HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
    /// This is used in order to avoid the problem of EIP-3607 for address
    /// zero which contains the forwarder code.
    caller: Address,
}

enum AccountState {
    Touched((AccountInfo, EvmStorage)),
    SelfDestructed(B256),
}

impl<'a, Host: Runtime> EtherlinkVMDB<'a, Host> {
    pub fn new(
        host: &'a mut Host,
        block: &'a BlockConstants,
        world_state_handler: &'a mut WorldStateHandler,
        caller: Address,
    ) -> Self {
        EtherlinkVMDB {
            host,
            block,
            world_state_handler,
            commit_status: true,
            withdrawals: vec![],
            caller,
        }
    }
}

pub trait DatabasePrecompileStateChanges {
    fn ticketer(&self) -> Result<ContractKt1Hash, Error>;
    fn ticket_balance(&self, ticket_hash: &U256, owner: &Address) -> Result<U256, Error>;
    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<Option<FaDepositWithProxy>, Error>;
}

pub(crate) trait DatabaseCommitPrecompileStateChanges {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges);
}

macro_rules! abort_on_error {
    ($obj:expr, $expr:expr, $msg:expr) => {
        if let Err(err) = $expr {
            $obj.abort();
            log!($obj.host, LogError, "{} error: {err:?}", $msg);
            return;
        }
    };
}

impl<Host: Runtime> EtherlinkVMDB<'_, Host> {
    pub fn commit_status(&self) -> bool {
        self.commit_status
    }

    pub fn initialize_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .begin_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn commit_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .commit_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn drop_storage(&mut self) -> Result<(), Error> {
        self.world_state_handler
            .rollback_transaction(self.host)
            .map_err(|err| Error::Custom(err.to_string()))
    }

    pub fn take_withdrawals(&mut self) -> Vec<Withdrawal> {
        std::mem::take(&mut self.withdrawals)
    }

    fn abort(&mut self) {
        self.commit_status = false;
    }

    fn update_account(&mut self, address: Address, account_state: AccountState) {
        match StorageAccount::get_or_create_account(
            self.host,
            self.world_state_handler,
            address,
        ) {
            Ok(mut storage_account) => match account_state {
                AccountState::Touched((info, storage)) => {
                    abort_on_error!(
                        self,
                        storage_account.set_info(self.host, info),
                        "DatabaseCommit `set_info`"
                    );

                    for (
                        key,
                        EvmStorageSlot {
                            original_value,
                            present_value,
                            ..
                        },
                    ) in storage
                    {
                        if original_value != present_value {
                            abort_on_error!(
                                self,
                                storage_account.set_storage(
                                    self.host,
                                    &key,
                                    &present_value,
                                ),
                                "DatabaseCommit `set_storage`"
                            );
                        }
                    }
                }
                AccountState::SelfDestructed(code_hash) => {
                    abort_on_error!(
                        self,
                        storage_account.clear_info(self.host, &code_hash),
                        "DatabaseCommit `clear_info`"
                    );
                    abort_on_error!(
                        self,
                        storage_account.clear_storage(self.host),
                        "DatabaseCommit `clear_storage`"
                    );
                }
            },
            error => {
                abort_on_error!(self, error, "DatabaseCommit `get_or_create_account`")
            }
        }
    }
}

impl<Host: Runtime> DatabasePrecompileStateChanges for EtherlinkVMDB<'_, Host> {
    fn ticket_balance(&self, ticket_hash: &U256, owner: &Address) -> Result<U256, Error> {
        let account_zero = StorageAccount::get_or_create_account(
            self.host,
            self.world_state_handler,
            Address::ZERO,
        )?;
        account_zero
            .read_ticket_balance(self.host, ticket_hash, owner)
            .map(|ticket| ticket.balance)
    }

    fn deposit_in_queue(
        &self,
        deposit_id: &U256,
    ) -> Result<Option<FaDepositWithProxy>, Error> {
        let account_zero = StorageAccount::get_or_create_account(
            self.host,
            self.world_state_handler,
            Address::ZERO,
        )?;
        account_zero.read_deposit_from_queue(self.host, deposit_id)
    }

    fn ticketer(&self) -> Result<ContractKt1Hash, Error> {
        let ticketer = self.host.store_read_all(&WITHDRAWALS_TICKETER_PATH)?;
        let kt1_b58 = String::from_utf8(ticketer.to_vec()).unwrap();
        Ok(ContractKt1Hash::from_b58check(&kt1_b58).unwrap())
    }
}

impl<Host: Runtime> Database for EtherlinkVMDB<'_, Host> {
    type Error = Error;

    fn basic(&mut self, address: Address) -> Result<Option<AccountInfo>, Self::Error> {
        let storage_account =
            StorageAccount::get_account(self.host, self.world_state_handler, address)?;
        let account_info = match storage_account {
            Some(storage_account) => storage_account.info(self.host)?,
            None => AccountInfo::default(),
        };

        if self.caller == Address::ZERO && address == Address::ZERO {
            // HACK: [PRECOMPILE_ZERO_ADDRESS_AND_SIMULATION]
            // This can only happen in a simulation case only.
            // The zero address contains code for legacy reasons related to precompiles.
            // We must return empty code here otherwise the simulation will fail because
            // of EIP-3607.
            return Ok(Some(AccountInfo {
                code_hash: KECCAK_EMPTY,
                code: None,
                ..account_info
            }));
        }

        Ok(Some(account_info))
    }

    fn code_by_hash(&mut self, code_hash: B256) -> Result<Bytecode, Self::Error> {
        let code_storage = CodeStorage::new(&code_hash)?;
        let bytecode = code_storage.get_code(self.host)?;

        Ok(bytecode)
    }

    fn storage(
        &mut self,
        address: Address,
        index: StorageKey,
    ) -> Result<StorageValue, Self::Error> {
        let storage_account =
            StorageAccount::get_account(self.host, self.world_state_handler, address)?;
        let storage_value = match storage_account {
            Some(storage_account) => storage_account.get_storage(self.host, &index)?,
            None => StorageValue::default(),
        };

        Ok(storage_value)
    }

    fn block_hash(&mut self, number: u64) -> Result<B256, Self::Error> {
        // return 0 when block number not in valid range
        // Ref. https://www.evm.codes/?fork=prague#40 (opcode 0x40)

        match self.block.number.checked_sub(number.into()) {
            Some(block_diff)
                if block_diff <= BLOCKS_STORED.into() && !block_diff.is_zero() =>
            {
                get_block_hash(self.host, number)
            }
            _ => Ok(B256::ZERO),
        }
    }
}
impl<Host: Runtime> DatabaseCommitPrecompileStateChanges for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, etherlink_data: PrecompileStateChanges) {
        self.withdrawals = etherlink_data.withdrawals.into_iter().collect();
        let Ok(mut address_zero) = StorageAccount::get_or_create_account(
            self.host,
            self.world_state_handler,
            Address::ZERO,
        ) else {
            log!(self.host, LogError, "DatabaseCommitPrecompileStateChanges `get_or_create_account` error for address zero");
            self.abort();
            return;
        };
        for ((owner, ticket_hash), amount) in etherlink_data.ticket_balances {
            abort_on_error!(
                self,
                address_zero.write_ticket_balance(
                    self.host,
                    &ticket_hash,
                    &owner,
                    amount
                ),
                "DatabaseCommitPrecompileStateChanges `write_ticket_balance`"
            );
        }
        for deposit_id in etherlink_data.removed_deposits {
            abort_on_error!(
                self,
                address_zero.remove_deposit_from_queue(self.host, &deposit_id),
                "DatabaseCommitPrecompileStateChanges `remove_deposit_from_queue`"
            );
        }
    }
}

impl<Host: Runtime> DatabaseCommit for EtherlinkVMDB<'_, Host> {
    fn commit(&mut self, changes: HashMap<Address, Account>) {
        for (address, account) in changes {
            // The account isn't marked as touched, the changes are not commited
            // to the database.
            if !account.is_touched() {
                continue;
            }

            // The account is touched and marked as selfdestructed, we clear the
            // account.
            if account.is_selfdestructed() {
                self.update_account(
                    address,
                    AccountState::SelfDestructed(account.info.code_hash),
                );
                continue;
            }

            // The account is touched, the changes are naturally commited to the database.
            self.update_account(
                address,
                AccountState::Touched((account.info, account.storage)),
            );
        }
    }
}
