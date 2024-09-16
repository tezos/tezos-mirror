// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! Global ticket table.
//!
//! Maintains a ledger that tracks ownership of deposited tickets.
//! Any EVM account can be ticket owner, whether it's EOA or smart contract.

use crate::account_storage::{account_path, AccountStorageError, EthereumAccount};
use primitive_types::{H160, H256, U256};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{concat, OwnedPath, RefPath};
use tezos_storage::{path_from_h256, read_u256_le_default, write_u256_le};

/// Path where global ticket table is stored
const TICKET_TABLE_PATH: RefPath = RefPath::assert_from(b"/ticket_table");

pub trait TicketTable {
    /// Increases ticket balance
    fn ticket_balance_add(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &H256,
        address: &H160,
        amount: U256,
    ) -> Result<bool, AccountStorageError>;

    /// Decreases ticket balance
    fn ticket_balance_remove(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &H256,
        address: &H160,
        amount: U256,
    ) -> Result<bool, AccountStorageError>;
}

pub(crate) fn ticket_balance_path(
    ticket_hash: &H256,
    address: &H160,
) -> Result<OwnedPath, AccountStorageError> {
    let suffix = concat(&path_from_h256(ticket_hash)?, &account_path(address)?)?;
    concat(&TICKET_TABLE_PATH, &suffix).map_err(Into::into)
}

impl TicketTable for EthereumAccount {
    fn ticket_balance_add(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &H256,
        owner: &H160,
        amount: U256,
    ) -> Result<bool, AccountStorageError> {
        let path = self.custom_path(&ticket_balance_path(ticket_hash, owner)?)?;
        let balance = read_u256_le_default(host, &path, U256::zero())?;

        if let Some(new_balance) = balance.checked_add(amount) {
            write_u256_le(host, &path, new_balance)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn ticket_balance_remove(
        &mut self,
        host: &mut impl Runtime,
        ticket_hash: &H256,
        owner: &H160,
        amount: U256,
    ) -> Result<bool, AccountStorageError> {
        let path = self.custom_path(&ticket_balance_path(ticket_hash, owner)?)?;
        let balance = read_u256_le_default(host, &path, U256::zero())?;

        if let Some(new_balance) = balance.checked_sub(amount) {
            write_u256_le(host, &path, new_balance)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_storage::read_u256_le_default;

    use crate::precompiles::SYSTEM_ACCOUNT_ADDRESS;

    use super::*;

    #[test]
    fn ticket_table_balance_add_succeeds() {
        let mut host = MockKernelHost::default();

        let mut account = EthereumAccount::from_address(&SYSTEM_ACCOUNT_ADDRESS).unwrap();

        let ticket_hash: H256 = H256([1u8; 32]);
        let address = H160([2u8; 20]);

        account
            .ticket_balance_add(&mut host, &ticket_hash, &address, 42.into())
            .unwrap();
        account
            .ticket_balance_add(&mut host, &ticket_hash, &address, 42.into())
            .unwrap();

        let path = b"\
            /evm/world_state/eth_accounts/0000000000000000000000000000000000000000/ticket_table\
            /0101010101010101010101010101010101010101010101010101010101010101\
            /0202020202020202020202020202020202020202";
        let balance =
            read_u256_le_default(&host, &RefPath::assert_from(path), U256::zero())
                .unwrap();

        assert_eq!(U256::from(84), balance);
    }

    #[test]
    fn ticket_table_balance_add_overflows() {
        let mut host = MockKernelHost::default();

        let mut account = EthereumAccount::from_address(&SYSTEM_ACCOUNT_ADDRESS).unwrap();

        let ticket_hash: H256 = H256([1u8; 32]);
        let address = H160([2u8; 20]);

        account
            .ticket_balance_add(&mut host, &ticket_hash, &address, U256::MAX)
            .unwrap();

        let res = account
            .ticket_balance_add(&mut host, &ticket_hash, &address, U256::MAX)
            .unwrap();
        assert!(!res);

        let path = b"\
            /evm/world_state/eth_accounts/0000000000000000000000000000000000000000/ticket_table\
            /0101010101010101010101010101010101010101010101010101010101010101\
            /0202020202020202020202020202020202020202";
        let balance =
            read_u256_le_default(&host, &RefPath::assert_from(path), U256::zero())
                .unwrap();

        assert_eq!(U256::MAX, balance);
    }
}
