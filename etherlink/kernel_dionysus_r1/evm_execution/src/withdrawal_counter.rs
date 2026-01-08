// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Withdrawal counter tracks successful XTZ/FA withdrawals,
//! in other words - absolute outbox messages identifiers.
//! It is not related to actual outbox message IDs and indended for offchain
//! usage only (in particular for indexing purposes).
//!
//! Implemented as an EVM account state extension to enable
//! revertable updates (if withdrawal fails the counter won't increment).

use primitive_types::U256;

use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::RefPath;
use tezos_storage::{read_u256_le_default, write_u256_le};

use crate::account_storage::{AccountStorageError, EthereumAccount};

/// Path where withdrawal counter is stored (relative to account)
pub const WITHDRAWAL_COUNTER_PATH: RefPath = RefPath::assert_from(b"/withdrawal_counter");

pub trait WithdrawalCounter {
    #[cfg(test)]
    fn withdrawal_counter_get(
        &self,
        host: &mut impl Runtime,
    ) -> Result<U256, AccountStorageError>;

    /// Returns current withdrawal ID from the storage (or 0 if it's not initialized)
    /// and increments & store the new value (will fail in case of overflow).
    fn withdrawal_counter_get_and_increment(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<U256, AccountStorageError>;
}

impl WithdrawalCounter for EthereumAccount {
    #[cfg(test)]
    fn withdrawal_counter_get(
        &self,
        host: &mut impl Runtime,
    ) -> Result<U256, AccountStorageError> {
        let path = self.custom_path(&WITHDRAWAL_COUNTER_PATH)?;
        Ok(read_u256_le_default(host, &path, U256::zero())?)
    }

    fn withdrawal_counter_get_and_increment(
        &mut self,
        host: &mut impl Runtime,
    ) -> Result<U256, AccountStorageError> {
        let path = self.custom_path(&WITHDRAWAL_COUNTER_PATH)?;
        let old_value = read_u256_le_default(host, &path, U256::zero())?;
        let new_value = old_value
            .checked_add(U256::one())
            .ok_or(AccountStorageError::NonceOverflow)?;

        write_u256_le(host, &path, new_value)?;
        Ok(old_value)
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::U256;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_storage::read_u256_le_default;

    use crate::{account_storage::EthereumAccount, precompiles::SYSTEM_ACCOUNT_ADDRESS};

    use super::WithdrawalCounter;

    #[test]
    fn withdrawal_counter_initializes_and_increments() {
        let mut mock_host = MockKernelHost::default();

        let mut account = EthereumAccount::from_address(&SYSTEM_ACCOUNT_ADDRESS).unwrap();
        let path = b"/evm/world_state/eth_accounts/0000000000000000000000000000000000000000/withdrawal_counter";

        let id = account
            .withdrawal_counter_get_and_increment(&mut mock_host)
            .unwrap();
        assert_eq!(U256::zero(), id);

        let next_id =
            read_u256_le_default(&mock_host, &RefPath::assert_from(path), U256::zero())
                .unwrap();
        assert_eq!(U256::one(), next_id);

        let id = account
            .withdrawal_counter_get_and_increment(&mut mock_host)
            .unwrap();
        assert_eq!(U256::one(), id);

        let next_id =
            read_u256_le_default(&mock_host, &RefPath::assert_from(path), U256::zero())
                .unwrap();
        assert_eq!(U256::from(2), next_id);
    }
}
