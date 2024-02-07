// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Adjustments & calculation of fees, over-and-above the execution gas fee.
//!
//! Users submit transactions which contain three values related to fees:
//! - `gas_limit`
//! - `max_fee_per_gas`
//! - `max_priority_fee_per_gas`
//!
//! We ignore `tx.max_priority_fee_per_gas` completely. For every transaction, we act as if the
//! user set `tx.max_priority_fee_per_gas = 0`. We therefore only care about `tx.gas_limit` and
//! `tx.max_fee_per_gas`.

use evm_execution::account_storage::{account_path, EthereumAccountStorage};
use evm_execution::handler::ExecutionOutcome;
use evm_execution::EthereumError;
use primitive_types::{H160, U256};
use tezos_ethereum::block::BlockFees;
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_smart_rollup_host::runtime::Runtime;

/// Instructions for 'balancing the books'.
#[derive(Debug)]
pub struct FeeUpdates {
    pub overall_gas_price: U256,
    pub overall_gas_used: U256,
    pub burn_amount: U256,
    pub charge_user_amount: U256,
    pub compensate_sequencer_amount: U256,
}

impl FeeUpdates {
    /// Returns the fee updates for a deposit.
    pub fn for_deposit(gas_used: U256) -> Self {
        Self {
            overall_gas_used: gas_used,
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: U256::zero(),
            compensate_sequencer_amount: U256::zero(),
        }
    }

    /// Returns fee updates of the transaction.
    ///
    /// *NB* this is not the gas price used _for execution_, but rather the gas price that
    /// should be reported in the transaction receipt.
    ///
    /// # Prerequisites
    /// The user must have already paid for 'execution gas fees'.
    pub fn for_tx(
        tx: &EthereumTransactionCommon,
        block_fees: &BlockFees,
        execution_gas_used: U256,
    ) -> Self {
        let execution_gas_fees = execution_gas_used * block_fees.base_fee_per_gas();
        let initial_added_fees = block_fees.flat_fee();
        let initial_total_fees = initial_added_fees + execution_gas_fees;

        // first, we find the price of gas (with all fees), for the given gas used
        let mut gas_price = cdiv(initial_added_fees, execution_gas_used)
            .saturating_add(block_fees.base_fee_per_gas());

        let gas_used = if gas_price > tx.max_fee_per_gas {
            // We can't charge more than `max_fee_per_gas`, so bump the gas limit too.
            // added_gas = initial_total_fee / mgp - execution_gas
            gas_price = tx.max_fee_per_gas;
            cdiv(initial_total_fees, gas_price)
        } else {
            execution_gas_used
        };

        let total_fees = gas_price * gas_used;

        // Due to rounding, we may have a small amount of unaccounted-for gas.
        // Assign this to the flat fee (to be burned).
        let burn_amount = block_fees.flat_fee() + total_fees - initial_total_fees;

        // For now, just the execution fees. Future MR will include data availability
        // also.
        let compensate_sequencer_amount = execution_gas_fees;

        Self {
            overall_gas_price: gas_price,
            overall_gas_used: gas_used,
            burn_amount,
            charge_user_amount: total_fees - execution_gas_fees,
            compensate_sequencer_amount,
        }
    }

    pub fn modify_outcome(&self, outcome: &mut ExecutionOutcome) {
        outcome.gas_used = self.overall_gas_used.as_u64();
    }

    pub fn apply(
        &self,
        host: &mut impl Runtime,
        accounts: &mut EthereumAccountStorage,
        caller: H160,
    ) -> Result<(), anyhow::Error> {
        let caller_account_path = account_path(&caller)?;
        let mut caller_account = accounts.get_or_create(host, &caller_account_path)?;
        if !caller_account.balance_remove(host, self.charge_user_amount)? {
            return Err(anyhow::anyhow!(
                "Failed to charge {caller} additional fees of {}",
                self.charge_user_amount
            ));
        }

        Ok(())
    }
}

/// Adjust a simulation outcome, to take non-execution fees into account.
///
/// This is done by adjusting `gas_used` upwards.
pub fn simulation_add_gas_for_fees(
    mut outcome: ExecutionOutcome,
    block_fees: &BlockFees,
    tx_gas_price: U256,
) -> Result<ExecutionOutcome, EthereumError> {
    let gas_for_fees = cdiv(block_fees.flat_fee(), tx_gas_price);
    let gas_for_fees = gas_as_u64(gas_for_fees)?;

    outcome.gas_used = outcome.gas_used.saturating_add(gas_for_fees);
    Ok(outcome)
}

/// Returns the gas limit for executing this transaction.
///
/// This is strictly lower than the gas limit set by the user, as additional gas is required
/// in order to pay for the *flat fee* and *data availability fee*.
///
/// The user pre-pays this (in addition to the flat fee & data availability fee) prior to execution.
/// If execution does not use all of the execution gas limit, they will be partially refunded.
pub fn tx_execution_gas_limit(
    tx: &EthereumTransactionCommon,
    fees: &BlockFees,
) -> Result<u64, EthereumError> {
    let gas_for_fees = cdiv(fees.flat_fee(), tx.max_fee_per_gas);

    let gas_for_fees = gas_as_u64(gas_for_fees)?;

    tx.gas_limit_with_fees()
        .checked_sub(gas_for_fees)
        .ok_or(EthereumError::GasToFeesUnderflow)
}

fn cdiv(l: U256, r: U256) -> U256 {
    match l.div_mod(r) {
        (res, rem) if rem.is_zero() => res,
        (res, _) => res.saturating_add(U256::one()),
    }
}

fn gas_as_u64(gas_for_fees: U256) -> Result<u64, EthereumError> {
    gas_for_fees
        .try_into()
        .map_err(|_e| EthereumError::FeesToGasOverflow)
}

#[cfg(test)]
mod tests {
    use super::*;
    use evm::{ExitReason, ExitSucceed};
    use evm_execution::{
        account_storage::{account_path, EthereumAccountStorage},
        handler::ExtendedExitReason,
    };
    use primitive_types::{H160, U256};
    use tezos_smart_rollup_mock::MockHost;

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn fee_updates_consistent(
            flat_fee in any::<u64>().prop_map(U256::from),
            execution_gas_used in (1_u64..).prop_map(U256::from),
            (max_fee_per_gas, base_fee_per_gas) in (1_u64.., 1_u64..)
                .prop_map(|(a, b)| if a > b {(a, b)} else {(b, a)})
                .prop_map(|(a, b)| (a.into(), b.into())),
        ) {
            // Arrange
            let tx = mock_tx(max_fee_per_gas);
            let block_fees = BlockFees::new(base_fee_per_gas, flat_fee);

            // Act
            let updates = FeeUpdates::for_tx(&tx, &block_fees, execution_gas_used);

            // Assert
            let assumed_execution_gas_cost = base_fee_per_gas * execution_gas_used;
            let total_fee = updates.overall_gas_price * updates.overall_gas_used;

            assert!(updates.burn_amount >= flat_fee, "burn amount should cover flat fee");
            assert_eq!(updates.charge_user_amount, total_fee - assumed_execution_gas_cost, "inconsistent user charge");
            assert_eq!(total_fee, updates.burn_amount + updates.compensate_sequencer_amount, "inconsistent total fees");
            assert_eq!(updates.compensate_sequencer_amount, assumed_execution_gas_cost, "inconsistent sequencer comp");
        }
    }

    #[test]
    fn simulation_covers_extra_fees() {
        fn expect_extra_gas(extra: u64, tx_gas_price: u64, flat_fee: u64) {
            // Arrange
            let initial_gas_used = 100;
            let block_fees = BlockFees::new(U256::one(), U256::from(flat_fee));

            let simulated_outcome = mock_execution_outcome(initial_gas_used);

            // Act
            let res = simulation_add_gas_for_fees(
                simulated_outcome,
                &block_fees,
                tx_gas_price.into(),
            );

            // Assert
            let expected = mock_execution_outcome(initial_gas_used + extra);
            assert_eq!(
                Ok(expected),
                res,
                "unexpected extra gas at price {tx_gas_price} and fee {flat_fee}"
            );
        }

        let flat_fee = 15;

        expect_extra_gas(4, 4, flat_fee);
        for tx_gas_price in 5..7 {
            expect_extra_gas(3, tx_gas_price, flat_fee);
        }
        for tx_gas_price in 8..14 {
            expect_extra_gas(2, tx_gas_price, flat_fee);
        }
        for tx_gas_price in 15..40 {
            // could be any value of gas price above 15
            expect_extra_gas(1, tx_gas_price, flat_fee);
        }
    }

    #[test]
    fn apply_deducts_balance_from_user() {
        // Arrange
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();

        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let balance = U256::from(1000);
        set_balance(&mut host, &mut evm_account_storage, address, balance);

        let fee_updates = FeeUpdates {
            overall_gas_used: U256::zero(),
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: balance / 2,
            compensate_sequencer_amount: U256::zero(),
        };

        // Act
        let result = fee_updates.apply(&mut host, &mut evm_account_storage, address);

        // Assert
        assert!(result.is_ok());
        let new_balance = get_balance(&mut host, &mut evm_account_storage, address);
        assert_eq!(balance / 2, new_balance);
    }

    #[test]
    fn apply_fails_user_charge_too_large() {
        // Arrange
        let mut host = MockHost::default();
        let mut evm_account_storage =
            evm_execution::account_storage::init_account_storage().unwrap();

        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let balance = U256::from(1000);
        set_balance(&mut host, &mut evm_account_storage, address, balance);

        let fee_updates = FeeUpdates {
            overall_gas_used: U256::zero(),
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: balance * 2,
            compensate_sequencer_amount: U256::zero(),
        };

        // Act
        let result = fee_updates.apply(&mut host, &mut evm_account_storage, address);

        // Assert
        assert!(result.is_err());
        let new_balance = get_balance(&mut host, &mut evm_account_storage, address);
        assert_eq!(balance, new_balance);
    }

    fn address_from_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn get_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: H160,
    ) -> U256 {
        let account = evm_account_storage
            .get_or_create(host, &account_path(&address).unwrap())
            .unwrap();
        account.balance(host).unwrap()
    }

    fn set_balance(
        host: &mut MockHost,
        evm_account_storage: &mut EthereumAccountStorage,
        address: H160,
        balance: U256,
    ) {
        let mut account = evm_account_storage
            .get_or_create(host, &account_path(&address).unwrap())
            .unwrap();
        assert!(account.balance(host).unwrap().is_zero());

        account.balance_add(host, balance).unwrap();
    }

    fn mock_execution_outcome(gas_used: u64) -> ExecutionOutcome {
        ExecutionOutcome {
            gas_used,
            is_success: true,
            reason: ExtendedExitReason::Exit(ExitReason::Succeed(ExitSucceed::Stopped)),
            new_address: None,
            logs: Vec::new(),
            result: None,
            withdrawals: Vec::new(),
            estimated_ticks_used: 1,
        }
    }

    fn mock_tx(max_fee_per_gas: U256) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Eip1559,
            None,
            U256::zero(),
            U256::zero(),
            max_fee_per_gas,
            0,
            Some(H160::zero()),
            U256::zero(),
            vec![],
            vec![],
            None,
        )
    }
}
