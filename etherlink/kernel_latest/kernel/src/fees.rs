// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
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
//!
//! Additionally, we charge a _data-availability_ fee, for each tx posted through L1.

use crate::simulation::SimulationOutcome;
use crate::transaction::TransactionContent;

use primitive_types::{H160, H256, U256};
use revm_etherlink::helpers::legacy::{h160_to_alloy, u256_to_alloy};
use revm_etherlink::{storage::world_state_handler::StorageAccount, Error};
use tezos_ethereum::access_list::AccessListItem;
use tezos_ethereum::block::BlockFees;
use tezos_ethereum::tx_common::EthereumTransactionCommon;
use tezos_evm_runtime::runtime::Runtime;

use std::mem::size_of;

// /!\
//     If you update these constants, you need to update the evm-node as well.
//     The node uses the constants to compute the DA fees without calling the
//     kernel, therefore they need to be synchronised.
//
//     If you happen to change the constants, it is recommended to write them
//     to the storage instead. If they're written in the storage, we don't need
//     need to default to a duplicated constant.
// /!\

/// Minimum base fee per gas, set to 1 Gwei.
pub const MINIMUM_BASE_FEE_PER_GAS: u64 = 10_u64.pow(9);

// We assume a tx (with empty data) consumes roughly 150 bytes in the inbox.
//
// This is a slight underestimate (when external message framing is included), but this is
// compensated by charging double the expected L1 gas fee for each byte.
const ASSUMED_TX_ENCODED_SIZE: usize = 150;

// 4 mutez per byte.
//
// The fee for injection on L1 is composed of the following
// (at default minimal nanotez per gas / default minimal fee):
// - base cost for 'add smart rollup message': 259 mutez
// - cost per new message (tx chunk): 5 mutez
// - cost per additional byte: 1 mutez
//
// To avoid a more complex calculation, simplify by assuming double the
// default cost per byte, to cover the static fees.
//
// Since multiple messages are grouped up into a single operation of
// 'add smart rollup messages', together they will cover the larger base
// fee for this operation.
pub(crate) const DA_FEE_PER_BYTE: u64 = 4 * 10_u64.pow(12);

/// Instructions for 'balancing the books'.
#[derive(Debug)]
pub struct FeeUpdates {
    pub overall_gas_price: U256,
    pub overall_gas_used: U256,
    pub burn_amount: U256,
    pub charge_user_amount: U256,
    pub compensate_sequencer_amount: U256,
}

impl TransactionContent {
    /// Returns fee updates of the transaction.
    ///
    /// *NB* this is not the gas price used _for execution_, but rather the gas price that
    /// should be reported in the transaction receipt.
    ///
    /// # Prerequisites
    /// The user must have already paid for 'execution gas fees'.
    pub fn fee_updates(
        &self,
        block_fees: &BlockFees,
        execution_gas_used: U256,
    ) -> FeeUpdates {
        match self {
            Self::Deposit(_) => FeeUpdates::for_deposit(execution_gas_used),
            Self::Ethereum(tx) => FeeUpdates::for_tx(tx, block_fees, execution_gas_used),
            Self::EthereumDelayed(_) | Self::TezosDelayed(_) => {
                FeeUpdates::for_delayed_tx(block_fees, execution_gas_used)
            }
            Self::FaDeposit(_) => FeeUpdates::for_fa_deposit(execution_gas_used),
        }
    }
}

impl FeeUpdates {
    fn for_deposit(gas_used: U256) -> Self {
        Self {
            overall_gas_used: gas_used,
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: U256::zero(),
            compensate_sequencer_amount: U256::zero(),
        }
    }

    fn for_fa_deposit(gas_used: U256) -> Self {
        Self {
            overall_gas_used: gas_used,
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: U256::zero(),
            compensate_sequencer_amount: U256::zero(),
        }
    }

    fn for_tx(
        tx: &EthereumTransactionCommon,
        block_fees: &BlockFees,
        execution_gas_used: U256,
    ) -> Self {
        let da_fee = da_fee(block_fees.da_fee_per_byte(), &tx.data, &tx.access_list);

        Self::of_gas_and_da_fee(block_fees, execution_gas_used, da_fee)
    }

    fn for_delayed_tx(block_fees: &BlockFees, execution_gas_used: U256) -> Self {
        Self::of_gas_and_da_fee(block_fees, execution_gas_used, U256::zero())
    }

    fn of_gas_and_da_fee(
        block_fees: &BlockFees,
        execution_gas_used: U256,
        da_fee: U256,
    ) -> Self {
        let execution_gas_fees = execution_gas_used * block_fees.base_fee_per_gas();

        let initial_added_fees = da_fee;
        let initial_total_fees = initial_added_fees + execution_gas_fees;

        let gas_price = block_fees.base_fee_per_gas();

        let gas_used = cdiv(initial_total_fees, gas_price);

        let total_fees = gas_price * gas_used;

        // Due to rounding, we may have a small amount of unaccounted-for gas.
        // Assign this to the burned fee.
        let burn_amount = execution_gas_fees + total_fees - initial_total_fees;

        Self {
            overall_gas_price: gas_price,
            overall_gas_used: gas_used,
            burn_amount,
            charge_user_amount: total_fees - execution_gas_fees,
            compensate_sequencer_amount: da_fee,
        }
    }

    pub fn modify_outcome(&self, outcome: &mut SimulationOutcome) {
        outcome.gas_used = self.overall_gas_used.as_u64();
    }

    pub fn apply(
        &self,
        host: &mut impl Runtime,
        caller: H160,
        sequencer_pool_address: Option<H160>,
    ) -> Result<(), anyhow::Error> {
        tezos_evm_logging::log!(
            host,
            tezos_evm_logging::Level::Debug,
            "Applying {self:?} for {caller}"
        );

        let mut caller_account = StorageAccount::from_address(&h160_to_alloy(&caller))?;

        if let Err(e) =
            caller_account.sub_balance(host, u256_to_alloy(&self.charge_user_amount))
        {
            return Err(anyhow::anyhow!(
                "Failed to charge {caller} additional fees of {}: {}",
                self.charge_user_amount,
                e
            ));
        }

        let sequencer = match sequencer_pool_address {
            None => {
                let burned_fee = self
                    .burn_amount
                    .saturating_add(self.compensate_sequencer_amount);

                crate::storage::update_burned_fees(host, burned_fee)?;
                return Ok(());
            }
            Some(sequencer) => {
                crate::storage::update_burned_fees(host, self.burn_amount)?;
                sequencer
            }
        };

        let mut account = StorageAccount::from_address(&h160_to_alloy(&sequencer))?;
        if let Err(e) =
            account.add_balance(host, u256_to_alloy(&self.compensate_sequencer_amount))
        {
            return Err(anyhow::anyhow!(
                "Failed to compensate sequencer {sequencer} of {}: {}",
                self.compensate_sequencer_amount,
                e
            ));
        }
        Ok(())
    }
}

/// Adjust a simulation outcome, to take non-execution fees into account.
///
/// This is done by adjusting `gas_used` upwards.
pub fn simulation_add_gas_for_fees(
    mut outcome: SimulationOutcome,
    block_fees: &BlockFees,
    tx_data: &[u8],
) -> Result<SimulationOutcome, Error> {
    // Simulation does not have an access list
    let gas_for_fees = gas_for_fees(
        block_fees.da_fee_per_byte(),
        // We select minimum base fee per gas, to ensure that the user has sufficient gas
        // even if the base_fee_per_gas falls by the time they submit their tx.
        block_fees.minimum_base_fee_per_gas(),
        tx_data,
        &[],
    )?;

    outcome.gas_used = outcome.gas_used.saturating_add(gas_for_fees);
    Ok(outcome)
}

/// Returns the gas limit for executing this transaction.
///
/// This is strictly lower than the gas limit set by the user, as additional gas is required
/// in order to pay for the *data availability fee*.
///
/// The user pre-pays this (in addition to the data availability fee) prior to execution.
/// If execution does not use all of the execution gas limit, they will be partially refunded.
/// If the transaction was sent through the delayed inbox, no additional fees need to be included
pub fn tx_execution_gas_limit(
    tx: &EthereumTransactionCommon,
    fees: &BlockFees,
    delayed: bool,
) -> Result<u64, Error> {
    if delayed {
        return Ok(tx.gas_limit_with_fees());
    }

    let gas_for_fees = gas_for_fees(
        fees.da_fee_per_byte(),
        fees.minimum_base_fee_per_gas(),
        &tx.data,
        &tx.access_list,
    )?;

    tx.gas_limit_with_fees()
        .checked_sub(gas_for_fees)
        .ok_or(Error::GasToFeesUnderflow)
}

/// Calculate gas for fees
pub(crate) fn gas_for_fees(
    da_fee_per_byte: U256,
    gas_price: U256,
    tx_data: &[u8],
    tx_access_list: &[AccessListItem],
) -> Result<u64, Error> {
    let fees = da_fee(da_fee_per_byte, tx_data, tx_access_list);

    let gas_for_fees = cdiv(fees, gas_price);
    gas_as_u64(gas_for_fees)
}

/// Data availability fee for a transaction with given data size.
pub(crate) fn da_fee(
    da_fee_per_byte: U256,
    tx_data: &[u8],
    access_list: &[AccessListItem],
) -> U256 {
    let access_data_size: usize = access_list
        .iter()
        .map(|ali| size_of::<H160>() + size_of::<H256>() * ali.storage_keys.len())
        .sum();

    U256::from(tx_data.len())
        .saturating_add(ASSUMED_TX_ENCODED_SIZE.into())
        .saturating_add(access_data_size.into())
        .saturating_mul(da_fee_per_byte)
}

fn cdiv(l: U256, r: U256) -> U256 {
    match l.div_mod(r) {
        (res, rem) if rem.is_zero() => res,
        (res, _) => res.saturating_add(U256::one()),
    }
}

fn gas_as_u64(gas_for_fees: U256) -> Result<u64, Error> {
    gas_for_fees
        .try_into()
        .map_err(|_e| Error::FeesToGasOverflow)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloy_primitives::Bytes;
    use primitive_types::{H160, U256};
    use revm::context::result::{ExecutionResult, Output};
    use revm_etherlink::helpers::legacy::alloy_to_u256;
    use revm_etherlink::ExecutionOutcome;
    use tezos_evm_runtime::runtime::MockKernelHost;

    use proptest::prelude::*;

    proptest! {
        #[test]
        fn fee_updates_consistent(
            da_fee in any::<u64>().prop_map(U256::from),
            data in any::<Vec<u8>>(),
            execution_gas_used in (1_u64..).prop_map(U256::from),
            [min_fee_per_gas, base_fee_per_gas, max_fee_per_gas] in [1_u64.., 1_u64.., 1_u64..]
                .prop_map(|mut prices| {prices.sort(); prices} )
                .prop_map(|[a, b, c]| [a.into(), b.into(), c.into()]),
        ) {
            // Arrange
            let data_size = data.len();
            let tx = mock_tx(max_fee_per_gas, data);
            let block_fees = BlockFees::new(min_fee_per_gas, base_fee_per_gas, da_fee);

            // Act
            let updates = FeeUpdates::for_tx(&tx, &block_fees, execution_gas_used);

            // Assert
            let assumed_execution_gas_cost = base_fee_per_gas * execution_gas_used;
            let total_fee = updates.overall_gas_price * updates.overall_gas_used;
            let expected_sequencer_comp = da_fee * (data_size + ASSUMED_TX_ENCODED_SIZE);

            assert_eq!(updates.overall_gas_price, base_fee_per_gas, "gas price should be 'base fee per gas'");
            assert!(updates.burn_amount >= assumed_execution_gas_cost, "inconsistent burn amount");
            assert_eq!(updates.charge_user_amount, total_fee - assumed_execution_gas_cost, "inconsistent user charge");
            assert_eq!(total_fee, updates.burn_amount + updates.compensate_sequencer_amount, "inconsistent total fees");
            assert_eq!(updates.compensate_sequencer_amount, expected_sequencer_comp, "inconsistent sequencer comp");
        }
    }

    #[test]
    fn simulation_covers_extra_fees() {
        let tx_data = &[0, 1, 2];

        let fee = ((tx_data.len() + ASSUMED_TX_ENCODED_SIZE) as u64) * DA_FEE_PER_BYTE;

        expect_extra_gas(fee / 4, 4, DA_FEE_PER_BYTE, tx_data);
        expect_extra_gas(fee / 5, 5, DA_FEE_PER_BYTE, tx_data);
        expect_extra_gas(fee / 6, 6, DA_FEE_PER_BYTE, tx_data);
        // expect extra gas to cover rounding
        expect_extra_gas(fee / 7 + 1, 7, DA_FEE_PER_BYTE, tx_data);
    }

    #[test]
    fn apply_updates_balances_no_sequencer() {
        // Arrange
        let mut host = MockKernelHost::default();

        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let balance = U256::from(1000);
        set_balance(&mut host, address, balance);

        let burn_amount = balance / 3;
        let compensate_sequencer_amount = balance / 4;

        let fee_updates = FeeUpdates {
            overall_gas_used: U256::zero(),
            overall_gas_price: U256::zero(),
            burn_amount,
            charge_user_amount: balance / 2,
            compensate_sequencer_amount,
        };

        // Act
        let result = fee_updates.apply(&mut host, address, None);

        // Assert
        assert!(result.is_ok());
        let new_balance = get_balance(&mut host, address);
        assert_eq!(balance / 2, new_balance);

        let burned = crate::storage::read_burned_fees(&mut host);

        // no sequencer reward address set - so everything is burned
        assert_eq!(burn_amount + compensate_sequencer_amount, burned);
    }

    #[test]
    fn apply_updates_balances_with_sequencer() {
        // Arrange
        let mut host = MockKernelHost::default();
        let sequencer_address =
            address_from_str("0123456789ABCDEF0123456789ABCDEF01234567");

        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let balance = U256::from(1000);
        set_balance(&mut host, address, balance);

        let sequencer_balance = U256::from(500);
        set_balance(&mut host, sequencer_address, sequencer_balance);

        let burn_amount = balance / 3;
        let compensate_sequencer_amount = balance / 4;

        let fee_updates = FeeUpdates {
            overall_gas_used: U256::zero(),
            overall_gas_price: U256::zero(),
            burn_amount,
            charge_user_amount: balance / 2,
            compensate_sequencer_amount,
        };

        // Act
        let result = fee_updates.apply(&mut host, address, Some(sequencer_address));

        // Assert
        assert!(result.is_ok());
        let new_balance = get_balance(&mut host, address);
        assert_eq!(balance / 2, new_balance);

        let burned = crate::storage::read_burned_fees(&mut host);
        assert_eq!(burn_amount, burned);

        let sequencer_new_balance = get_balance(&mut host, sequencer_address);
        assert_eq!(
            sequencer_new_balance,
            sequencer_balance + compensate_sequencer_amount
        );
    }

    #[test]
    fn apply_fails_user_charge_too_large() {
        // Arrange
        let mut host = MockKernelHost::default();

        let address = address_from_str("af1276cbb260bb13deddb4209ae99ae6e497f446");
        let balance = U256::from(1000);
        set_balance(&mut host, address, balance);

        let fee_updates = FeeUpdates {
            overall_gas_used: U256::zero(),
            overall_gas_price: U256::zero(),
            burn_amount: U256::zero(),
            charge_user_amount: balance * 2,
            compensate_sequencer_amount: U256::zero(),
        };

        // Act
        let result = fee_updates.apply(&mut host, address, None);

        // Assert
        assert!(result.is_err());
        let new_balance = get_balance(&mut host, address);
        assert_eq!(balance, new_balance);
    }

    #[test]
    fn da_fee_includes_access_lists() {
        // Arrange
        let ali = AccessListItem {
            address: H160::zero(),
            storage_keys: vec![H256::zero(), H256::zero()],
        };
        let al = &[ali.clone(), ali];

        let data = &[1, 2, 3];

        // Act
        let fee = da_fee(super::DA_FEE_PER_BYTE.into(), data, al);

        // Assert
        let expected_bytes = data.len() + 2 * (20 /* address */ + 2 * 32/* keys */);
        let expected_fee =
            (expected_bytes + ASSUMED_TX_ENCODED_SIZE) as u64 * DA_FEE_PER_BYTE;

        assert_eq!(fee, expected_fee.into());
    }

    fn address_from_str(s: &str) -> H160 {
        let data = &hex::decode(s).unwrap();
        H160::from_slice(data)
    }

    fn get_balance(host: &mut MockKernelHost, address: H160) -> U256 {
        let account = StorageAccount::from_address(&h160_to_alloy(&address)).unwrap();
        let info = account.info(host).unwrap();
        alloy_to_u256(&info.balance)
    }

    fn set_balance(host: &mut MockKernelHost, address: H160, balance: U256) {
        let mut account = StorageAccount::from_address(&h160_to_alloy(&address)).unwrap();
        let mut info = account.info(host).unwrap();
        assert!(info.balance.is_zero());
        info.balance = info.balance.saturating_add(u256_to_alloy(&balance));
        account.set_info(host, info).unwrap();
    }

    fn mock_execution_outcome(gas_used: u64) -> SimulationOutcome {
        let outcome = ExecutionOutcome {
            withdrawals: vec![],
            result: ExecutionResult::Success {
                reason: revm::context::result::SuccessReason::Return,
                gas_used,
                gas_refunded: 0,
                logs: vec![],
                output: Output::Call(Bytes::new()),
            },
        };
        SimulationOutcome { gas_used, outcome }
    }

    fn mock_tx(max_fee_per_gas: U256, data: Vec<u8>) -> EthereumTransactionCommon {
        EthereumTransactionCommon::new(
            tezos_ethereum::transaction::TransactionType::Eip1559,
            None,
            0,
            U256::zero(),
            max_fee_per_gas,
            0,
            Some(H160::zero()),
            U256::zero(),
            data,
            vec![],
            None,
            None,
        )
    }

    fn expect_extra_gas(extra: u64, min_gas_price: u64, da_fee: u64, tx_data: &[u8]) {
        // Arrange
        let initial_gas_used = 100;
        let block_fees = BlockFees::new(min_gas_price.into(), U256::one(), da_fee.into());

        let simulated_outcome = mock_execution_outcome(initial_gas_used);

        // Act
        let res =
            simulation_add_gas_for_fees(simulated_outcome, &block_fees, tx_data).unwrap();

        // Assert
        assert_eq!(
            initial_gas_used + extra,
            res.gas_used,
            "unexpected extra gas at price {min_gas_price} and data_len {}",
            tx_data.len()
        );
    }
}
