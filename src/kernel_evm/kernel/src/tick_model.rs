// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::{apply::TransactionReceiptInfo, inbox::Transaction};
use evm::Config;
use tezos_ethereum::tx_common::EthereumTransactionCommon;

///////////////////////////////////////////////////////////////////////////////
/// TICK MODEL CONSTANTS
/// The following values were estimated using benchmarking, and should not be
/// modified without running more

/// Overapproximation of the amount of ticks for a deposit
/// TODO : https://gitlab.com/tezos/tezos/-/issues/5873
/// estimate value using benchmarks
const TICK_FOR_DEPOSIT: u64 = TICK_FOR_CRYPTO;
/// Overapproximation of the amount of ticks per gas unit
/// TODO : https://gitlab.com/tezos/tezos/-/issues/5873
/// estimate value using benchmarks
const TICK_PER_GAS: u64 = 2000;
// Overapproximation of ticks used in signature verification
const TICK_FOR_CRYPTO: u64 = 25_000_000;
/// Overapproximation of the ticks used by the kernel to process a transaction
/// before checking or execution
/// TODO : https://gitlab.com/tezos/tezos/-/issues/5873
/// estimate value using benchmarks
const TRANSACTION_OVERHEAD: u64 = 1_000_000;
/// Maximum number of ticks for a kernel run as set by the PVM
/// see [ticks_per_snapshot]
pub const MAX_TICKS: u64 = 11_000_000_000;
/// Safety margin the kernel enforce to avoid approaching the maximum number of
/// ticks
const SAFETY_MARGIN: u64 = 2_000_000_000;
/// Overapproximation of the number of ticks the kernel uses to initialise and
/// reload its state
/// TODO : https://gitlab.com/tezos/tezos/-/issues/5873
/// estimate value using benchmarks
const INITIALISATION_OVERHEAD: u64 = 1_000_000_000;
/// see [maximum_reboots_per_input]
pub const _MAXIMUM_NUMBER_OF_REBOOTS: u32 = 1_000;
/// The minimum amount of gas for an ethereum transaction.
const BASE_GAS: u64 = Config::london().gas_transaction_call;

///////////////////////////////////////////////////////////////////////////////

pub fn estimate_ticks_for_transaction(transaction: &Transaction) -> u64 {
    match &transaction.content {
        crate::inbox::TransactionContent::Deposit(_) => ticks_of_deposit(),
        crate::inbox::TransactionContent::Ethereum(eth) => ticks_of_gas(eth.gas_limit),
    }
}

fn ticks_of_deposit() -> u64 {
    TICK_FOR_DEPOSIT + TRANSACTION_OVERHEAD
}

pub fn ticks_of_gas(gas: u64) -> u64 {
    // the ticks used in crypto are not derived from gas
    // the base fee (for all transaction) represent those ticks
    // so the minimum amount is deduced from the gas
    let gas_execution = gas.saturating_sub(BASE_GAS);
    gas_execution
        .saturating_mul(TICK_PER_GAS)
        .saturating_add(TRANSACTION_OVERHEAD)
        .saturating_add(TICK_FOR_CRYPTO)
}

/// Check that a transaction can fit inside the tick limit
pub fn estimate_would_overflow(estimated_ticks: u64, transaction: &Transaction) -> bool {
    estimate_ticks_for_transaction(transaction)
        .saturating_add(estimated_ticks)
        .saturating_add(SAFETY_MARGIN)
        > MAX_TICKS
}

/// Initial amount for the tick accumulator, corresponding to the overhead of a
/// block
pub fn top_level_overhead_ticks() -> u64 {
    INITIALISATION_OVERHEAD
}

/// An invalid transaction could not be transmitted to the VM, eg. the nonce
/// was wrong, or the signature verification failed.
pub fn ticks_of_invalid_transaction() -> u64 {
    // invalid transaction only cost crypto ticks
    TICK_FOR_CRYPTO + TRANSACTION_OVERHEAD
}

pub fn ticks_of_valid_transaction(
    transaction: &Transaction,
    receipt_info: &TransactionReceiptInfo,
) -> u64 {
    match &transaction.content {
        crate::inbox::TransactionContent::Ethereum(eth) => {
            ticks_of_valid_transaction_ethereum(eth, receipt_info)
        }
        crate::inbox::TransactionContent::Deposit(_) => ticks_of_deposit(),
    }
}

/// A valid transaction is a transaction that could be transmitted to
/// evm_execution. It can succeed (with or without effect on the state)
/// or fail (if the VM encountered an error).
pub fn ticks_of_valid_transaction_ethereum(
    transaction: &EthereumTransactionCommon,
    receipt_info: &TransactionReceiptInfo,
) -> u64 {
    match &receipt_info.execution_outcome {
        Some(outcome) => ticks_of_gas(outcome.gas_used),
        None => ticks_of_gas(transaction.gas_limit),
    }
}
