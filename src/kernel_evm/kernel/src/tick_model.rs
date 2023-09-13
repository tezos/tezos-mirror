// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::{apply::TransactionReceiptInfo, inbox::Transaction};
use tezos_ethereum::tx_common::EthereumTransactionCommon;

/// Tick model constants
///
/// Some of the following values were estimated using benchmarking, and should
/// be updated only when the benchmarks are executed.
/// This doesn't apply to inherited constants from the PVM, e.g. maximum
/// number of reboots.
pub mod constants {

    /// Maximum number of ticks for a kernel run as set by the PVM
    pub(crate) const MAX_TICKS: u64 = 11_000_000_000;

    /// Maximum number of allowed ticks for a kernel run. We consider a safety
    /// margin and an incompressible initilisation overhead.
    pub const MAX_ALLOWED_TICKS: u64 =
        MAX_TICKS - SAFETY_MARGIN - INITIALISATION_OVERHEAD;

    /// Maximum number of reboots for a level as set by the PVM.
    pub(crate) const _MAX_NUMBER_OF_REBOOTS: u32 = 1_000;

    /// Overapproximation of the amount of ticks for a deposit.
    pub const TICKS_FOR_DEPOSIT: u64 = TICKS_FOR_CRYPTO;

    /// Overapproximation of the amount of ticks per gas unit.
    pub const TICKS_PER_GAS: u64 = 2000;

    // Overapproximation of ticks used in signature verification.
    pub const TICKS_FOR_CRYPTO: u64 = 25_000_000;

    /// Overapproximation of the ticks used by the kernel to process a transaction
    /// before checking or execution.
    pub const TRANSACTION_OVERHEAD: u64 = 1_000_000;

    /// Safety margin the kernel enforce to avoid approaching the maximum number
    /// of ticks.
    pub const SAFETY_MARGIN: u64 = 2_000_000_000;

    /// Overapproximation of the number of ticks the kernel uses to initialise and
    /// reload its state
    pub const INITIALISATION_OVERHEAD: u64 = 1_000_000_000;

    /// The minimum amount of gas for an ethereum transaction.
    pub const BASE_GAS: u64 = crate::CONFIG.gas_transaction_call;
}

pub fn estimate_ticks_for_transaction(transaction: &Transaction) -> u64 {
    match &transaction.content {
        crate::inbox::TransactionContent::Deposit(_) => ticks_of_deposit(),
        crate::inbox::TransactionContent::Ethereum(eth) => ticks_of_gas(eth.gas_limit),
    }
}

fn ticks_of_deposit() -> u64 {
    constants::TICKS_FOR_DEPOSIT + constants::TRANSACTION_OVERHEAD
}

pub fn ticks_of_gas(gas: u64) -> u64 {
    gas.saturating_mul(constants::TICKS_PER_GAS)
        .saturating_add(constants::TRANSACTION_OVERHEAD)
}

/// Check that a transaction can fit inside the tick limit
pub fn estimate_would_overflow(estimated_ticks: u64, transaction: &Transaction) -> bool {
    estimate_ticks_for_transaction(transaction).saturating_add(estimated_ticks)
        > constants::MAX_ALLOWED_TICKS
}

/// An invalid transaction could not be transmitted to the VM, eg. the nonce
/// was wrong, or the signature verification failed.
pub fn ticks_of_invalid_transaction() -> u64 {
    // If the transaction is invalid, only the base cost is considered.
    constants::BASE_GAS
        .saturating_mul(constants::TICKS_PER_GAS)
        .saturating_add(constants::TRANSACTION_OVERHEAD)
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
