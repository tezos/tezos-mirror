// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::inbox::Transaction;

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

    /// Overapproximation of the amount of ticks for a deposit. Should take
    /// everything into account, execution and registering
    pub const TICKS_FOR_DEPOSIT: u64 = 2_000_000;

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

    /// The minimum amount of gas for an ethereum transaction.
    pub const BASE_GAS: u64 = crate::CONFIG.gas_transaction_call;

    /// The maximum gas limit allowed for a transaction. We need to set a limit
    /// on the gas so we can consider the transaction in a reboot. If we don't
    /// set a limit, we could reboot again and again until the transaction
    /// fits in a reboot, which will never happen.
    pub const MAX_TRANSACTION_GAS_LIMIT: u64 = MAX_ALLOWED_TICKS / TICKS_PER_GAS;

    /// Overapproximation of the upper bound of the number of ticks used to
    /// fetch the inbox. Considers an inbox with the size of a full block, and
    /// apply a tick model affine in the size of the inbox.
    pub const FETCH_UPPER_BOUND: u64 = 350_000_000;

    /// Overapproximation of the number of ticks used in kernel initialization
    pub const KERNEL_INITIALIZATION: u64 = 50_000_000;

    /// Overapproximation of the number of ticks the kernel uses to initialise and
    /// reload its state.
    /// TODO: #6091
    /// Hidden hypothesis here: BIP reading is equivalent to fetch (TBV)
    pub const INITIALISATION_OVERHEAD: u64 = FETCH_UPPER_BOUND + KERNEL_INITIALIZATION;

    /// Overapproximation of the upper bound of the number of ticks used to
    /// finalize a block. Considers a block corresponding to an inbox full of
    /// transfers, and apply a tick model affine in the number of tx.
    pub const FINALIZE_UPPER_BOUND: u64 = 150_000_000;

    /// The number of ticks used for storing receipt is overapproximated by
    /// an affine function of the size of the receipt
    pub const RECEIPT_TICKS_COEF: u64 = 960;
    pub const RECEIPT_TICKS_INTERCEPT: u64 = 200_000;

    /// The number of ticks used for storing transactions is overapproximated by
    /// an affine function of the size of the transaction
    pub const TX_OBJ_TICKS_COEF: u64 = 880;
    pub const TX_OBJ_TICKS_INTERCEPT: u64 = 200_000;

    /// The number of ticks used to compute the bloom filter is overapproximated
    /// by an affine function of the size of the bloom
    /// (nb of logs + nb of topics)
    pub const BLOOM_TICKS_INTERCEPT: u64 = 10000;
    pub const BLOOM_TICKS_COEF: u64 = 85000;

    /// The number of ticks used during transaction execution doing something
    /// other than executing an opcode is overapproximated by an affine function
    /// of the size of a transaction object
    pub const TRANSACTION_OVERHEAD_INTERCEPT: u64 = 1_150_000;
    pub const TRANSACTION_OVERHEAD_COEF: u64 = 880;
    pub const TRANSFERT_OBJ_SIZE: u64 = 347;
}

pub fn estimate_ticks_for_transaction(transaction: &Transaction) -> u64 {
    match &transaction.content {
        crate::inbox::TransactionContent::Deposit(_) => {
            ticks_of_deposit(constants::TICKS_FOR_DEPOSIT)
        }
        crate::inbox::TransactionContent::Ethereum(eth) => {
            average_ticks_of_gas(eth.gas_limit)
        }
    }
}

pub fn estimate_remaining_ticks_for_transaction_execution(
    ticks: u64,
    tx_data_size: u64,
) -> u64 {
    constants::MAX_ALLOWED_TICKS
        .saturating_sub(ticks_of_transaction_overhead(tx_data_size))
        .saturating_sub(ticks)
}

fn ticks_of_deposit(resulting_ticks: u64) -> u64 {
    resulting_ticks.saturating_add(constants::TRANSACTION_OVERHEAD)
}

pub fn average_ticks_of_gas(gas: u64) -> u64 {
    gas.saturating_mul(constants::TICKS_PER_GAS)
        .saturating_add(constants::TRANSACTION_OVERHEAD)
}

fn ticks_of_transaction_overhead(tx_data_size: u64) -> u64 {
    // analysis was done using the object size. It is approximated from the
    // data size
    let tx_obj_size = tx_data_size + constants::TRANSFERT_OBJ_SIZE;
    tx_obj_size
        .saturating_mul(constants::TRANSACTION_OVERHEAD_COEF)
        .saturating_add(constants::TRANSACTION_OVERHEAD_INTERCEPT)
}

/// Check that a transaction can fit inside the tick limit
pub fn estimate_would_overflow(estimated_ticks: u64, transaction: &Transaction) -> bool {
    estimate_ticks_for_transaction(transaction).saturating_add(estimated_ticks)
        > constants::MAX_ALLOWED_TICKS
}

/// An invalid transaction could not be transmitted to the VM, eg. the nonce
/// was wrong, or the signature verification failed.
pub fn ticks_of_invalid_transaction(tx_data_size: u64) -> u64 {
    // If the transaction is invalid, only the base cost is considered.
    constants::BASE_GAS
        .saturating_mul(constants::TICKS_PER_GAS)
        .saturating_add(ticks_of_transaction_overhead(tx_data_size))
}

/// Adds the possible overhead this is not accounted during the validation of
/// the transaction. Transaction evaluation (the interpreter) accounts for the
/// ticks itself.
pub fn ticks_of_valid_transaction(
    transaction: &Transaction,
    resulting_ticks: u64,
) -> u64 {
    match &transaction.content {
        crate::inbox::TransactionContent::Ethereum(_) => {
            ticks_of_valid_transaction_ethereum(resulting_ticks, transaction.data_size())
        }
        // Ticks are already spent during the validation of the transaction (see
        // apply.rs).
        crate::inbox::TransactionContent::Deposit(_) => ticks_of_deposit(resulting_ticks),
    }
}

/// A valid transaction is a transaction that could be transmitted to
/// evm_execution. It can succeed (with or without effect on the state)
/// or fail (if the VM encountered an error).
pub fn ticks_of_valid_transaction_ethereum(
    resulting_ticks: u64,
    tx_data_size: u64,
) -> u64 {
    resulting_ticks
        .saturating_add(constants::TICKS_FOR_CRYPTO)
        .saturating_add(ticks_of_transaction_overhead(tx_data_size))
}

pub fn bloom_size(logs: &[tezos_ethereum::Log]) -> usize {
    let mut size = 0;
    // for n in 0..logs.len() {
    for item in logs.iter() {
        size += item.topics.len();
    }
    size
}

pub fn ticks_of_register(receipt_size: u64, obj_size: u64, bloom_size: u64) -> u64 {
    let receipt_ticks: u64 = receipt_size
        .saturating_mul(constants::RECEIPT_TICKS_COEF)
        .saturating_add(constants::RECEIPT_TICKS_INTERCEPT);
    let obj_ticks: u64 = obj_size
        .saturating_mul(constants::TX_OBJ_TICKS_COEF)
        .saturating_add(constants::TX_OBJ_TICKS_INTERCEPT);
    let bloom_ticks: u64 = bloom_size
        .saturating_mul(constants::BLOOM_TICKS_COEF)
        .saturating_add(constants::BLOOM_TICKS_INTERCEPT);
    receipt_ticks
        .saturating_add(obj_ticks)
        .saturating_add(bloom_ticks)
}
