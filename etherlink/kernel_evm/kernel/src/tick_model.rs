// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_ethereum::transaction::IndexedLog;

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

    /// Overapproximation using an upper bound of the number of ticks needed to
    /// store a queue before rebooting.
    /// The bound is calculated using a model linear in size of encoding of the
    /// queue, and applying to a queue of size 512kB, which correspond to a full
    /// inbox. This value doesn't take into account the facts that the encoding
    /// is not the same in the inbox, and that some transactions have been
    /// executed already.
    pub const QUEUE_STORING_UPPER_BOUND: u64 = 1_000_000_000;

    /// Safety margin the kernel enforce to avoid approaching the maximum number
    /// of ticks.
    pub const SAFETY_MARGIN: u64 = QUEUE_STORING_UPPER_BOUND + 1_000_000_000;

    /// The minimum amount of gas for an ethereum transaction.
    pub const BASE_GAS: u64 = crate::CONFIG.gas_transaction_call;

    /// Overapproximation of the upper bound of the number of ticks used to
    /// fetch the inbox. Considers an inbox with the size of a full block, and
    /// apply a tick model affine in the size of the inbox.
    /// NOT USED BUT KEPT FOR DOCUMENTATION
    // pub const FETCH_UPPER_BOUND: u64 = 350_000_000;

    /// Overapproximation of the upper bound of the number of ticks used to
    /// read the queue from storage. Considers a queue with the same size as the
    /// maximum inbox
    /// size (512kB) and apply a tick model affine in the size
    /// of the queue.
    pub const QUEUE_READ_UPPER_BOUND: u64 = 500_000_000;

    /// Overapproximation of the number of ticks used in kernel initialization
    pub const KERNEL_INITIALIZATION: u64 = 50_000_000;

    /// Overapproximation of the number of ticks the kernel uses to initialise and
    /// reload its state. Uses the maximum between reading the queue and reading
    /// the inbox, and adds the kernel initialization.
    /// max(FETCH_UPPER_BOUND, QUEUE_READ_UPPER_BOUND)
    pub const INITIALISATION_OVERHEAD: u64 =
        QUEUE_READ_UPPER_BOUND + KERNEL_INITIALIZATION;

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

/// Estimation of the number of ticks the kernel can safely spend in the
/// execution of the opcodes.
pub fn estimate_remaining_ticks_for_transaction_execution(
    ticks: u64,
    tx_data_size: u64,
) -> u64 {
    constants::MAX_ALLOWED_TICKS
        .saturating_sub(ticks_of_transaction_overhead(tx_data_size))
        .saturating_sub(ticks)
}

/// Estimation of the number of ticks used up for executing a transaction
/// besides executing the opcodes.
fn ticks_of_transaction_overhead(tx_data_size: u64) -> u64 {
    // analysis was done using the object size. It is approximated from the
    // data size
    let tx_obj_size = tx_data_size + constants::TRANSFERT_OBJ_SIZE;
    tx_obj_size
        .saturating_mul(constants::TRANSACTION_OVERHEAD_COEF)
        .saturating_add(constants::TRANSACTION_OVERHEAD_INTERCEPT)
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
/// ticks itself [resulting_ticks].
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
        crate::inbox::TransactionContent::Deposit(_) => resulting_ticks,
    }
}

/// A valid transaction is a transaction that could be transmitted to
/// evm_execution. It can succeed (with or without effect on the state)
/// or fail (if the VM encountered an error).
fn ticks_of_valid_transaction_ethereum(resulting_ticks: u64, tx_data_size: u64) -> u64 {
    resulting_ticks
        .saturating_add(constants::TICKS_FOR_CRYPTO)
        .saturating_add(ticks_of_transaction_overhead(tx_data_size))
}

/// The bloom size is the number of logs plus the size of each one, ie the nb of
/// times a new value is added to the bloom filter. See [logs_to_bloom].
pub fn bloom_size(logs: &[IndexedLog]) -> usize {
    let mut size = logs.len();
    for item in logs.iter() {
        size += item.log.topics.len();
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
