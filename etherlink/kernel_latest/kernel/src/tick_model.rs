// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_ethereum::transaction::IndexedLog;

use crate::transaction::Transaction;

use self::constants::TICKS_FOR_CRYPTO;

/// Tick model constants
///
/// Some of the following values were estimated using benchmarking, and should
/// be updated only when the benchmarks are executed.
/// This doesn't apply to inherited constants from the PVM, e.g. maximum
/// number of reboots.
pub mod constants {

    /// Maximum of gas allowed for a transaction.
    /// Comes from the block limit, defined in EIP-1559 as 2 * gas target
    pub const MAXIMUM_GAS_LIMIT: u64 = 30_000_000;

    /// Maximum number of ticks for a kernel run.
    /// Order of magnitude lower than the limit set by the PVM to provide
    /// security margin.
    pub(crate) const MAX_TICKS: u64 = 30_000_000_000;

    /// Maximum number of allowed ticks for a kernel run. We consider a safety
    /// margin and an incompressible initilisation overhead.
    pub const MAX_ALLOWED_TICKS: u64 = MAX_TICKS;

    /// Maximum number of reboots for a level as set by the PVM.
    pub(crate) const _MAX_NUMBER_OF_REBOOTS: u32 = 1_000;

    /// Overapproximation of the amount of ticks for a deposit. Should take
    /// everything into account, execution and registering
    pub const TICKS_FOR_DEPOSIT: u64 = 2_000_000;

    /// Overapproximation of the amount of ticks per gas unit.
    pub const TICKS_PER_GAS: u64 = 2000;

    // Overapproximation of ticks used in signature verification.
    pub const TICKS_FOR_CRYPTO: u64 = 25_000_000;

    /// The minimum amount of gas for an ethereum transaction.
    pub const BASE_GAS: u64 = 21_000;

    /// Overapproximation of the upper bound of the number of ticks used to
    /// finalize a block. Considers a block corresponding to an inbox full of
    /// transfers, and apply a tick model affine in the number of tx.
    pub const FINALIZE_UPPER_BOUND: u64 = 150_000_000;

    /// The number of ticks used during transaction execution doing something
    /// other than executing an opcode is overapproximated by an affine function
    /// of the size of a transaction object
    pub const TRANSACTION_OVERHEAD_INTERCEPT: u64 = 1_150_000;
    pub const TRANSACTION_OVERHEAD_COEF: u64 = 880;
    pub const TRANSFERT_OBJ_SIZE: u64 = 347;

    pub const TRANSACTION_HASH_INTERCEPT: u64 = 200_000;
    pub const TRANSACTION_HASH_COEF: u64 = 1400;

    /// The number of ticks to parse a blueprint chunk
    pub const TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE: u64 = 27_000_000;
    pub const TICKS_FOR_BLUEPRINT_INTERCEPT: u64 = 25_000_000;

    /// The number of ticks to parse a transaction from the delayed bridge
    pub const TICKS_FOR_DELAYED_MESSAGES: u64 = 1_380_000;

    /// Number of ticks used to parse deposits
    pub const TICKS_PER_DEPOSIT_PARSING: u64 = 1_500_000;
}

/// Estimation of the number of ticks used up for executing a transaction
/// besides executing the opcodes.
fn ticks_of_transaction_overhead(tx_data_size: u64) -> u64 {
    // analysis was done using the object size. It is approximated from the
    // data size
    let tx_obj_size = tx_data_size + constants::TRANSFERT_OBJ_SIZE;
    let tx_hash = tx_data_size
        .saturating_mul(constants::TRANSACTION_HASH_COEF)
        .saturating_add(constants::TRANSACTION_HASH_INTERCEPT);
    tx_obj_size
        .saturating_mul(constants::TRANSACTION_OVERHEAD_COEF)
        .saturating_add(constants::TRANSACTION_OVERHEAD_INTERCEPT)
        .saturating_add(tx_hash)
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
    use crate::transaction::TransactionContent::*;

    match &transaction.content {
        Ethereum(_) | EthereumDelayed(_) => {
            ticks_of_valid_transaction_ethereum(resulting_ticks, transaction.data_size())
        }
        // Ticks are already spent during the validation of the transaction (see
        // apply.rs).
        Deposit(_) | FaDeposit(_) | TezosDelayed(_) => resulting_ticks,
    }
}

/// A valid transaction is a transaction that could be transmitted to
/// evm_execution. It can succeed (with or without effect on the state)
/// or fail (if the VM encountered an error).
fn ticks_of_valid_transaction_ethereum(resulting_ticks: u64, tx_data_size: u64) -> u64 {
    resulting_ticks
        .saturating_add(TICKS_FOR_CRYPTO)
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

pub fn maximum_ticks_for_sequencer_chunk() -> u64 {
    constants::TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE
}
