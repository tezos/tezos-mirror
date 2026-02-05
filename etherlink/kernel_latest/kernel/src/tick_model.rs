// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_ethereum::transaction::IndexedLog;

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

    /// The number of ticks to parse a blueprint chunk
    pub const TICKS_FOR_BLUEPRINT_CHUNK_SIGNATURE: u64 = 27_000_000;
    pub const TICKS_FOR_BLUEPRINT_INTERCEPT: u64 = 25_000_000;

    /// The number of ticks to parse a transaction from the delayed bridge
    pub const TICKS_FOR_DELAYED_MESSAGES: u64 = 1_380_000;

    /// Number of ticks used to parse deposits
    pub const TICKS_PER_DEPOSIT_PARSING: u64 = 1_500_000;
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
