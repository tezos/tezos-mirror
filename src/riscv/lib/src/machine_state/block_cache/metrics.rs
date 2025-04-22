// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! This module implements block metrics.
//!
//! Namely:
//! - the op-codes contained in a block
//! - the number of times it has been constructed and called
//! - whether or not the block is supported by the JIT
//!
//! The macro [`block_metrics`] is used to record metrics.

/// Record a block metric - namely that a block has been
/// - constructed
/// - successfully JIT-compiled
/// - called
///
/// This macro has no effect when the `metrics` feature is disabled.
#[cfg(not(feature = "metrics"))]
macro_rules! block_metrics {
    (hash = $hash:expr, constructed = $block:expr) => {};

    (hash = $hash:expr, record_jitted) => {};

    (hash = $hash:expr, record_called) => {};
}

#[cfg(feature = "metrics")]
pub(crate) use core::block_metrics;

#[cfg(not(feature = "metrics"))]
pub(crate) use block_metrics;

use super::ICallPlaced;
use super::block::Block;
use crate::machine_state::memory::MemoryConfig;
use crate::state::NewState;
use crate::state_backend::EnrichedCell;
use crate::state_backend::ManagerAlloc;
use crate::state_backend::ManagerBase;
use crate::state_backend::ManagerRead;
use crate::storage::Hash;

#[cfg(feature = "metrics")]
#[doc(hidden)]
pub mod core {
    use std::cmp::Ordering;
    use std::collections::BTreeSet;
    use std::collections::HashMap;
    use std::path::Path;

    use super::BlockHash;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::block_cache::block::Block;
    use crate::machine_state::instruction::Instruction;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::MemoryConfig;
    use crate::state_backend::ManagerRead;
    use crate::state_backend::hash::Hash;
    use crate::state_backend::owned_backend::Owned;

    /// Record a block metric - namely that a block has been
    /// - constructed
    /// - successfully JIT-compiled
    /// - called
    ///
    /// This macro has no effect when the `metrics` feature is disabled.
    macro_rules! block_metrics {
        (hash = $hash:expr, constructed = $block:expr) => {
            $crate::machine_state::block_cache::metrics::core::BlockCacheMetrics::with_borrow_mut(
                |bm| bm.record_constructed($hash, $block),
            );
        };

        (hash = $hash:expr, record_jitted) => {
            $crate::machine_state::block_cache::metrics::core::BlockCacheMetrics::with_borrow_mut(
                |bm| bm.record_jitted($hash),
            );
        };

        (hash = $hash:expr, record_called) => {
            $crate::machine_state::block_cache::metrics::core::BlockCacheMetrics::with_borrow_mut(
                |bm| bm.record_called($hash),
            );
        };
    }

    pub(crate) use block_metrics;

    /// Write block metrics to the given file, in the following format:
    /// ```
    /// [OpCodes] | JIT-compiled | times_constructed | times_called
    /// ```
    ///
    /// If `exclude_supported_instructions = true` is passed, any opcode that is supported
    /// by the JIT is not written in the output.
    #[macro_export]
    macro_rules! dump_block_metrics {
        (file = $file:expr, exclude_supported_instructions = $exclude:expr) => {
            $crate::machine_state::block_cache::metrics::core::BlockCacheMetrics::with_borrow(
                |bm| bm.write_to_file($file, !$exclude),
            );
        };
    }

    /// Mapping of blocks (identified by their hash) to their metrics.
    ///
    /// See [`BlockMetrics`].
    #[derive(Default)]
    pub struct BlockCacheMetrics {
        entries: HashMap<Hash, BlockMetrics>,
    }

    impl BlockCacheMetrics {
        pub fn with_borrow_mut<T>(run: impl Fn(&mut BlockCacheMetrics) -> T) -> T {
            thread_local! {
                /// Static variable used by [`block_metrics`] as a place to record metrics.
                static BLOCK_METRICS: std::cell::RefCell<BlockCacheMetrics> = Default::default();
            }

            BLOCK_METRICS.with_borrow_mut(run)
        }

        pub fn with_borrow<T>(run: impl Fn(&BlockCacheMetrics) -> T) -> T {
            Self::with_borrow_mut(|metrics| run(metrics))
        }

        /// Record that the given block has been constructed.
        pub fn record_constructed<MC: MemoryConfig, B: Block<MC, M>, M: ManagerRead>(
            &mut self,
            hash: &Hash,
            block: &B,
        ) {
            if let Some(entry) = self.entries.get_mut(hash) {
                entry.constructed_count += 1;
                return;
            }

            let instr = block
                .instr()
                .iter()
                .map(|i| i.read_stored())
                .collect::<Vec<_>>();

            let metrics = BlockMetrics {
                instr,
                jit_compiled: false,
                called_count: 0,
                constructed_count: 1,
            };

            self.entries.insert(*hash, metrics);
        }

        /// Record that the block identified by `hash` has been
        /// successfully jit-compiled.
        pub fn record_jitted(&mut self, hash: &Hash) {
            let entry = self
                .entries
                .get_mut(hash)
                .expect("all blocks called are first constructed");

            entry.jit_compiled = true;
        }

        /// Record that the block identified by `hash` has been called.
        pub fn record_called(&mut self, hash: &BlockHash) {
            let BlockHash::Runnable(hash) = hash else {
                panic!("Called blocks must be runnable");
            };

            let entry = self
                .entries
                .get_mut(hash)
                .expect("all blocks called are first constructed");

            entry.called_count += 1;
        }

        /// Write block metrics to the given file.
        ///
        /// These are ordered top-down by descending order of most frequently called.
        pub fn write_to_file(
            &self,
            filename: &Path,
            include_supported_instructions: bool,
        ) -> Result<(), Box<dyn std::error::Error>> {
            use std::io::Write;

            let mut stats = BTreeSet::<BlockMetrics>::new();

            for stat in self.entries.values() {
                stats.insert(stat.clone());
            }

            let mut stats_file = std::fs::File::create(filename)?;

            for stats in stats.into_iter().rev() {
                let instr = stats
                    .instr
                    .iter()
                    .map(|instr| instr.opcode)
                    .filter(|opcode| {
                        include_supported_instructions
                            || opcode
                                .to_lowering::<MachineCoreState<M4K, Owned>>()
                                .is_none()
                    })
                    .collect::<Vec<_>>();
                writeln!(
                    &mut stats_file,
                    "{:?} | {} | {} | {}",
                    instr, stats.jit_compiled, stats.constructed_count, stats.called_count
                )?;
            }

            Ok(())
        }
    }

    /// Corresponds to a line in the block metrics output.
    ///
    /// Exclude `jit_compiled` from the implementations of `Eq` and `Ord`,
    /// to ensure consistent ordering between JIT-enabled and Interpreted
    /// metric files (as `jit_compiled` is always false in Interpreted runs).
    #[derive(Default, Clone)]
    pub struct BlockMetrics {
        instr: Vec<Instruction>,
        constructed_count: usize,
        called_count: usize,
        jit_compiled: bool,
    }

    impl PartialEq for BlockMetrics {
        fn eq(&self, other: &Self) -> bool {
            self.called_count == other.called_count
                && self.constructed_count == other.constructed_count
                && Hash::blake2b_hash(&self.instr)
                    .unwrap()
                    .eq(&Hash::blake2b_hash(&other.instr).unwrap())
        }
    }

    impl Eq for BlockMetrics {}

    impl PartialOrd for BlockMetrics {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for BlockMetrics {
        fn cmp(&self, other: &Self) -> Ordering {
            let cmp = self.called_count.cmp(&other.called_count);
            if cmp != Ordering::Equal {
                return cmp;
            }

            let cmp = self.constructed_count.cmp(&other.constructed_count);
            if cmp != Ordering::Equal {
                return cmp;
            }

            Hash::blake2b_hash(self.instr.as_slice())
                .unwrap()
                .cmp(&Hash::blake2b_hash(&other.instr).unwrap())
        }
    }
}

/// Wrapper type that can be used to instrument any `B: Block` with metrics.
pub struct BlockMetrics<B> {
    block: B,
    #[cfg(test)]
    pub(crate) block_hash: BlockHash,
    #[cfg(not(test))]
    block_hash: BlockHash,
}

impl<B: NewState<M>, M: ManagerBase> NewState<M> for BlockMetrics<B> {
    fn new(manager: &mut M) -> Self
    where
        M: ManagerAlloc,
    {
        Self {
            block: B::new(manager),
            block_hash: BlockHash::Dirty,
        }
    }
}

impl<B: Block<MC, M>, MC: MemoryConfig, M: ManagerBase> Block<MC, M> for BlockMetrics<B> {
    type BlockBuilder = B::BlockBuilder;

    fn reset(&mut self)
    where
        M: crate::state_backend::ManagerReadWrite,
    {
        self.block.reset();
        self.block_hash = BlockHash::Dirty;
    }

    fn instr(&self) -> &[crate::state_backend::EnrichedCell<super::ICallPlaced<MC, M>, M>]
    where
        M: crate::state_backend::ManagerRead,
    {
        self.block.instr()
    }

    /// # Safety
    ///
    /// The `block_builder` parameter must always be the same one, for the lifetime of the block.
    unsafe fn run_block(
        &mut self,
        core: &mut crate::machine_state::MachineCoreState<MC, M>,
        instr_pc: crate::machine_state::memory::Address,
        steps: &mut usize,
        block_builder: &mut Self::BlockBuilder,
    ) -> Result<(), crate::traps::EnvironException>
    where
        M: crate::state_backend::ManagerReadWrite,
    {
        if let BlockHash::Dirty = self.block_hash {
            let hash = block_hash(self.block.instr());
            block_metrics!(hash = &hash, constructed = self);

            self.block_hash = BlockHash::Runnable(hash);
        }

        block_metrics!(hash = &self.block_hash, record_called);

        unsafe { self.block.run_block(core, instr_pc, steps, block_builder) }
    }

    fn num_instr(&self) -> usize
    where
        M: crate::state_backend::ManagerRead,
    {
        self.block.num_instr()
    }

    fn struct_ref<'a, F: crate::state_backend::FnManager<crate::state_backend::Ref<'a, M>>>(
        &'a self,
    ) -> crate::state_backend::AllocatedOf<super::block::BlockLayout, F::Output> {
        self.block.struct_ref::<F>()
    }

    fn push_instr(&mut self, instr: crate::machine_state::instruction::Instruction)
    where
        M: crate::state_backend::ManagerReadWrite,
    {
        self.block.push_instr(instr);
        self.block_hash = BlockHash::Dirty;
    }

    fn invalidate(&mut self)
    where
        M: crate::state_backend::ManagerWrite,
    {
        self.block.invalidate();
        self.block_hash = BlockHash::Dirty;
    }

    fn bind(allocated: crate::state_backend::AllocatedOf<super::block::BlockLayout, M>) -> Self
    where
        <M as ManagerBase>::ManagerRoot: crate::state_backend::ManagerReadWrite,
    {
        Self {
            block: B::bind(allocated),
            block_hash: BlockHash::Dirty,
        }
    }

    fn start_block(&mut self)
    where
        M: crate::state_backend::ManagerWrite,
    {
        self.block.start_block();
        self.block_hash = BlockHash::Dirty;
    }
}

impl<B: Clone> Clone for BlockMetrics<B> {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            block_hash: BlockHash::Dirty,
        }
    }
}

/// The hash of a block is by default `Dirty` - ie it may be under construction.
///
/// Only once blocks are made callable, within the specific context of the current backend, is
/// the hash calculated. At this point the block is declared `Runnable`.
#[derive(Debug, PartialEq, Eq)]
pub enum BlockHash {
    /// This block may be under construction.
    ///
    /// In order for any such block to run, it may be made runnable. First by calculating
    /// its block hash and triggering any side effects (such as JIT compilation).
    Dirty,
    /// This block can be run.
    Runnable(Hash),
}

/// Construct a block hash from the contained instructions.
fn block_hash<MC: MemoryConfig, M: ManagerRead>(
    block: &[EnrichedCell<ICallPlaced<MC, M>, M>],
) -> Hash {
    let instr = block
        .iter()
        .map(|i| i.read_ref_stored())
        .collect::<Vec<_>>();

    Hash::blake2b_hash(instr).expect("Hashing instructions always succeeds")
}
