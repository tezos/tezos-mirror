// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>

//! Dispatching of blocks under JIT is done via hot-swappable
//! function pointers.
//!
//! This module exposes wrappers for the style of dispatch and compilation that is done.
//!
//! Currently, this is only 'inline' jit, but will soon be expanded to 'outline' jit also;
//! where 'outline' means any JIT compilation occurs in a separate thread.

use super::InlineJit;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::memory::MemoryConfig;

/// Dispatch target that wraps a [`DispatchFn`].
///
/// This is the target used for compilation - see [`DispatchCompiler::compile`].
pub struct DispatchTarget<MC: MemoryConfig, M: JitStateAccess> {
    fun: std::cell::Cell<super::DispatchFn<MC, M>>,
}

impl<MC: MemoryConfig, M: JitStateAccess> DispatchTarget<MC, M> {
    /// Reset the dispatch target to the interpreted dispatch mechanism.
    pub fn reset(&self) {
        self.set(InlineJit::run_block_interpreted);
    }

    /// Set the dispatch target to use the given `block_run` function.
    pub fn set(&self, fun: super::DispatchFn<MC, M>) {
        self.fun.set(fun);
    }

    /// Get the dispatch target's current `block_run` function.
    pub fn get(&self) -> super::DispatchFn<MC, M> {
        self.fun.get()
    }
}

impl<MC: MemoryConfig, M: JitStateAccess> Default for DispatchTarget<MC, M> {
    fn default() -> Self {
        Self {
            fun: std::cell::Cell::new(InlineJit::run_block_interpreted),
        }
    }
}
