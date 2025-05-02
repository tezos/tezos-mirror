// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>

//! Dispatching of blocks under JIT is done via hot-swappable
//! function pointers.
//!
//! This module exposes wrappers for the style of dispatch and compilation that is done.
//!
//! Currently, this is only 'inline' jit, but will soon be expanded to 'outline' jit also;
//! where 'outline' means any JIT compilation occurs in a separate thread.

use super::DispatchFn;
use super::Jitted;
use crate::jit::JIT;
use crate::jit::JitFn;
use crate::jit::state_access::JitStateAccess;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::memory::MemoryConfig;

/// Dispatch target that wraps a [`DispatchFn`].
///
/// This is the target used for compilation - see [`DispatchCompiler::compile`].
pub struct DispatchTarget<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> {
    fun: std::cell::Cell<DispatchFn<D, MC, M>>,
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> DispatchTarget<D, MC, M> {
    /// Reset the dispatch target to the interpreted dispatch mechanism.
    pub fn reset(&self) {
        self.set(Jitted::run_block_interpreted);
    }

    /// Set the dispatch target to use the given `block_run` function.
    pub fn set(&self, fun: DispatchFn<D, MC, M>) {
        self.fun.set(fun);
    }

    /// Get the dispatch target's current `block_run` function.
    pub fn get(&self) -> DispatchFn<D, MC, M> {
        self.fun.get()
    }
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> Default
    for DispatchTarget<D, MC, M>
{
    fn default() -> Self {
        Self {
            fun: std::cell::Cell::new(Jitted::run_block_interpreted),
        }
    }
}

/// A compiler that can JIT-compile blocks of instructions, and hot-swap the execution of
/// said block in the given dispatch target.
pub trait DispatchCompiler<MC: MemoryConfig, M: JitStateAccess>: Default + Sized {
    /// Compile a block, hot-swapping the `run_block` function contained in `target` in
    /// the process. This could be to an interpreted execution method, and/or jit-compiled
    /// function.
    ///
    /// NB - the hot-swapping of JIT-compiled blocks may occur at any time, and is not
    /// guaranteed to be contained within the call-time of this function. (This is true for
    /// outline jit, especially).
    fn compile(
        &mut self,
        target: &mut DispatchTarget<Self, MC, M>,
        instr: Vec<Instruction>,
    ) -> DispatchFn<Self, MC, M>;
}

impl<MC: MemoryConfig, M: JitStateAccess> DispatchCompiler<MC, M> for JIT<MC, M> {
    fn compile(
        &mut self,
        target: &mut DispatchTarget<Self, MC, M>,
        instr: Vec<Instruction>,
    ) -> DispatchFn<Self, MC, M> {
        let fun = match self.compile(&instr) {
            Some(jitfn) => {
                // Safety: the two function signatures are identical, apart from the first and
                // last parameters. These are both pointers, and ignored by the JitFn.
                //
                // It's therefore safe to cast these to thin-pointers to any type.
                unsafe { std::mem::transmute::<JitFn<MC, M>, DispatchFn<Self, MC, M>>(jitfn) }
            }
            None => Jitted::run_block_not_compiled,
        };

        target.set(fun);

        fun
    }
}
