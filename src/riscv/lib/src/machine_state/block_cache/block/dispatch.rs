// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>

//! Dispatching of blocks under JIT is done via hot-swappable
//! function pointers.
//!
//! This module exposes wrappers for the style of dispatch and compilation that is done.
//!
//! Currently, this is only 'inline' jit, but will soon be expanded to 'outline' jit also;
//! where 'outline' means any JIT compilation occurs in a separate thread.

use std::marker::PhantomData;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::mpsc;
use std::sync::mpsc::Sender;

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
    /// Function pointer stored as an atomic usize.
    ///
    /// This will allow the `fun` to be updated from a background thread.
    /// See <https://doc.rust-lang.org/std/primitive.fn.html#casting-to-and-from-integers> for
    /// considerations taken whilst converting pointer <--> usize.
    fun: Arc<AtomicUsize>,
    called_times: usize,
    _pd: PhantomData<(D, MC, M)>,
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> DispatchTarget<D, MC, M> {
    /// Reset the dispatch target to the interpreted dispatch mechanism.
    pub fn reset(&mut self) {
        // in resetting the block, we must allocated a new Arc<AtomicUsize>.
        //
        // If we just reset the current arc, outline jit could update it from the background thread
        // after reset it - meaning a reset/under construction block could now have a jitted function for
        // a completely different set of instructions.
        self.fun = Arc::new(AtomicUsize::new(
            Jitted::<D, MC, M>::run_block_interpreted as usize,
        ));

        self.called_times = 0;
    }

    /// Set the dispatch target to use the given `block_run` function.
    pub fn set(&self, fun: DispatchFn<D, MC, M>) {
        // casting a function pointer as usize is ok to do.
        let fun = fun as usize;

        // store using Release ordering - any subsequent loading with Acquire will see the new ptr.
        self.fun.store(fun, Ordering::Release);
    }

    /// Get the dispatch target's current `block_run` function.
    pub fn get(&self) -> DispatchFn<D, MC, M> {
        // load using Acquire ordering - so that it will see the previous store which was with
        // Release.
        let fun = self.fun.load(Ordering::Acquire);

        // to avoid problematic integer -> pointer conversion, we must cast it as a pointer first.
        let fun = fun as *const ();

        // Safety: the pointer is indeed a function pointer with an ABI matching `DispatchFn`.
        unsafe { std::mem::transmute::<*const (), DispatchFn<D, MC, M>>(fun) }
    }
}

impl<D: DispatchCompiler<MC, M>, MC: MemoryConfig, M: JitStateAccess> Default
    for DispatchTarget<D, MC, M>
{
    fn default() -> Self {
        Self {
            fun: Arc::new(AtomicUsize::new(
                Jitted::<D, MC, M>::run_block_interpreted as usize,
            )),
            called_times: 0,
            _pd: PhantomData,
        }
    }
}

/// A compiler that can JIT-compile blocks of instructions, and hot-swap the execution of
/// said block in the given dispatch target.
pub trait DispatchCompiler<MC: MemoryConfig, M: JitStateAccess>: Default + Sized {
    /// Whether compilation should be attempted for the block.
    fn should_compile(&self, target: &mut DispatchTarget<Self, MC, M>) -> bool;

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
    #[inline]
    fn should_compile(&self, _target: &mut DispatchTarget<Self, MC, M>) -> bool {
        // every block must be compiled immediately for inline jit, as it's used for testing
        // jit compatibility with interpreted mode.
        true
    }

    fn compile(
        &mut self,
        target: &mut DispatchTarget<Self, MC, M>,
        instr: Vec<Instruction>,
    ) -> DispatchFn<Self, MC, M> {
        let fun = match self.compile(&instr) {
            Some(jitfn) => {
                // Safety: the two function signatures are identical, apart from the first and
                // last parameters. These are both thin-pointers, and ignored by the JitFn.
                //
                // It's therefore safe to cast this function pointer to an identical ABI, where
                // this first and last parameter are thin-references to any value. This is the
                // case for both `Jitted` and `Jitted::BlockBuilder` which are both Sized.
                //
                // See <https://doc.rust-lang.org/std/primitive.fn.html#abi-compatibility> for more
                // information on ABI compatability.
                unsafe { std::mem::transmute::<JitFn<MC, M>, DispatchFn<Self, MC, M>>(jitfn) }
            }
            None => Jitted::run_block_not_compiled,
        };

        target.set(fun);

        fun
    }
}

/// JIT compiler for blocks that performs compilation in a
/// background thread.
pub struct OutlineCompiler<MC: MemoryConfig, M: JitStateAccess> {
    // We will not touch the jit from the execution thread, however we must maintain
    // a reference to it - to ensure it is not dropped before we are done with execution,
    // even if the background compilation thread panics.
    #[expect(unused)]
    jit: Arc<Mutex<JIT<MC, M>>>,
    sender: Sender<CompilationRequest>,
}

impl<MC: MemoryConfig + Send, M: JitStateAccess + Send + 'static> OutlineCompiler<MC, M> {
    fn new() -> Self {
        let (sender, receiver) = mpsc::channel();
        let jit: Arc<Mutex<JIT<MC, M>>> = Default::default();

        let compiler = Self {
            jit: jit.clone(),
            sender,
        };

        std::thread::spawn(move || {
            {
                let mut jit = jit.lock().expect("Only this thread locks the JIT");

                while let Ok(msg) = receiver.recv() {
                    if let Some(jitfn) = jit.compile(&msg.instr) {
                        // Safety: this function will be retrieved as a DispatchFn, rather than a
                        // JitFn. The two function signatures are identical, apart from the first and
                        // last parameters. These are both thin-pointers, and ignored by the JitFn.
                        //
                        // It's therefore safe to cast this function pointer to an identical ABI, where
                        // this first and last parameter are thin-references to any value. This is the
                        // case for both `Jitted` and `Jitted::BlockBuilder` which are both Sized.
                        //
                        // See <https://doc.rust-lang.org/std/primitive.fn.html#abi-compatibility> for more
                        // information on ABI compatability.
                        msg.fun.store(jitfn as usize, Ordering::Release);
                    };
                }
            }
            // because we used blocking recv with an asynchronous channel, this only fails when the
            // other end of the channel has been dropped.
            //
            // This means the BlockBuilder has been dropped - and thus execution has stopped.
            // We are therefore safe to drop the JIT.
        });

        compiler
    }
}

impl<MC: MemoryConfig + Send, M: JitStateAccess + Send + 'static> Default
    for OutlineCompiler<MC, M>
{
    fn default() -> Self {
        Self::new()
    }
}

impl<MC: MemoryConfig + Send, M: JitStateAccess + Send + 'static> DispatchCompiler<MC, M>
    for OutlineCompiler<MC, M>
{
    fn should_compile(&self, target: &mut DispatchTarget<Self, MC, M>) -> bool {
        const COMPILATION_THRESHOLD: usize = 1000;

        target.called_times += 1;
        target.called_times > COMPILATION_THRESHOLD
    }

    fn compile(
        &mut self,
        target: &mut DispatchTarget<Self, MC, M>,
        instr: Vec<Instruction>,
    ) -> DispatchFn<Self, MC, M> {
        let request = CompilationRequest {
            instr,
            fun: target.fun.clone(),
        };

        // This will always succeed, unless the compilation thread has panicked
        // (as this would result in the receiving end of the channel being closed).
        //
        // If it has, execution must still continue - but everything will fallback
        // to interpreted mode.
        //
        // NB - any blocks already JIT compiled are safe to keep calling, as the
        // data behind the mutex (the JIT) is kept alive for as long as we maintain
        // our reference to it, despite the lock itself being poisoned.
        let _ = self.sender.send(request);

        let fun = Jitted::run_block_not_compiled;
        target.set(fun);
        fun
    }
}

struct CompilationRequest {
    instr: Vec<Instruction>,
    fun: Arc<AtomicUsize>,
}
