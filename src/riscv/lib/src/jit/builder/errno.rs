// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Wrapper for C-style error handling with out-parameters.
//!
//! Any fallible function will return an 'error code' - either
//! `1` for failure or `0` for success.
//!
//! Any returned values are written via 'out-pointers' - and should only be
//! loaded on success. [`Errno`] provides a wrapper for this mechanism,
//! that ensures safety.

use cranelift::codegen::ir;
use cranelift::codegen::ir::InstBuilder;
use cranelift::frontend::FunctionBuilder;

use crate::jit::state_access::JitStateAccess;
use crate::machine_state::memory::MemoryConfig;

/// Wrapper for `Errno`-type returns.
///
/// This is formed of two parts, the 'code', and a output function.
/// The output function will only be called on success - ie any
/// out parameters (with the exception of the exception ptr) - can be assumed
/// to be initialised, if the externally-called function guarantees it.
///
/// *NB* a trait is used, rather than a structure, to allow us to naturally use
/// _unboxed closures_ for the function that returns the output values, without needing
/// to thread extra generics.
pub(crate) trait Errno<T, MC: MemoryConfig, JSA: JitStateAccess> {
    /// Insert exception handling branch that will be triggered at runtime
    /// when `errno` indicates failure.
    ///
    /// The caller of this function is put back into the context of the happy path -
    /// ie, where `errno` indicates success and no exception occurred.
    fn handle(self, builder: &mut super::Builder<'_, MC, JSA>) -> T;
}

impl<T, F, MC: MemoryConfig, JSA: JitStateAccess> Errno<T, MC, JSA> for (ir::Value, F)
where
    F: FnOnce(&mut FunctionBuilder<'_>) -> T,
{
    fn handle(self, builder: &mut super::Builder<'_, MC, JSA>) -> T {
        let errno_code = self.0;

        let on_error = builder.builder.create_block();
        let on_ok = builder.builder.create_block();

        builder
            .builder
            .ins()
            .brif(errno_code, on_error, &[], on_ok, &[]);

        let snapshot = builder.dynamic;

        // Exception succesfully handled, we have a new PC and need to complete
        // the current step.
        builder.builder.switch_to_block(on_error);
        builder.handle_exception();

        builder.builder.seal_block(on_error);

        // The exception has to be handled by the environment. Do not complete
        // the current step; this will be done by the ECall handling mechanism
        builder.builder.switch_to_block(on_ok);
        builder.dynamic = snapshot;

        (self.1)(&mut builder.builder)
    }
}
