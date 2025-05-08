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

/// Helper type for ensuring fallible operations are handled correctly.
///
/// The errno is constructed out of three pieces:
/// - whether or not a failure occurred
/// - if yes, the pointer to the exception in memory that has been written with the failure kind
/// - if no, a handler to load any state that was returned in `out-params` that is now safe to
///   access.
///
/// The only way to access the values that will be returned on success, is via the
/// [`Errno::handle`] method.
pub(crate) struct ErrnoImpl<T, F>
where
    F: FnOnce(&mut FunctionBuilder<'_>) -> T,
{
    errno: ir::Value,
    exception_ptr: ir::Value,
    on_ok: F,
}

impl<T, F> ErrnoImpl<T, F>
where
    F: FnOnce(&mut FunctionBuilder<'_>) -> T,
{
    /// Construct a new `Errno` that must be handled.
    pub(crate) fn new(errno: ir::Value, exception_ptr: ir::Value, on_ok: F) -> Self {
        Self {
            errno,
            exception_ptr,
            on_ok,
        }
    }
}

impl<T, F, MC: MemoryConfig, JSA: JitStateAccess> Errno<T, MC, JSA> for ErrnoImpl<T, F>
where
    F: FnOnce(&mut FunctionBuilder<'_>) -> T,
{
    fn handle(self, builder: &mut super::Builder<'_, MC, JSA>) -> T {
        let errno_code = self.errno;

        let on_error = builder.builder.create_block();
        let on_ok = builder.builder.create_block();

        builder
            .builder
            .ins()
            .brif(errno_code, on_error, &[], on_ok, &[]);

        let snapshot = builder.dynamic;

        builder.builder.switch_to_block(on_error);
        // Handle the exception in the 'error occurred' block
        builder.handle_exception(self.exception_ptr);

        builder.builder.seal_block(on_error);

        // Go back to the 'no error occurred' block, any returned
        // values are ok to load
        builder.builder.switch_to_block(on_ok);
        builder.dynamic = snapshot;

        (self.on_ok)(&mut builder.builder)
    }
}

/// Helper type for producing the required IR for the address alignment check at the beginning
/// of an atomic operation.
///
/// The only way to access the values that will be returned on success, is via the
/// [`Errno::handle`] method.
pub(crate) struct AtomicAccessGuard {
    errno: ir::Value,
    address: ir::Value,
}

impl AtomicAccessGuard {
    /// Construct a new `Errno` that must be handled.
    pub(crate) fn new(errno: ir::Value, address: ir::Value) -> Self {
        Self { errno, address }
    }
}

impl<MC: MemoryConfig, JSA: JitStateAccess> Errno<(), MC, JSA> for AtomicAccessGuard {
    fn handle(self, builder: &mut super::Builder<'_, MC, JSA>) {
        let error_branch = builder.builder.create_block();
        let fallthrough = builder.builder.create_block();

        builder
            .builder
            .ins()
            .brif(self.errno, error_branch, &[], fallthrough, &[]);

        // both IR blocks need access to the dynamic values at this point in time.
        // These are modified by the jump to the end block below, and possibly by the
        // `on_branching` function.
        let snapshot = builder.dynamic;

        builder.builder.switch_to_block(error_branch);

        let exception_ptr = builder
            .jsa_call
            .raise_store_amo_access_fault_exception(&mut builder.builder, self.address);
        builder.handle_exception(exception_ptr);

        builder.builder.seal_block(error_branch);

        // Restore the dynamic values to the branching point, for the fallthrough block.
        builder.dynamic = snapshot;
        builder.builder.switch_to_block(fallthrough)
    }
}
