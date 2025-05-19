// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! JIT-compiled blocks must be able to interact with the
//! RISC-V [`MachineCoreState`] passed to them.
//!
//! In Cranelift, this works in two stages.
//!
//! First, the `extern "C"` function pointers must be
//! registered as external symbols in the [jit builder] & the corresponding signatures declared
//! in the [jit module]. This allows generated code to link with these functions.
//!
//! The second step occurs _during jit compilation_ itself. The linked functions must be re-declared
//! within the [function builder] itself. This then allows for a [direct function call] to be issued,
//! which will indeed perform the function call at runtime.
//!
//! [jit builder]: JITBuilder
//! [jit module]: cranelift_jit::JITModule
//! [function builder]: cranelift::frontend::FunctionBuilderContext
//! [direct function call]: cranelift::codegen::ir::InstBuilder::call

mod abi;
mod stack;

use std::marker::PhantomData;
use std::mem::MaybeUninit;

use abi::AbiCall;
use cranelift::codegen::ir;
use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::ir::Type;
use cranelift::codegen::ir::Value;
use cranelift::codegen::ir::types::I8;
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::MemFlags;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::FuncId;
use cranelift_module::Module;
use cranelift_module::ModuleResult;

use super::builder::X64;
use super::builder::errno::Errno;
use super::builder::errno::ErrnoImpl;
use crate::instruction_context::ICB;
use crate::instruction_context::LoadStoreWidth;
use crate::machine_state::MachineCoreState;
use crate::machine_state::memory::Address;
use crate::machine_state::memory::MemoryConfig;
use crate::machine_state::registers::NonZeroXRegister;
use crate::machine_state::registers::XRegisters;
use crate::machine_state::registers::XValue;
use crate::state_backend::ManagerReadWrite;
use crate::state_backend::owned_backend::Owned;
use crate::state_backend::proof_backend::ProofGen;
use crate::traps::EnvironException;
use crate::traps::Exception;

macro_rules! register_jsa_functions {
    ($($name:ident => ($field:path, $fn:path)),* $(,)?) => {
        /// Register state access symbols in the builder.
        pub(super) fn register_jsa_symbols<MC: MemoryConfig, JSA: JitStateAccess>(
            builder: &mut JITBuilder,
        ) {
            $(builder.symbol(stringify!($field), $field as *const u8);)*
        }

        /// Identifications of globally imported [`JitStateAccess`] methods.
        pub(super) struct JsaImports<MC: MemoryConfig, JSA: JitStateAccess> {
            $(
                pub $name: FuncId,
            )*
            _pd: PhantomData<(MC, JSA)>,
        }

        impl<MC: MemoryConfig, JSA: JitStateAccess> JsaImports<MC, JSA> {
            /// Register external functions within the JIT Module.
            pub(super) fn declare_in_module(module: &mut JITModule) -> ModuleResult<Self> {
                let ptr_type = module.target_config().pointer_type();
                let call_conv = module.target_config().default_call_conv;

                $(
                    let abi = $fn($field);
                    let $name = abi.declare_function(module, stringify!($field), ptr_type, call_conv)?;
                )*

                Ok(Self {
                    $(
                        $name,
                    )*
                    _pd: PhantomData,
                })
            }
        }
    };
}

register_jsa_functions!(
    pc_write => (JSA::pc_write::<MC>, AbiCall<2>::args),
    xreg_read => (JSA::xregister_read::<MC>, AbiCall<2>::args),
    xreg_write => (JSA::xregister_write::<MC>, AbiCall<3>::args),
    handle_exception => (JSA::handle_exception::<MC>, AbiCall<4>::args),
    raise_illegal_instruction_exception => (JSA::raise_illegal_instruction_exception, AbiCall<1>::args),
    raise_store_amo_access_fault_exception => (JSA::raise_store_amo_access_fault_exception, AbiCall<2>::args),
    ecall_from_mode => (JSA::ecall::<MC>, AbiCall<2>::args),
    memory_store => (JSA::memory_store::<MC>, AbiCall<5>::args),
    memory_load => (JSA::memory_load::<MC>, AbiCall::<6>::args)
);

/// State Access that a JIT-compiled block may use.
///
/// In future, this will come in two parts:
/// - `extern "C"` functions that can be registered in the JIT module
/// - a way of calling those functions from within JIT-compiled code
pub trait JitStateAccess: ManagerReadWrite {
    /// Update the instruction pc in the state.
    extern "C" fn pc_write<MC: MemoryConfig>(core: &mut MachineCoreState<MC, Self>, pc: u64) {
        core.hart.pc.write(pc)
    }
    /// Read the value of the given [`NonZeroXRegister`].
    extern "C" fn xregister_read<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        reg: NonZeroXRegister,
    ) -> XValue {
        core.hart.xregisters.read_nz(reg)
    }

    /// Write the given value to the given [`NonZeroXRegister`].
    extern "C" fn xregister_write<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        reg: NonZeroXRegister,
        val: XValue,
    ) {
        core.hart.xregisters.write_nz(reg, val)
    }

    /// Handle an [`Exception`].
    ///
    /// If the exception is succesfully handled, the
    /// `current_pc` is updated to the new value, and returns true. The `current_pc`
    /// remains initialised to its previous value otherwise.
    ///
    /// If the exception needs to be treated by the execution environment,
    /// `result` is updated with the `EnvironException` and `false` is
    /// returned.
    ///
    /// # Panics
    ///
    /// Panics if the exception does not have `Some(_)` value.
    ///
    /// See [`MachineCoreState::address_on_exception`].
    extern "C" fn handle_exception<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        current_pc: &mut Address,
        exception: &Exception,
        result: &mut Result<(), EnvironException>,
    ) -> bool {
        let res = core.address_on_exception(*exception, *current_pc);

        match res {
            Err(e) => {
                *result = Err(e);
                false
            }
            Ok(address) => {
                *current_pc = address;
                true
            }
        }
    }

    /// Raise an [`Exception::IllegalInstruction`].
    ///
    /// Writes the instruction to the given exception memory, after which it would be safe to
    /// assume it is initialised.
    extern "C" fn raise_illegal_instruction_exception(exception_out: &mut MaybeUninit<Exception>) {
        exception_out.write(Exception::IllegalInstruction);
    }

    /// Raise an [`Exception::StoreAMOAccessFault`].
    ///
    /// Writes the instruction to the given exception memory, after which it would be safe to
    /// assume it is initialised.
    extern "C" fn raise_store_amo_access_fault_exception(
        exception_out: &mut MaybeUninit<Exception>,
        address: u64,
    ) {
        exception_out.write(Exception::StoreAMOAccessFault(address));
    }

    /// Raise the appropriate environment-call exception given the current machine mode.
    ///
    /// Writes the exception to the given exception memory, after which it would be safe to
    /// assume it is initialised.
    extern "C" fn ecall<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        exception_out: &mut MaybeUninit<Exception>,
    ) {
        exception_out.write(core.hart.run_ecall());
    }

    /// Store the lowest `width` bytes of the given value to memory, at the physical address.
    ///
    /// If the store is successful, `false` is returned to indicate no exception handling is necessary.
    ///
    /// If the store fails (due to out of bouds etc) then an exception will be written
    /// to `exception_out` and `true` returned to indicate exception handling will be necessary.
    ///
    /// # Panics
    ///
    /// Panics if the `width` passed is not a supported [`LoadStoreWidth`].
    extern "C" fn memory_store<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        address: u64,
        value: u64,
        width: u8,
        exception_out: &mut MaybeUninit<Exception>,
    ) -> bool {
        let Some(width) = LoadStoreWidth::new(width) else {
            panic!("The given width {width} is not a supported LoadStoreWidth");
        };

        match <MachineCoreState<MC, Self> as ICB>::main_memory_store(core, address, value, width) {
            Ok(()) => false,
            Err(exception) => {
                exception_out.write(exception);
                true
            }
        }
    }

    /// Load `width` bytes from memory, at the physical address, into lowest `width` bytes of an
    /// `XValue`, with (un)signed extension.
    ///
    /// If the load is successful, `false` is returned to indicate no exception handling is
    /// necessary.
    ///
    /// If the load fails (due to out of bouds etc) then an exception will be written
    /// to `exception_out` and `true` returned to indicate exception handling will be necessary.
    ///
    /// # Panics
    ///
    /// Panics if the `width` passed is not a supported [`LoadStoreWidth`].
    extern "C" fn memory_load<MC: MemoryConfig>(
        core: &mut MachineCoreState<MC, Self>,
        address: u64,
        width: u8,
        signed: bool,
        xval_out: &mut MaybeUninit<XValue>,
        exception_out: &mut MaybeUninit<Exception>,
    ) -> bool {
        let Some(width) = LoadStoreWidth::new(width) else {
            panic!("The given width {width} is not a supported LoadStoreWidth");
        };

        match <MachineCoreState<MC, Self> as ICB>::main_memory_load(core, address, signed, width) {
            Ok(value) => {
                xval_out.write(value);
                false
            }
            Err(exception) => {
                exception_out.write(exception);
                true
            }
        }
    }

    // -------------
    // IR Generation
    // -------------

    /// Emit the required IR to read the value from the given xregister.
    fn ir_xreg_read<MC: MemoryConfig>(
        jsa_calls: &mut JsaCalls<'_, MC, Self>,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
    ) -> X64 {
        let xreg_read = jsa_calls.xreg_read.get_or_insert_with(|| {
            jsa_calls
                .module
                .declare_func_in_func(jsa_calls.imports.xreg_read, builder.func)
        });
        let reg = builder.ins().iconst(I8, reg as i64);
        let call = builder.ins().call(*xreg_read, &[core_ptr, reg]);
        X64(builder.inst_results(call)[0])
    }

    /// Emit the required IR to write the value to the given xregister.
    fn ir_xreg_write<MC: MemoryConfig>(
        jsa_calls: &mut JsaCalls<'_, MC, Self>,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
        value: X64,
    ) {
        let xreg_write = jsa_calls.xreg_write.get_or_insert_with(|| {
            jsa_calls
                .module
                .declare_func_in_func(jsa_calls.imports.xreg_write, builder.func)
        });
        let reg = builder.ins().iconst(I8, reg as i64);
        builder.ins().call(*xreg_write, &[core_ptr, reg, value.0]);
    }
}

impl JitStateAccess for Owned {
    fn ir_xreg_read<MC: MemoryConfig>(
        _jsa_calls: &mut JsaCalls<'_, MC, Self>,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
    ) -> X64 {
        let offset = std::mem::offset_of!(MachineCoreState<MC, Self>, hart.xregisters)
            + XRegisters::<Owned>::xregister_offset(reg);

        // memory access corresponds directly to the xregister value
        // - known to be aligned and non-trapping
        let val = builder
            .ins()
            .load(ir::types::I64, MemFlags::trusted(), core_ptr, offset as i32);

        X64(val)
    }

    fn ir_xreg_write<MC: MemoryConfig>(
        _jsa_calls: &mut JsaCalls<'_, MC, Self>,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
        value: X64,
    ) {
        let offset = std::mem::offset_of!(MachineCoreState<MC, Self>, hart.xregisters)
            + XRegisters::<Owned>::xregister_offset(reg);

        // memory access corresponds directly to the xregister value
        // - known to be aligned and non-trapping
        builder
            .ins()
            .store(MemFlags::trusted(), value.0, core_ptr, offset as i32);
    }
}

impl<M: ManagerReadWrite> JitStateAccess for ProofGen<M> {}

/// References to locally imported [`JitStateAccess`] methods, used to directly call
/// these accessor methods in the JIT-compilation context.
pub struct JsaCalls<'a, MC: MemoryConfig, JSA: JitStateAccess> {
    module: &'a mut JITModule,
    imports: &'a JsaImports<MC, JSA>,
    ptr_type: Type,
    pc_write: Option<FuncRef>,
    xreg_read: Option<FuncRef>,
    xreg_write: Option<FuncRef>,
    handle_exception: Option<FuncRef>,
    raise_illegal_instruction_exception: Option<FuncRef>,
    raise_store_amo_access_fault_exception: Option<FuncRef>,
    ecall_from_mode: Option<FuncRef>,
    memory_store: Option<FuncRef>,
    memory_load: Option<FuncRef>,
    _pd: PhantomData<(MC, JSA)>,
}

impl<'a, MC: MemoryConfig, JSA: JitStateAccess> JsaCalls<'a, MC, JSA> {
    /// Wrapper to simplify calling JSA methods from within the function under construction.
    pub(super) fn func_calls(
        module: &'a mut JITModule,
        imports: &'a JsaImports<MC, JSA>,
        ptr_type: Type,
    ) -> Self {
        Self {
            module,
            imports,
            ptr_type,
            pc_write: None,
            xreg_read: None,
            xreg_write: None,
            handle_exception: None,
            raise_illegal_instruction_exception: None,
            raise_store_amo_access_fault_exception: None,
            ecall_from_mode: None,
            memory_store: None,
            memory_load: None,
            _pd: PhantomData,
        }
    }

    /// Emit the required IR to set the pc to the given value.
    pub(super) fn pc_write(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        pc_val: X64,
    ) {
        let pc_write = self.pc_write.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.pc_write, builder.func)
        });
        builder.ins().call(*pc_write, &[core_ptr, pc_val.0]);
    }

    /// Emit the required IR to call `handle_exception`.
    ///
    /// # Panics
    ///
    /// The call to `handle_exception` will panic (at runtime) if no exception
    /// has occurred so-far in the JIT-compiled function, if the error-handling
    /// code is triggerred.
    pub(super) fn handle_exception(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        exception_ptr: Value,
        result_ptr: Value,
        current_pc: X64,
    ) -> ExceptionHandledOutcome {
        let handle_exception = self.handle_exception.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.handle_exception, builder.func)
        });

        let pc_slot = stack::Slot::<stack::Address>::new(self.ptr_type, builder);
        pc_slot.store(builder, current_pc.0);

        let pc_ptr = pc_slot.ptr(builder);

        let call = builder.ins().call(*handle_exception, &[
            core_ptr,
            pc_ptr,
            exception_ptr,
            result_ptr,
        ]);

        let handled = builder.inst_results(call)[0];
        // Safety: the pc is initialised prior to the call, and is guaranteed to
        // remain initialised regardless of the result of external call.
        let new_pc = unsafe { pc_slot.load(builder) };

        ExceptionHandledOutcome {
            handled,
            new_pc: X64(new_pc),
        }
    }

    /// Emit the required IR to call `raise_illegal_exception`.
    ///
    /// This returns an initialised pointer to the exception.
    pub(super) fn raise_illegal_instruction_exception(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
    ) -> Value {
        let exception_slot = stack::Slot::<Exception>::new(self.ptr_type, builder);
        let exception_ptr = exception_slot.ptr(builder);

        let raise_illegal = self
            .raise_illegal_instruction_exception
            .get_or_insert_with(|| {
                self.module.declare_func_in_func(
                    self.imports.raise_illegal_instruction_exception,
                    builder.func,
                )
            });

        builder.ins().call(*raise_illegal, &[exception_ptr]);

        exception_ptr
    }

    /// Emit the required IR to call `raise_store_amo_access_fault_exception`.
    ///
    /// This returns an initialised pointer to the exception.
    pub(super) fn raise_store_amo_access_fault_exception(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        address: Value,
    ) -> Value {
        let exception_slot = stack::Slot::<Exception>::new(self.ptr_type, builder);
        let exception_ptr = exception_slot.ptr(builder);

        let raise_store_amo_access_fault = self
            .raise_store_amo_access_fault_exception
            .get_or_insert_with(|| {
                self.module.declare_func_in_func(
                    self.imports.raise_store_amo_access_fault_exception,
                    builder.func,
                )
            });

        builder
            .ins()
            .call(*raise_store_amo_access_fault, &[exception_ptr, address]);

        exception_ptr
    }

    /// Emit the required IR to call `ecall`.
    ///
    /// This returns an initialised pointer to the appropriate environment
    /// call exception for the current machine mode.
    pub(super) fn ecall(&mut self, builder: &mut FunctionBuilder<'_>, core_ptr: Value) -> Value {
        let exception_slot = stack::Slot::<Exception>::new(self.ptr_type, builder);
        let exception_ptr = exception_slot.ptr(builder);

        let ecall_from_mode = self.ecall_from_mode.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.ecall_from_mode, builder.func)
        });

        builder
            .ins()
            .call(*ecall_from_mode, &[core_ptr, exception_ptr]);

        exception_ptr
    }

    /// Emit the required IR to call `memory_store`.
    ///
    /// Returns `errno` - on success, no additional values are returned.
    pub(super) fn memory_store(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        phys_address: X64,
        value: X64,
        width: LoadStoreWidth,
    ) -> impl Errno<(), MC, JSA> + 'static {
        let memory_store = self.memory_store.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.memory_store, builder.func)
        });

        let exception_slot = stack::Slot::<Exception>::new(self.ptr_type, builder);
        let exception_ptr = exception_slot.ptr(builder);

        let width = builder.ins().iconst(I8, width as u8 as i64);

        let call = builder.ins().call(*memory_store, &[
            core_ptr,
            phys_address.0,
            value.0,
            width,
            exception_ptr,
        ]);

        let errno = builder.inst_results(call)[0];

        ErrnoImpl::new(errno, exception_ptr, |_| {})
    }

    /// Emit the required IR to call `memory_load`.
    ///
    /// Returns `errno` - on success, the loaded value is returned.
    pub(super) fn memory_load(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        phys_address: X64,
        signed: bool,
        width: LoadStoreWidth,
    ) -> impl Errno<X64, MC, JSA> + 'static {
        let memory_load = self.memory_load.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.memory_load, builder.func)
        });

        let exception_slot = stack::Slot::<Exception>::new(self.ptr_type, builder);
        let exception_ptr = exception_slot.ptr(builder);

        let xval_slot = stack::Slot::<XValue>::new(self.ptr_type, builder);
        let xval_ptr = xval_slot.ptr(builder);

        let width = builder.ins().iconst(I8, width as u8 as i64);
        let signed = builder.ins().iconst(I8, signed as i64);

        let call = builder.ins().call(*memory_load, &[
            core_ptr,
            phys_address.0,
            width,
            signed,
            xval_ptr,
            exception_ptr,
        ]);

        let errno = builder.inst_results(call)[0];

        ErrnoImpl::new(errno, exception_ptr, move |builder| {
            // Safety: the xval is initialised prior to the call, and is guaranteed to
            // remain initialised regardless of the result of external call.
            let xval = unsafe { xval_slot.load(builder) };
            X64(xval)
        })
    }
}

/// Outcome of handling an exception.
pub(super) struct ExceptionHandledOutcome {
    /// Whether the exception was succesfully handled.
    ///
    /// - If true, the exception was handled and the step is completed.
    /// - If false, the exception must be instead handled by the environment.
    ///   The step is not complete.
    pub handled: Value,
    /// The new value of the instruction pc, after exception handling.
    pub new_pc: X64,
}
