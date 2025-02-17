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

use crate::{
    machine_state::{
        MachineCoreState,
        main_memory::MainMemoryLayout,
        registers::{NonZeroXRegister, XValue},
    },
    state_backend::{ManagerReadWrite, owned_backend::Owned},
};
use cranelift::{
    codegen::ir::{
        AbiParam, FuncRef, InstBuilder, Signature, Value,
        types::{I8, I64},
    },
    frontend::FunctionBuilder,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, ModuleResult};
use std::marker::PhantomData;

const PC_WRITE_SYMBOL: &str = "JSA::pc_write";

const XREG_READ_SYMBOL: &str = "JSA::xreg_read";

const XREG_WRITE_SYMBOL: &str = "JSA::xreg_write";

/// State Access that a JIT-compiled block may use.
///
/// In future, this will come in two parts:
/// - `extern "C"` functions that can be registered in the JIT module
/// - a way of calling those functions from within JIT-compiled code
pub trait JitStateAccess: ManagerReadWrite {
    /// Update the instruction pc in the state.
    extern "C" fn pc_write<ML: MainMemoryLayout>(core: &mut MachineCoreState<ML, Self>, pc: u64);

    /// Read the value of the given [`NonZeroXRegister`].
    extern "C" fn xregister_read<ML: MainMemoryLayout>(
        core: &mut MachineCoreState<ML, Self>,
        reg: NonZeroXRegister,
    ) -> XValue;

    /// Write the given value to the given [`NonZeroXRegister`].
    extern "C" fn xregister_write<ML: MainMemoryLayout>(
        core: &mut MachineCoreState<ML, Self>,
        reg: NonZeroXRegister,
        val: XValue,
    );
}

impl JitStateAccess for Owned {
    extern "C" fn pc_write<ML: MainMemoryLayout>(core: &mut MachineCoreState<ML, Self>, pc: u64) {
        core.hart.pc.write(pc)
    }

    extern "C" fn xregister_read<ML: MainMemoryLayout>(
        core: &mut MachineCoreState<ML, Self>,
        reg: NonZeroXRegister,
    ) -> XValue {
        core.hart.xregisters.read_nz(reg)
    }

    extern "C" fn xregister_write<ML: MainMemoryLayout>(
        core: &mut MachineCoreState<ML, Self>,
        reg: NonZeroXRegister,
        val: XValue,
    ) {
        core.hart.xregisters.write_nz(reg, val)
    }
}

/// Register state access symbols in the builder.
pub(super) fn register_jsa_symbols<ML: MainMemoryLayout, JSA: JitStateAccess>(
    builder: &mut JITBuilder,
) {
    builder.symbol(
        PC_WRITE_SYMBOL,
        <JSA as JitStateAccess>::pc_write::<ML> as *const u8,
    );

    builder.symbol(
        XREG_READ_SYMBOL,
        <JSA as JitStateAccess>::xregister_read::<ML> as *const u8,
    );

    builder.symbol(
        XREG_WRITE_SYMBOL,
        <JSA as JitStateAccess>::xregister_write::<ML> as *const u8,
    );
}

/// Identifications of globally imported [`JitStateAccess`] methods.
pub(super) struct JsaImports<ML: MainMemoryLayout, JSA: JitStateAccess> {
    pc_write: FuncId,
    xreg_read: FuncId,
    xreg_write: FuncId,
    _pd: PhantomData<(ML, JSA)>,
}

impl<ML: MainMemoryLayout, JSA: JitStateAccess> JsaImports<ML, JSA> {
    /// Register external functions within the JIT Module.
    pub(super) fn declare_in_module(module: &mut JITModule) -> ModuleResult<Self> {
        let call_conv = module.target_config().default_call_conv;

        let ptr = module.target_config().pointer_type();
        let ptr = AbiParam::new(ptr);

        let address = AbiParam::new(I64);

        let xreg = AbiParam::new(I8);
        let xvalue = AbiParam::new(I64);

        // PC
        let pc_write_sig = Signature {
            params: vec![ptr, address],
            returns: vec![],
            call_conv,
        };
        let pc_write = module.declare_function(PC_WRITE_SYMBOL, Linkage::Import, &pc_write_sig)?;

        // NonZeroXRegisters
        let xregister_read_sig = Signature {
            params: vec![ptr, xreg],
            returns: vec![xvalue],
            call_conv,
        };
        let xreg_read =
            module.declare_function(XREG_READ_SYMBOL, Linkage::Import, &xregister_read_sig)?;

        let xregister_write_sig = Signature {
            params: vec![ptr, xreg, xvalue],
            returns: vec![],
            call_conv,
        };
        let xreg_write =
            module.declare_function(XREG_WRITE_SYMBOL, Linkage::Import, &xregister_write_sig)?;

        Ok(Self {
            pc_write,
            xreg_read,
            xreg_write,
            _pd: PhantomData,
        })
    }
}

/// References to locally imported [`JitStateAccess`] methods, used to directly call
/// these accessor methods in the JIT-compilation context.
pub(super) struct JsaCalls<'a, ML: MainMemoryLayout, JSA: JitStateAccess> {
    module: &'a mut JITModule,
    imports: &'a JsaImports<ML, JSA>,
    pc_write: Option<FuncRef>,
    xreg_read: Option<FuncRef>,
    xreg_write: Option<FuncRef>,
    _pd: PhantomData<(ML, JSA)>,
}

impl<'a, ML: MainMemoryLayout, JSA: JitStateAccess> JsaCalls<'a, ML, JSA> {
    /// Wrapper to simplify calling JSA methods from within the function under construction.
    pub(super) fn func_calls(module: &'a mut JITModule, imports: &'a JsaImports<ML, JSA>) -> Self {
        Self {
            module,
            imports,
            pc_write: None,
            xreg_read: None,
            xreg_write: None,
            _pd: PhantomData,
        }
    }

    /// Emit the required IR to set the pc to the given value.
    pub(super) fn pc_write(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        pc_val: Value,
    ) {
        let pc_write = self.pc_write.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.pc_write, builder.func)
        });
        builder.ins().call(*pc_write, &[core_ptr, pc_val]);
    }

    /// Emit the required IR to read the value from the given xregister.
    pub(super) fn xreg_read(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
    ) -> Value {
        let xreg_read = self.xreg_read.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.xreg_read, builder.func)
        });
        let reg = builder.ins().iconst(I8, reg as i64);
        let call = builder.ins().call(*xreg_read, &[core_ptr, reg]);
        builder.inst_results(call)[0]
    }

    /// Emit the required IR to write the value to the given xregister.
    pub(super) fn xreg_write(
        &mut self,
        builder: &mut FunctionBuilder<'_>,
        core_ptr: Value,
        reg: NonZeroXRegister,
        value: Value,
    ) {
        let xreg_write = self.xreg_write.get_or_insert_with(|| {
            self.module
                .declare_func_in_func(self.imports.xreg_write, builder.func)
        });
        let reg = builder.ins().iconst(I8, reg as i64);
        builder.ins().call(*xreg_write, &[core_ptr, reg, value]);
    }
}
