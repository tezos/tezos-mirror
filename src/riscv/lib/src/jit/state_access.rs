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
    machine_state::{main_memory::MainMemoryLayout, MachineCoreState},
    state_backend::{owned_backend::Owned, ManagerReadWrite},
};
use cranelift::{
    codegen::ir::{types::I64, AbiParam, FuncRef, InstBuilder, Signature, Value},
    frontend::FunctionBuilder,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module, ModuleResult};
use std::marker::PhantomData;

const PC_WRITE_SYMBOL: &str = "JSA::pc_write";

/// State Access that a JIT-compiled block may use.
///
/// In future, this will come in two parts:
/// - `extern "C"` functions that can be registered in the JIT module
/// - a way of calling those functions from within JIT-compiled code
// TODO (RV-404): add functionality to
//  - read/write to xregisters
pub trait JitStateAccess: ManagerReadWrite {
    /// Update the instruction pc in the state.
    extern "C" fn pc_write<ML: MainMemoryLayout>(core: &mut MachineCoreState<ML, Self>, pc: u64);
}

impl JitStateAccess for Owned {
    extern "C" fn pc_write<ML: MainMemoryLayout>(core: &mut MachineCoreState<ML, Self>, pc: u64) {
        core.hart.pc.write(pc)
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
}

/// Identifications of globally imported [`JitStateAccess`] methods.
pub(super) struct JsaImports<ML: MainMemoryLayout, JSA: JitStateAccess> {
    pc_write: FuncId,
    _pd: PhantomData<(ML, JSA)>,
}

impl<ML: MainMemoryLayout, JSA: JitStateAccess> JsaImports<ML, JSA> {
    /// Register external functions within the JIT Module.
    pub(super) fn declare_in_module(module: &mut JITModule) -> ModuleResult<Self> {
        let call_conv = module.target_config().default_call_conv;

        let ptr = module.target_config().pointer_type();
        let ptr = AbiParam::new(ptr);

        let pc_write_sig = Signature {
            params: vec![ptr, AbiParam::new(I64)],
            returns: vec![],
            call_conv,
        };

        let pc_write = module.declare_function(PC_WRITE_SYMBOL, Linkage::Import, &pc_write_sig)?;

        Ok(Self {
            pc_write,
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
    _pd: PhantomData<(ML, JSA)>,
}

impl<'a, ML: MainMemoryLayout, JSA: JitStateAccess> JsaCalls<'a, ML, JSA> {
    /// Wrapper to simplify calling JSA methods from within the function under construction.
    pub(super) fn func_calls(module: &'a mut JITModule, imports: &'a JsaImports<ML, JSA>) -> Self {
        Self {
            module,
            imports,
            pc_write: None,
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
}
