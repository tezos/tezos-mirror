// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A JIT library for compilation of sequences (or blocks) of RISC-V
//! instructions to native code.

mod builder;
pub mod state_access;

use self::builder::Builder;
use self::state_access::JsaCalls;
use self::state_access::JsaImports;
use self::state_access::register_jsa_symbols;
use crate::machine_state::MachineCoreState;
use crate::machine_state::ProgramCounterUpdate;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::main_memory::MainMemoryLayout;
use crate::traps::EnvironException;
use cranelift::codegen::CodegenError;
use cranelift::codegen::ir::types::I64;
use cranelift::codegen::settings::SetError;
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Linkage;
use cranelift_module::Module;
use cranelift_module::ModuleError;
use state_access::JitStateAccess;
use thiserror::Error;

/// Alias for the function signature produced by the JIT compilation.
type JitFn<ML, JSA> = unsafe extern "C" fn(&mut MachineCoreState<ML, JSA>, u64, &mut usize);

/// A jit-compiled function that can be [called] over [`MachineCoreState`].
///
/// [called]: Self::call
pub struct JCall<ML: MainMemoryLayout, JSA: JitStateAccess> {
    fun: JitFn<ML, JSA>,
}

impl<ML: MainMemoryLayout, JSA: JitStateAccess> JCall<ML, JSA> {
    /// Run the jit-compiled function over the state.
    ///
    /// # Safety
    ///
    /// When calling, the [JIT] that compiled this function *must*
    /// still be alive.
    pub unsafe fn call(
        &self,
        core: &mut MachineCoreState<ML, JSA>,
        pc: u64,
        steps: &mut usize,
    ) -> Result<(), EnvironException> {
        (self.fun)(core, pc, steps);
        Ok(())
    }
}

/// Errors that may arise from the initialisation of the JIT.
#[derive(Debug, Error)]
pub enum JitError {
    /// Failures setting flags.
    #[error("Failed to set flag {0}")]
    Setting(#[from] SetError),
    /// Native compilation unsupported on the current arch/os.
    #[error("Native platform unsupported: {0}")]
    UnsupportedPlatform(&'static str),
    /// Constructing the Cranelift builder failed.
    #[error("Unable to initialise builder {0}")]
    BuilderFailure(#[from] CodegenError),
    /// Unable to register external [`JitStateAccess`] functionality.
    #[error("Unable to register external JSA functions: {0}")]
    JsaRegistration(#[from] ModuleError),
}

/// The JIT is responsible for compiling blocks of instructions to machine code,
/// returning a function that can be run over the [`MachineCoreState`].
pub struct JIT<ML: MainMemoryLayout, JSA: JitStateAccess> {
    /// The function builder context, which is reused across multiple
    /// [`FunctionBuilder`] instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    /// Imported [JitStateAccess] functions.
    jsa_imports: JsaImports<ML, JSA>,

    /// Counter for naming of functions
    next_id: usize,
}

impl<ML: MainMemoryLayout, JSA: JitStateAccess> JIT<ML, JSA> {
    /// Create a new instance of the JIT, which will be able to
    /// produce functions that can be run over the current
    /// memory layout & manager.
    pub fn new() -> Result<Self, JitError> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;

        let isa_builder = cranelift_native::builder().map_err(JitError::UnsupportedPlatform)?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        register_jsa_symbols::<ML, JSA>(&mut builder);

        let mut module = JITModule::new(builder);
        let jsa_imports = JsaImports::declare_in_module(&mut module)?;

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            module,
            next_id: 0,
            jsa_imports,
        })
    }

    /// Compile a sequence of instructions to a callable native function.
    ///
    /// Not all instructions are currently supported. For blocks containing
    /// unsupported instructions, `None` will be returned.
    pub fn compile<'a>(
        &mut self,
        instr: impl IntoIterator<Item = &'a Instruction>,
    ) -> Option<JCall<ML, JSA>> {
        let mut builder = self.start();

        for i in instr {
            let Some(lower) = i.opcode.to_lowering() else {
                builder.fail();
                self.clear();
                return None;
            };

            let pc_update = unsafe {
                // # SAFETY: lower is called with args from the same instruction that it
                // was derived
                (lower)(i.args(), &mut builder)
            };

            let ProgramCounterUpdate::Next(width) = pc_update else {
                todo!("RV-428: support jumps/branching instructions");
            };

            builder.pc_offset += width as u64;
            builder.steps += 1;
        }

        builder.end();

        let fun = self.finalise();

        Some(JCall { fun })
    }

    /// Setup the builder, ensuring the entry block of the function is correct.
    ///
    /// # Input Args
    ///
    /// | `core: &mut MachineCoreState` | `int (ptr) -> MachineCoreState` |
    /// | `pc: Address`                 | `I64`                           |
    /// | `steps: &mut usize`           | `int (ptr) -> int`              |
    ///
    /// # Return
    ///
    /// | `steps: usize`                | `int`                           |
    fn start(&mut self) -> Builder<'_, ML, JSA> {
        let ptr = self.module.target_config().pointer_type();

        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        self.ctx.func.signature.params.push(AbiParam::new(I64));
        self.ctx.func.signature.params.push(AbiParam::new(ptr));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let core_ptr_val = builder.block_params(entry_block)[0];
        let pc_val = builder.block_params(entry_block)[1];
        let steps_ptr_val = builder.block_params(entry_block)[2];

        let jsa_call = JsaCalls::func_calls(&mut self.module, &self.jsa_imports);

        Builder::<'_, ML, JSA> {
            builder,
            ptr,
            core_ptr_val,
            steps_ptr_val,
            steps: 0,
            pc_val,
            pc_offset: 0,
            jsa_call,
        }
    }

    /// Produce a unique name, that can be used when a function is finalised.
    fn name(&mut self) -> impl AsRef<str> {
        let name = self.next_id.to_string();
        self.next_id += 1;
        name
    }

    /// Finalise the function currently under construction.
    fn finalise(&mut self) -> JitFn<ML, JSA> {
        let name = self.name();
        let id = self
            .module
            .declare_function(name.as_ref(), Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())
            .unwrap();

        // define the function to jit
        self.module.define_function(id, &mut self.ctx).unwrap();

        // finalise the function
        self.module.finalize_definitions().unwrap();
        let code = self.module.get_finalized_function(id);

        self.clear();

        // SAFETY: the signature of a JitFn matches exactly the abi we specified in the
        //         entry block. Compilation has succeeded & therefore this produced code
        //         is safe to call.
        unsafe { std::mem::transmute(code) }
    }

    /// Clear the current context to allow a new function to be compiled
    fn clear(&mut self) {
        self.module.clear_context(&mut self.ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::machine_state::block_cache::bcall::{BCall, Block, BlockLayout, Interpreted};
    use crate::machine_state::main_memory::tests::T1K;
    use crate::machine_state::{MachineCoreState, MachineCoreStateLayout};
    use crate::parser::instruction::InstrWidth::*;
    use crate::state_backend::test_helpers::assert_eq_struct;
    use crate::state_backend::{FnManagerIdent, ManagerRead};
    use crate::{backend_test, create_state};

    fn instructions<ML: MainMemoryLayout, M>(block: &Interpreted<ML, M>) -> Vec<Instruction>
    where
        M: ManagerRead,
    {
        let instr = block.instr();
        instr.iter().map(|cell| cell.read_stored()).collect()
    }

    // Simplified variant of the `Cached` structure in the block cache.
    backend_test!(test_cnop, F, {
        use Instruction as I;

        // Arrange
        let scenarios: &[&[I]] = &[
            &[I::new_nop(Compressed)],
            &[I::new_nop(Compressed), I::new_nop(Uncompressed)],
            &[
                I::new_nop(Uncompressed),
                I::new_nop(Compressed),
                I::new_nop(Uncompressed),
            ],
        ];

        let mut jit = JIT::<T1K, F::Manager>::new().unwrap();

        for scenario in scenarios {
            let mut interpreted =
                create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
            let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
            let mut block = create_state!(Interpreted, BlockLayout<T1K>, F, T1K);

            block.start_block();
            for instr in scenario.iter() {
                block.push_instr(*instr);
            }

            let mut interpreted_steps = 0;
            let mut jitted_steps = 0;

            let initial_pc = 0;
            interpreted.hart.pc.write(initial_pc);
            jitted.hart.pc.write(initial_pc);

            // Act
            let fun = jit
                .compile(instructions(&block).as_slice())
                .expect("Compilation of CNop should succeed");

            let interpreted_res = block.callable().unwrap().run_block(
                &mut interpreted,
                initial_pc,
                &mut interpreted_steps,
            );
            let jitted_res = unsafe {
                // # Safety - the jit is not dropped until after we
                //            exit the for loop
                fun.call(&mut jitted, initial_pc, &mut jitted_steps)
            };

            // Assert
            assert_eq!(jitted_res, interpreted_res);
            assert_eq!(
                interpreted_steps, jitted_steps,
                "Interpreted mode ran for {interpreted_steps}, compared to jit-mode of {jitted_steps}"
            );

            assert_eq!(interpreted_steps, scenario.len());

            assert_eq_struct(
                &interpreted.struct_ref::<FnManagerIdent>(),
                &jitted.struct_ref::<FnManagerIdent>(),
            );
        }
    });

    backend_test!(test_cmv, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;
        use Instruction as I;

        // Arrange
        let scenarios: &[&[I]] = &[
            &[I::new_mv(x2, x1, Compressed)],
            &[I::new_mv(x2, x1, Uncompressed)],
            &[
                I::new_mv(x2, x1, Compressed),
                I::new_mv(x3, x2, Uncompressed),
            ],
        ];

        let mut jit = JIT::<T1K, F::Manager>::new().unwrap();

        for scenario in scenarios {
            let mut interpreted =
                create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
            let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
            let mut block = create_state!(Interpreted, BlockLayout<T1K>, F, T1K);

            block.start_block();
            for instr in scenario.iter() {
                block.push_instr(*instr);
            }

            let mut interpreted_steps = 0;
            let mut jitted_steps = 0;

            let initial_pc = 0;
            interpreted.hart.pc.write(initial_pc);
            jitted.hart.pc.write(initial_pc);

            interpreted.hart.xregisters.write_nz(x1, 1);
            jitted.hart.xregisters.write_nz(x1, 1);

            // Act
            let fun = jit
                .compile(instructions(&block).as_slice())
                .expect("Compilation of CNop should succeed");

            let interpreted_res = block.callable().unwrap().run_block(
                &mut interpreted,
                initial_pc,
                &mut interpreted_steps,
            );
            let jitted_res = unsafe {
                // # Safety - the jit is not dropped until after we
                //            exit the for loop
                fun.call(&mut jitted, initial_pc, &mut jitted_steps)
            };

            // Assert
            assert_eq!(jitted_res, interpreted_res);
            assert_eq!(
                interpreted_steps, jitted_steps,
                "Interpreted mode ran for {interpreted_steps}, compared to jit-mode of {jitted_steps}"
            );

            assert_eq!(interpreted_steps, scenario.len());

            // every scenario expects x2 to be 1 after
            assert_eq!(interpreted.hart.xregisters.read_nz(x2), 1);
            assert_eq!(jitted.hart.xregisters.read_nz(x2), 1);

            assert_eq_struct(
                &interpreted.struct_ref::<FnManagerIdent>(),
                &jitted.struct_ref::<FnManagerIdent>(),
            );
        }
    });

    backend_test!(test_add, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;
        use Instruction as I;

        // Arrange

        // calculate fibonacci(4) == 5
        let scenario: &[I] = &[
            I::new_add(x2, x2, x1, Compressed),
            I::new_add(x1, x1, x2, Uncompressed),
            I::new_add(x2, x2, x1, Uncompressed),
            I::new_add(x1, x1, x2, Compressed),
        ];

        let mut jit = JIT::<T1K, F::Manager>::new().unwrap();

        let mut interpreted = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let mut block = create_state!(Interpreted, BlockLayout<T1K>, F, T1K);

        block.start_block();
        for instr in scenario.iter() {
            block.push_instr(*instr);
        }

        let mut interpreted_steps = 0;
        let mut jitted_steps = 0;

        let initial_pc = 0;
        interpreted.hart.pc.write(initial_pc);
        jitted.hart.pc.write(initial_pc);

        interpreted.hart.xregisters.write_nz(x1, 1);
        jitted.hart.xregisters.write_nz(x1, 1);

        // Act
        let fun = jit
            .compile(instructions(&block).as_slice())
            .expect("Compilation of CNop should succeed");

        let interpreted_res = block.callable().unwrap().run_block(
            &mut interpreted,
            initial_pc,
            &mut interpreted_steps,
        );
        let jitted_res = unsafe {
            // # Safety - the jit is not dropped until after we
            //            exit the block
            fun.call(&mut jitted, initial_pc, &mut jitted_steps)
        };

        // Assert
        assert_eq!(jitted_res, interpreted_res);
        assert_eq!(
            interpreted_steps, jitted_steps,
            "Interpreted mode ran for {interpreted_steps}, compared to jit-mode of {jitted_steps}"
        );

        assert_eq!(interpreted_steps, scenario.len());

        // have got to the fibonacci number 5
        assert_eq!(interpreted.hart.xregisters.read_nz(x1), 5);
        assert_eq!(jitted.hart.xregisters.read_nz(x1), 5);

        assert_eq_struct(
            &interpreted.struct_ref::<FnManagerIdent>(),
            &jitted.struct_ref::<FnManagerIdent>(),
        );
    });

    backend_test!(test_jit_recovers_from_compilation_failure, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;
        use Instruction as I;

        // Arrange
        let failure: &[I] = &[
            // does not currently lowering
            I::new_andi(x1, x1, 13, Uncompressed),
        ];

        let success: &[I] = &[I::new_nop(Compressed)];

        let mut jit = JIT::<T1K, F::Manager>::new().unwrap();

        let mut jitted = create_state!(MachineCoreState, MachineCoreStateLayout<T1K>, F, T1K);
        let mut block = create_state!(Interpreted, BlockLayout<T1K>, F, T1K);

        block.start_block();
        for instr in failure.iter() {
            block.push_instr(*instr);
        }

        let mut jitted_steps = 0;

        let initial_pc = 0;
        jitted.hart.pc.write(initial_pc);

        jitted.hart.xregisters.write_nz(x1, 1);

        // Act
        let res = jit.compile(instructions(&block).as_slice());

        assert!(
            res.is_none(),
            "Compilation of unsupported instruction should fail"
        );

        block.start_block();
        for instr in success.iter() {
            block.push_instr(*instr);
        }

        let fun = jit
            .compile(instructions(&block).as_slice())
            .expect("Compilation of subsequent functions should succeed");
        let jitted_res = unsafe {
            // # Safety - the jit is not dropped until after we
            //            exit the block.
            fun.call(&mut jitted, initial_pc, &mut jitted_steps)
        };

        assert!(jitted_res.is_ok());
        assert_eq!(jitted_steps, success.len());
    });
}
