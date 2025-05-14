// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! A JIT library for compilation of sequences (or blocks) of RISC-V
//! instructions to native code.

pub(crate) mod builder;
pub(crate) mod state_access;

use std::collections::HashMap;
use std::ffi::c_void;

use cranelift::codegen::CodegenError;
use cranelift::codegen::ir::types::I64;
use cranelift::codegen::settings::SetError;
use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::JITBuilder;
use cranelift_jit::JITModule;
use cranelift_module::Linkage;
use cranelift_module::Module;
use cranelift_module::ModuleError;
use state_access::JitStateAccess;
use thiserror::Error;

use self::builder::Builder;
use self::state_access::JsaCalls;
use self::state_access::JsaImports;
use self::state_access::register_jsa_symbols;
use crate::machine_state::MachineCoreState;
use crate::machine_state::block_cache::metrics::block_metrics;
use crate::machine_state::instruction::Instruction;
use crate::machine_state::memory::MemoryConfig;
use crate::state_backend::hash::Hash;
use crate::traps::EnvironException;

/// Alias for the function signature produced by the JIT compilation.
///
/// This must have the same Abi as [`DispatchFn`], which is used by
/// the block dispatch mechanism in the block cache.
///
/// The JitFn does not inspect the first and last parameters here, however.
/// These parameters are needed by the initial dispatch mechanism to enable
/// JIT-compilation & hot-swapping. To avoid over-specifying these parameters here
/// (which can among other things cause type-checking issues), we replace the parameters
/// with pointers to `c_void` - which in the C abi map to the same parameter type as the
/// thin-references to the actual variables passed.
///
/// [`DispatchFn`]: crate::machine_state::block_cache::block::DispatchFn
#[expect(
    improper_ctypes_definitions,
    reason = "The receiving functions know the layout of the referenced types"
)]
pub type JitFn<MC, JSA> = unsafe extern "C" fn(
    // ignored
    *const c_void,
    &mut MachineCoreState<MC, JSA>,
    u64,
    &mut usize,
    &mut Result<(), EnvironException>,
    // ignored
    *const c_void,
);

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
pub struct JIT<MC: MemoryConfig, JSA: JitStateAccess> {
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
    jsa_imports: JsaImports<MC, JSA>,

    /// Cache of compilation results.
    cache: HashMap<Hash, Option<JitFn<MC, JSA>>>,
}

impl<MC: MemoryConfig, JSA: JitStateAccess> JIT<MC, JSA> {
    /// Create a new instance of the JIT, which will be able to
    /// produce functions that can be run over the current
    /// memory configuration and manager.
    pub fn new() -> Result<Self, JitError> {
        if std::mem::size_of::<usize>() != std::mem::size_of::<u64>() {
            return Err(JitError::UnsupportedPlatform(
                "octez-riscv JIT only supports 64-bit architectures",
            ));
        }

        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("is_pic", "false")?;

        let isa_builder = cranelift_native::builder().map_err(JitError::UnsupportedPlatform)?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        register_jsa_symbols::<MC, JSA>(&mut builder);

        let mut module = JITModule::new(builder);
        let jsa_imports = JsaImports::declare_in_module(&mut module)?;

        Ok(Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: codegen::Context::new(),
            module,
            jsa_imports,
            cache: Default::default(),
        })
    }

    /// Compile a sequence of instructions to a callable native function.
    ///
    /// Not all instructions are currently supported. For blocks containing
    /// unsupported instructions, `None` will be returned.
    pub fn compile(&mut self, instr: &[Instruction]) -> Option<JitFn<MC, JSA>> {
        let Ok(hash) = Hash::blake2b_hash(instr) else {
            return None;
        };

        if let Some(compilation_result) = self.cache.get(&hash) {
            return *compilation_result;
        }

        let mut builder = self.start();

        for i in instr {
            let Some(lower) = i.opcode.to_lowering() else {
                builder.fail();
                self.clear();
                self.cache.insert(hash, None);
                return None;
            };

            let pc_update = unsafe {
                // # SAFETY: lower is called with args from the same instruction that it
                // was derived
                (lower)(i.args(), &mut builder)
            };

            let Some(pc_update) = pc_update else {
                builder.end_unconditional_exception();

                let jcall = self.produce_function(&hash);

                return Some(jcall);
            };

            if !builder.complete_step(pc_update) {
                // We have encountered an unconditional jump, exit the block.
                break;
            }
        }

        builder.end();
        let jcall = self.produce_function(&hash);

        Some(jcall)
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
    fn start(&mut self) -> Builder<'_, MC, JSA> {
        let ptr = self.module.target_config().pointer_type();

        // first param ignored
        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        // params
        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        self.ctx.func.signature.params.push(AbiParam::new(I64));
        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        self.ctx.func.signature.params.push(AbiParam::new(ptr));
        // last param ignored
        self.ctx.func.signature.params.push(AbiParam::new(ptr));

        let builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let jsa_call = JsaCalls::func_calls(&mut self.module, &self.jsa_imports, ptr);

        Builder::<'_, MC, JSA>::new(ptr, builder, jsa_call)
    }

    /// Finalise and cache the function under construction.
    fn produce_function(&mut self, hash: &Hash) -> JitFn<MC, JSA> {
        let name = hex::encode(hash);

        let fun = self.finalise(&name);

        self.cache.insert(*hash, Some(fun));
        block_metrics!(hash = hash, record_jitted);

        fun
    }

    /// Finalise the function currently under construction.
    fn finalise(&mut self, name: &str) -> JitFn<MC, JSA> {
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

// TODO: https://linear.app/tezos/issue/RV-496
//       `Block::BlockBuilder` should not require Default, as it
//         does not allow for potential fallilibility
impl<MC: MemoryConfig, M: JitStateAccess> Default for JIT<MC, M> {
    fn default() -> Self {
        Self::new().expect("JIT is supported on all octez-riscv supported platforms")
    }
}

#[cfg(test)]
mod tests {
    use std::ptr::null;

    use Instruction as I;

    use super::*;
    use crate::backend_test;
    use crate::instruction_context::LoadStoreWidth;
    use crate::machine_state::MachineCoreState;
    use crate::machine_state::block_cache::block::Block;
    use crate::machine_state::block_cache::block::Interpreted;
    use crate::machine_state::block_cache::block::InterpretedBlockBuilder;
    use crate::machine_state::memory::M4K;
    use crate::machine_state::memory::Memory;
    use crate::machine_state::memory::MemoryConfig;
    use crate::machine_state::registers::NonZeroXRegister;
    use crate::machine_state::registers::XRegister;
    use crate::machine_state::registers::nz;
    use crate::parser::instruction::InstrWidth;
    use crate::parser::instruction::InstrWidth::*;
    use crate::state::NewState;
    use crate::state_backend::FnManagerIdent;
    use crate::state_backend::ManagerRead;
    use crate::state_backend::test_helpers::TestBackendFactory;
    use crate::state_backend::test_helpers::assert_eq_struct;

    fn instructions<MC: MemoryConfig, M>(block: &Interpreted<MC, M>) -> Vec<Instruction>
    where
        M: ManagerRead,
    {
        let instr = block.instr();
        instr.iter().map(|cell| cell.read_stored()).collect()
    }

    type SetupHook<F> = dyn Fn(&mut MachineCoreState<M4K, <F as TestBackendFactory>::Manager>);
    type AssertHook<F> = dyn Fn(&MachineCoreState<M4K, <F as TestBackendFactory>::Manager>);

    struct Scenario<F: TestBackendFactory> {
        initial_pc: Option<u64>,
        expected_steps: Option<usize>,
        instructions: Vec<Instruction>,
        setup_hook: Option<Box<SetupHook<F>>>,
        assert_hook: Option<Box<AssertHook<F>>>,
    }

    impl<F: TestBackendFactory> Scenario<F> {
        fn simple(instructions: &[Instruction]) -> Self {
            Scenario {
                initial_pc: None,
                expected_steps: None,
                instructions: instructions.to_vec(),
                setup_hook: None,
                assert_hook: None,
            }
        }

        /// Run a test scenario over both the Interpreted & JIT modes of compilation,
        /// to ensure they behave identically.
        fn run(
            &self,
            jit: &mut JIT<M4K, F::Manager>,
            interpreted_bb: &mut InterpretedBlockBuilder,
        ) {
            // Create the states for the interpreted and jitted runs.
            let mut manager = F::manager();
            let mut interpreted = MachineCoreState::<M4K, _>::new(&mut manager);
            interpreted.main_memory.set_all_readable_writeable();

            let mut jitted = MachineCoreState::<M4K, _>::new(&mut manager);
            jitted.main_memory.set_all_readable_writeable();

            // Create the block of instructions.
            let mut block = Interpreted::<M4K, _>::new(&mut manager);
            block.start_block();
            for instr in self.instructions.iter() {
                block.push_instr(*instr);
            }

            // Run the setup hooks.
            if let Some(hook) = &self.setup_hook {
                (hook)(&mut interpreted);
                (hook)(&mut jitted)
            }

            // initialise starting parameters: pc, steps
            let initial_pc = self.initial_pc.unwrap_or_default();
            let mut interpreted_steps = 0;
            let mut jitted_steps = 0;
            interpreted.hart.pc.write(initial_pc);
            jitted.hart.pc.write(initial_pc);

            // Create the JIT function.
            let fun = jit
                .compile(instructions(&block).as_slice())
                .expect("Compilation of block should succeed.");

            // Run the block in both interpreted and jitted mode.
            let interpreted_res = unsafe {
                // SAFETY: interpreted blocks are always callable
                block.run_block(
                    &mut interpreted,
                    initial_pc,
                    &mut interpreted_steps,
                    interpreted_bb,
                )
            };

            let mut jitted_res = Ok(());
            unsafe {
                // # Safety - the block builder is alive for at least
                //            the duration of the `run` function.
                (fun)(
                    null(),
                    &mut jitted,
                    initial_pc,
                    &mut jitted_steps,
                    &mut jitted_res,
                    null(),
                )
            };

            // Assert state equality.
            assert_eq!(jitted_res, interpreted_res);
            assert_eq!(
                interpreted_steps, jitted_steps,
                "Interpreted mode ran for {interpreted_steps}, compared to jit-mode of {jitted_steps}"
            );
            assert_eq_struct(
                &interpreted.struct_ref::<FnManagerIdent>(),
                &jitted.struct_ref::<FnManagerIdent>(),
            );

            // Only check steps against one state, as we know both interpreted/jit steps are equal.
            let expected_steps = self.expected_steps.unwrap_or(self.instructions.len());
            assert_eq!(
                interpreted_steps, expected_steps,
                "Scenario ran for {interpreted_steps} steps, but expected {expected_steps}"
            );

            // Run the assert hooks. Since we have already verified that the states are equal,
            // we can run the assert hooks on just the interpreted state.
            if let Some(hook) = &self.assert_hook {
                (hook)(&mut interpreted);
            }
        }
    }

    /// A builder for creating scenarios.
    struct ScenarioBuilder<F: TestBackendFactory> {
        initial_pc: Option<u64>,
        expected_steps: Option<usize>,
        instructions: Vec<Instruction>,
        setup_hook: Option<Box<SetupHook<F>>>,
        assert_hook: Option<Box<AssertHook<F>>>,
    }

    impl<F: TestBackendFactory> Default for ScenarioBuilder<F> {
        fn default() -> Self {
            ScenarioBuilder {
                initial_pc: None,
                expected_steps: None,
                instructions: Vec::new(),
                setup_hook: None,
                assert_hook: None,
            }
        }
    }

    impl<F: TestBackendFactory> ScenarioBuilder<F> {
        fn set_instructions(mut self, instructions: &[Instruction]) -> Self {
            self.instructions = instructions.to_vec();
            self
        }

        fn set_initial_pc(mut self, initial_pc: u64) -> Self {
            self.initial_pc = Some(initial_pc);
            self
        }

        fn set_expected_steps(mut self, expected_steps: usize) -> Self {
            self.expected_steps = Some(expected_steps);
            self
        }

        fn set_assert_hook(mut self, assert_hook: Box<AssertHook<F>>) -> Self {
            self.assert_hook = Some(assert_hook);
            self
        }

        fn set_setup_hook(mut self, setup_hook: Box<SetupHook<F>>) -> Self {
            self.setup_hook = Some(setup_hook);
            self
        }

        fn build(self) -> Scenario<F> {
            Scenario {
                initial_pc: self.initial_pc,
                expected_steps: self.expected_steps,
                instructions: self.instructions,
                setup_hook: self.setup_hook,
                assert_hook: self.assert_hook,
            }
        }
    }

    macro_rules! setup_hook {
        ($core:ident, $F:ident, $block:expr) => {
            Box::new(move |$core: &mut MachineCoreState<M4K, $F::Manager>| $block)
        };
    }

    macro_rules! assert_hook {
        ($core:ident, $F:ident, $block:expr) => {
            Box::new(move |$core: &MachineCoreState<M4K, $F::Manager>| $block)
        };
    }

    backend_test!(test_cnop, F, {
        let scenarios: &[Scenario<F>] = &[
            Scenario::simple(&[I::new_nop(Compressed)]),
            Scenario::simple(&[I::new_nop(Compressed), I::new_nop(Uncompressed)]),
            Scenario::simple(&[
                I::new_nop(Uncompressed),
                I::new_nop(Compressed),
                I::new_nop(Uncompressed),
            ]),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_cmv, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x2_is_one = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x2), 1);
        });

        // Arrange
        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[I::new_li(x1, 1, Compressed), I::new_mv(x2, x1, Compressed)])
                .set_assert_hook(assert_x2_is_one.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Uncompressed),
                    I::new_mv(x2, x1, Uncompressed),
                ])
                .set_assert_hook(assert_x2_is_one.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Compressed),
                    I::new_mv(x2, x1, Compressed),
                    I::new_mv(x3, x2, Compressed),
                ])
                .set_assert_hook(assert_x2_is_one)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_negate, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, -1, Compressed),
                    I::new_li(x3, 1, Compressed),
                    I::new_neg(x2, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 1, Uncompressed),
                    I::new_neg(x3, x1, Uncompressed),
                    I::new_neg(x2, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_x2_equal)
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, i64::MIN, Uncompressed),
                    I::new_neg(x2, x1, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), i64::MIN as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_add, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_is_five = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x1), 5);
        });

        let scenario: Scenario<F> = ScenarioBuilder::default()
            .set_instructions(&[
                I::new_li(x1, 1, Uncompressed),
                I::new_add(x2, x2, x1, Compressed),
                I::new_add(x1, x1, x2, Uncompressed),
                I::new_add(x2, x2, x1, Uncompressed),
                I::new_add(x1, x1, x2, Compressed),
            ])
            .set_assert_hook(assert_x1_is_five)
            .build();

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        scenario.run(&mut jit, &mut interpreted_bb);
    });

    backend_test!(test_add_word, F, {
        use Instruction as I;

        use crate::machine_state::registers::a0;
        use crate::machine_state::registers::a1;
        use crate::machine_state::registers::nz;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, 10, Uncompressed),
                    I::new_li(nz::a1, 1, Compressed),
                    I::new_add_word(nz::a2, a0, a1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), 11);
                }))
                .build(),
            // Test that we wrap around and truncate before sign extending. This
            // operation 0xFFFFFFFF + 0xFFFFFFFF should produce a different result
            // for 32-bit (truncated sum with sign extension) vs 64-bit operations.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, 0xFFFFFFFF, Compressed),
                    I::new_li(nz::a1, 0xFFFFFFFF, Uncompressed),
                    I::new_add_word(nz::a2, a0, a1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    // In 32-bit addition:
                    // 0xFFFFFFFF + 0xFFFFFFFF = 0x1FFFFFFFE
                    // Truncated to 32 bits: 0xFFFFFFFE
                    // Sign extended to 64 bits: 0xFFFFFFFFFFFFFFFE
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), -2i64 as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_add_word_i, F, {
        use Instruction as I;

        use crate::machine_state::registers::a0;
        use crate::machine_state::registers::nz;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, 10, Uncompressed),
                    I::new_add_word_immediate(nz::a1, a0, 1_i64, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a1), 11);
                }))
                .build(),
            // Test that we wrap around and truncate before sign extending. This
            // operation 0xFFFFFFFF + 0xFFFFFFFF should produce a different result
            // for 32-bit (truncated sum with sign extension) vs 64-bit operations.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, 0xFFFFFFFF, Compressed),
                    I::new_add_word_immediate(nz::a1, a0, 0xFFFFFFFF_i64, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    // In 32-bit addition:
                    // 0xFFFFFFFF + 0xFFFFFFFF = 0x1FFFFFFFE
                    // Truncated to 32 bits: 0xFFFFFFFE
                    // Sign extended to 64 bits: 0xFFFFFFFFFFFFFFFE
                    assert_eq!(core.hart.xregisters.read_nz(nz::a1), -2i64 as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_sub, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Uncompressed),
                    I::new_sub(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 0);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Compressed),
                    I::new_li(x3, -10, Uncompressed),
                    I::new_sub(x2, x1, x3, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 20);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 10, Uncompressed),
                    I::new_li(x3, 100, Compressed),
                    I::new_sub(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), (-90_i64) as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_sub_word, F, {
        use Instruction as I;

        use crate::machine_state::registers::a0;
        use crate::machine_state::registers::a1;
        use crate::machine_state::registers::nz;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, 10, Uncompressed),
                    I::new_li(nz::a1, 1, Compressed),
                    I::new_sub_word(nz::a2, a0, a1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), 9);
                }))
                .build(),
            // Test that we wrap around 0 and truncate before sign extending. This
            // operation 0xFFFFFFFFFFFFFFFF - 0xFFFFFFFF00000000 should produce a
            // different result for 32-bit (all 1s) and 64-bit operations (only lower 32-bits
            // as 1s).
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, !0, Compressed),
                    I::new_li(nz::a1, 0xFFFFFFFF00000000u64 as i64, Uncompressed),
                    I::new_sub_word(nz::a2, a0, a1, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), !0);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_and, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Bitwise and with all ones is self.
                .set_instructions(&[
                    I::new_li(x1, 13872, Uncompressed),
                    I::new_li(x3, !0, Compressed),
                    I::new_and(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                // Bitwise and with itself is self.
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_and(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            ScenarioBuilder::default()
                // Bitwise and with 0 is 0.
                .set_instructions(&[
                    I::new_li(x1, 0, Uncompressed),
                    I::new_li(x3, 540921, Compressed),
                    I::new_and(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_or, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            // Bitwise or with all ones is all-ones.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, !0, Uncompressed),
                    I::new_li(x3, 13872, Compressed),
                    I::new_or(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise or with itself is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_or(x2, x1, x1, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise or with 0 is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 540921, Uncompressed),
                    I::new_li(x3, 0, Compressed),
                    I::new_or(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 0xF0F0, Compressed),
                    I::new_x64_or_immediate(x2, x1, 0x0F0F, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 0xFFFF);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 0x0000, Compressed),
                    I::new_x64_or_immediate(x2, x1, 0x5555, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 0x5555);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_x64_mul, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 5, Uncompressed),
                    I::new_li(x3, 10, Compressed),
                    I::new_mul(x2, x1, x3, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), 50);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, !0, Compressed),
                    I::new_mul(x2, x1, x1, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(
                        core.hart.xregisters.read_nz(x2),
                        u64::MAX.wrapping_mul(u64::MAX)
                    );
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, -20, Compressed),
                    I::new_li(x3, 40, Uncompressed),
                    I::new_mul(x2, x1, x3, Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), -800i64 as u64);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_x32_mul, F, {
        use crate::machine_state::registers::a0;
        use crate::machine_state::registers::a1;
        use crate::machine_state::registers::a2;

        let test_x32_mul = |value1: i64,
                            value2: i64,
                            expected_result: u64,
                            instruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, value1, instruction_width),
                    I::new_li(nz::a1, value2, instruction_width),
                    I::new_x32_mul(a2, a0, a1, instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), expected_result);
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            test_x32_mul(10, 5, 50, Uncompressed),
            // Test that we truncate to 32 bits before sign extending
            // 2^32 * 2 = 2^33, but truncated to 32 bits = 0
            test_x32_mul(0x1_0000_0000, 2, 0, Compressed),
            // Test with negative numbers
            // -10 * 5 = -50, sign extended to 64 bits
            test_x32_mul(-10, 5, -50i64 as u64, Compressed),
            // Test with large 32-bit values that overflow
            // INT32_MAX * 2 = 0xFFFFFFFE (truncated to 32 bits)
            // Sign extended to 64 bits: 0xFFFFFFFFFFFFFFFE
            test_x32_mul(0x7FFFFFFF, 2, -2i64 as u64, Compressed),
            // Test with values that would produce different results in 32-bit vs 64-bit multiplication
            // In 32-bit: INT32_MIN * INT32_MIN = 0 (truncated)
            // In 64-bit: would be 0x4000000000000000
            test_x32_mul(0x80000000, 0x80000000, 0, Compressed),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_div_jit, F, {
        use crate::machine_state::registers::nz;
        use crate::machine_state::registers::*;

        let test_division = |numerator: i64, denominator: i64, expected: u64| {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(nz::a0, numerator, Uncompressed),
                    I::new_li(nz::a1, denominator, Compressed),
                    I::new_x64_div_signed(nz::a2, a0, a1, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_expected_steps(4)
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(nz::a2), expected);
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // check standard division
            test_division(10, 2, 5),
            // check signed division.
            test_division(10, -2, -5_i64 as u64),
            test_division(-10, -2, 5),
            // check division by zero
            test_division(i64::MAX, 0, -1_i64 as u64),
            // check when `val(rs2) == -1` but `val(rs1) != i64::MIN`.
            test_division(38294, -1, -38294_i64 as u64),
            // check when `val(rs1) == i64::MIN` but `val(rs2) != -1`.
            test_division(i64::MIN, i64::MIN, 1),
            // check when `val(rs1) == i64::MIN` and `val(rs2) == -1`.
            test_division(i64::MIN, -1, i64::MIN as u64),
            // check division of a smaller-magnitude number by a larger-magnitude number.
            test_division(40, -80, 0),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_j, F, {
        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                // Jumping to the next instruction should exit the block
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_nop(Compressed),
                    I::new_j(2, Compressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 6);
                }))
                .set_expected_steps(3)
                .build(),
            ScenarioBuilder::default()
                // Jump past 0 - in both worlds we should wrap around.
                .set_instructions(&[I::new_j(-4, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), u64::MAX - 3);
                }))
                .set_expected_steps(1)
                .build(),
            ScenarioBuilder::default()
                // Jump past u64::MAX - in both worlds we should wrap around but not
                // execute functions past the end of the block (the jump).
                .set_instructions(&[
                    I::new_nop(Uncompressed),
                    I::new_nop(Uncompressed),
                    I::new_j(i64::MAX, Uncompressed),
                    I::new_nop(Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_initial_pc((i64::MAX - 5) as u64)
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 1);
                }))
                .set_expected_steps(3)
                .build(),
            ScenarioBuilder::default()
                // jump by nothing
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_j(0, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 2);
                }))
                .set_expected_steps(2)
                .build(),
            ScenarioBuilder::default()
                // jumping to start of the block should exit the block in both interpreted and jitted world
                .set_instructions(&[
                    I::new_nop(Compressed),
                    I::new_nop(Compressed),
                    I::new_j(-4, Compressed),
                    I::new_nop(Uncompressed),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), 0);
                }))
                .set_expected_steps(3)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_jump_instructions, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let test_jr = |base_reg: NonZeroXRegister,
                       base_val: i64,
                       expected_pc: u64,
                       instruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(base_reg, base_val, instruction_width),
                    I::new_jr(base_reg, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), expected_pc);
                }))
                .set_expected_steps(2)
                .build()
        };

        let test_jr_imm = |base_reg: NonZeroXRegister,
                           base_val: i64,
                           offset: i64,
                           expected_pc: u64,
                           instruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(base_reg, base_val, instruction_width),
                    I::new_jr_imm(base_reg, offset, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), expected_pc);
                }))
                .set_expected_steps(2)
                .build()
        };

        let test_jalr = |base_reg: NonZeroXRegister,
                         base_val: i64,
                         rd: NonZeroXRegister,
                         expected_pc: u64,
                         expected_rd: u64,
                         instruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(base_reg, base_val, instruction_width),
                    I::new_jalr(rd, base_reg, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), expected_pc);
                    assert_eq!(core.hart.xregisters.read_nz(rd), expected_rd);
                }))
                .set_expected_steps(2)
                .build()
        };

        let test_jalr_imm = |base_reg: NonZeroXRegister,
                             base_val: i64,
                             offset: i64,
                             rd: NonZeroXRegister,
                             expected_pc: u64,
                             expected_rd: u64,
                             instruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(base_reg, base_val, instruction_width),
                    I::new_jalr_imm(rd, base_reg, offset, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), expected_pc);
                    assert_eq!(core.hart.xregisters.read_nz(rd), expected_rd);
                }))
                .set_expected_steps(2)
                .build()
        };

        let test_jalr_absolute = |target: i64,
                                  rd: NonZeroXRegister,
                                  instruction_width: InstrWidth,
                                  expected_rd: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_jalr_absolute(rd, target, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), target as u64);
                    assert_eq!(core.hart.xregisters.read_nz(rd), expected_rd);
                }))
                .set_expected_steps(1)
                .build()
        };

        let test_j_absolute = |target: i64, instruction_width: InstrWidth| -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_j_absolute(target, instruction_width),
                    I::new_nop(instruction_width),
                ])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), target as u64);
                }))
                .set_expected_steps(1)
                .build()
        };

        let test_jal = |offset: i64,
                        initial_pc: u64,
                        expected_pc: u64,
                        expected_x1: u64,
                        intruction_width: InstrWidth|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_instructions(&[I::new_jal(x1, offset, intruction_width)])
                .set_initial_pc(initial_pc)
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.pc.read(), expected_pc);
                    assert_eq!(core.hart.xregisters.read_nz(x1), expected_x1);
                }))
                .set_expected_steps(1)
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // Test jr
            test_jr(x2, 10, 10, Compressed),
            test_jr(x6, 0, 0, Uncompressed),
            // Test jr_imm
            test_jr_imm(x2, 10, 10, 20, Compressed),
            test_jr_imm(x6, 10, -10, 0, Uncompressed),
            // Test jalr
            test_jalr(x2, 100_000, x1, 100_000, 8, Uncompressed),
            test_jalr(x6, 0, x3, 0, 4, Compressed),
            // Test jalr_imm
            test_jalr_imm(x1, 10, 10, x2, 20, 4, Compressed),
            test_jalr_imm(x1, 1000, -10, x2, 990, 8, Uncompressed),
            // Test jalr_absolute
            test_jalr_absolute(10, x1, Compressed, 2),
            test_jalr_absolute(0, x3, Uncompressed, 4),
            // Test j_absolute
            test_j_absolute(10, Compressed),
            test_j_absolute(0, Uncompressed),
            // Test jal
            test_jal(10, 0, 10, 2, Compressed),
            test_jal(-10, 10, 0, 12, Compressed),
            test_jal(1000, 1000, 2000, 1004, Uncompressed),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_addi, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_is_five = assert_hook!(core, F, {
            assert_eq!(core.hart.xregisters.read_nz(x1), 5);
        });

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x1, 2, Compressed),
                    I::new_addi(x1, x1, 3, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x1, i64::MAX, Compressed),
                    I::new_addi(x1, x1, i64::MAX, Compressed),
                    I::new_addi(x1, x1, 7, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five.clone())
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_addi(x1, x3, 7, Compressed),
                    I::new_addi(x1, x1, -2, Uncompressed),
                ])
                .set_assert_hook(assert_x1_is_five)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_andi, F, {
        use Instruction as I;

        use crate::machine_state::registers::NonZeroXRegister::*;

        let assert_x1_and_x2_equal = assert_hook!(core, F, {
            assert_eq!(
                core.hart.xregisters.read_nz(x1),
                core.hart.xregisters.read_nz(x2)
            );
        });

        let scenarios: &[Scenario<F>] = &[
            // Bitwise and with all ones is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 13872, Uncompressed),
                    I::new_andi(x2, x1, !0, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise and with itself is self.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 49666, Uncompressed),
                    I::new_andi(x2, x1, 49666, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal.clone())
                .build(),
            // Bitwise and with 0 is 0.
            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(x1, 0, Uncompressed),
                    I::new_andi(x2, x1, 50230, Compressed),
                ])
                .set_assert_hook(assert_x1_and_x2_equal)
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_set_less_than, F, {
        use crate::machine_state::registers::XRegister::*;

        const TRUE: u64 = 1;
        const FALSE: u64 = 0;

        let test_slt = |constructor: fn(NonZeroXRegister, XRegister, XRegister) -> I,
                        lhs: (XRegister, i64),
                        rhs: (XRegister, i64),
                        expected: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.hart.xregisters.write(lhs.0, lhs.1 as u64);
                    core.hart.xregisters.write(rhs.0, rhs.1 as u64);
                }))
                .set_instructions(&[constructor(nz::ra, lhs.0, rhs.0)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(
                        expected,
                        core.hart.xregisters.read_nz(nz::ra),
                        "Expected {expected} for Slt* lhs: {lhs:?}, rhs: {rhs:?}"
                    )
                }))
                .build()
        };

        let test_slt_imm = |constructor: fn(NonZeroXRegister, XRegister, i64) -> I,
                            lhs: (XRegister, i64),
                            rhs: i64,
                            expected: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.hart.xregisters.write(lhs.0, lhs.1 as u64);
                }))
                .set_instructions(&[constructor(nz::ra, lhs.0, rhs)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(
                        expected,
                        core.hart.xregisters.read_nz(nz::ra),
                        "Expected {expected} for Slt* lhs: {lhs:?}, rhs: {rhs:?}"
                    )
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // -------------------------
            // equal values always false
            // -------------------------
            // Slt
            test_slt(I::new_set_less_than_signed, (x1, 1), (x2, 1), FALSE),
            test_slt(I::new_set_less_than_signed, (x0, 1), (x2, 0), FALSE),
            test_slt(I::new_set_less_than_signed, (x3, -1), (x2, -1), FALSE),
            // Sltu
            test_slt(I::new_set_less_than_unsigned, (x1, 1), (x2, 1), FALSE),
            test_slt(I::new_set_less_than_unsigned, (x0, 1), (x2, 0), FALSE),
            test_slt(I::new_set_less_than_unsigned, (x3, -1), (x2, -1), FALSE),
            // Slti
            test_slt_imm(I::new_set_less_than_immediate_signed, (x1, 1), 1, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x0, 1), 0, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x3, -1), -1, FALSE),
            // Sltiu
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x1, 1), 1, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x0, 1), 0, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x3, -1), -1, FALSE),
            // --------------------------------
            // greater than values always false
            // --------------------------------
            // Slt
            test_slt(I::new_set_less_than_signed, (x1, 3), (x2, 1), FALSE),
            test_slt(I::new_set_less_than_signed, (x0, 0), (x2, -2), FALSE),
            test_slt(I::new_set_less_than_signed, (x3, -1), (x2, -5), FALSE),
            // Sltu
            test_slt(I::new_set_less_than_unsigned, (x1, 1), (x2, 1), FALSE),
            test_slt(I::new_set_less_than_unsigned, (x2, 5), (x0, 0), FALSE),
            test_slt(I::new_set_less_than_unsigned, (x3, -1), (x2, 2), FALSE),
            // Slti
            test_slt_imm(I::new_set_less_than_immediate_signed, (x1, 2), 1, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x5, 1), 0, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x3, -5), -6, FALSE),
            // Sltiu
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x1, 5), 1, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x3, -1), 15, FALSE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x3, -1), -6, FALSE),
            // ----------------------------
            // less than values always true
            // ----------------------------
            // Slt
            test_slt(I::new_set_less_than_signed, (x1, 2), (x2, 5), TRUE),
            test_slt(I::new_set_less_than_signed, (x0, 0), (x2, 3), TRUE),
            test_slt(I::new_set_less_than_signed, (x3, -5), (x2, -3), TRUE),
            // Sltu
            test_slt(I::new_set_less_than_unsigned, (x1, 1), (x2, -1), TRUE),
            test_slt(I::new_set_less_than_unsigned, (x0, 0), (x3, 5), TRUE),
            test_slt(I::new_set_less_than_unsigned, (x3, -2), (x2, -1), TRUE),
            // Slti
            test_slt_imm(I::new_set_less_than_immediate_signed, (x1, 2), 5, TRUE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x5, 0), 3, TRUE),
            test_slt_imm(I::new_set_less_than_immediate_signed, (x3, -6), -5, TRUE),
            // Sltiu
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x1, 3), 5, TRUE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x3, 5), -15, TRUE),
            test_slt_imm(I::new_set_less_than_immediate_unsigned, (x3, -7), -6, TRUE),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_branch, F, {
        let test_branch =
            |non_branch: fn(NonZeroXRegister, NonZeroXRegister, i64, InstrWidth) -> I,
             branch: fn(NonZeroXRegister, NonZeroXRegister, i64, InstrWidth) -> I,
             lhs: i64,
             rhs: i64|
             -> Scenario<F> {
                let initial_pc: u64 = 0x1000;
                let imm: i64 = -0x2000;
                let expected_pc_branch = initial_pc.wrapping_add(imm as u64).wrapping_add(8);

                ScenarioBuilder::default()
                    .set_initial_pc(initial_pc)
                    .set_instructions(&[
                        I::new_li(nz::a1, lhs, InstrWidth::Compressed),
                        I::new_li(nz::a2, rhs, InstrWidth::Compressed),
                        non_branch(nz::a1, nz::a2, imm, InstrWidth::Uncompressed),
                        branch(nz::a1, nz::a2, imm, InstrWidth::Uncompressed),
                        I::new_nop(InstrWidth::Compressed),
                    ])
                    .set_expected_steps(4)
                    .set_assert_hook(assert_hook!(core, F, {
                        assert_eq!(
                            expected_pc_branch,
                            core.hart.pc.read(),
                            "Expected {expected_pc_branch} pc for B*Zero cmp {lhs}, {rhs}"
                        )
                    }))
                    .build()
            };

        let scenarios: &[Scenario<F>] = &[
            // Equality
            test_branch(I::new_branch_equal, I::new_branch_not_equal, 2, 3),
            test_branch(I::new_branch_not_equal, I::new_branch_equal, 2, 2),
            test_branch(I::new_branch_equal, I::new_branch_not_equal, 2, -3),
            // LessThanUnsigned + GreaterThanOrEqualUnsigned
            test_branch(
                I::new_branch_less_than_unsigned,
                I::new_branch_greater_than_or_equal_unsigned,
                3,
                2,
            ),
            test_branch(
                I::new_branch_less_than_unsigned,
                I::new_branch_greater_than_or_equal_unsigned,
                2,
                2,
            ),
            test_branch(
                I::new_branch_greater_than_or_equal_unsigned,
                I::new_branch_less_than_unsigned,
                2,
                -3,
            ),
            // LessThanSigned + GreaterThanOrEqualSigned
            test_branch(
                I::new_branch_less_than_signed,
                I::new_branch_greater_than_or_equal_signed,
                3,
                2,
            ),
            test_branch(
                I::new_branch_less_than_signed,
                I::new_branch_greater_than_or_equal_signed,
                2,
                2,
            ),
            test_branch(
                I::new_branch_less_than_signed,
                I::new_branch_greater_than_or_equal_signed,
                2,
                -3,
            ),
            test_branch(
                I::new_branch_greater_than_or_equal_signed,
                I::new_branch_less_than_signed,
                -4,
                -3,
            ),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_branch_compare_zero, F, {
        let test_branch_compare_zero = |non_branch: fn(NonZeroXRegister, i64, InstrWidth) -> I,
                                        branch: fn(NonZeroXRegister, i64, InstrWidth) -> I,
                                        val: i64|
         -> Scenario<F> {
            let initial_pc: u64 = 0x1000;
            let imm: i64 = 0x2000;
            let expected_pc_branch = initial_pc + imm as u64 + 4;

            ScenarioBuilder::default()
                .set_initial_pc(initial_pc)
                .set_instructions(&[
                    I::new_li(nz::ra, val, InstrWidth::Compressed),
                    non_branch(nz::ra, imm, InstrWidth::Compressed),
                    branch(nz::ra, imm, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                .set_expected_steps(3)
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(
                        expected_pc_branch,
                        core.hart.pc.read(),
                        "Expected {expected_pc_branch} pc for B*Zero cmp {val:?}"
                    )
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // Equality
            test_branch_compare_zero(I::new_branch_equal_zero, I::new_branch_not_equal_zero, 12),
            test_branch_compare_zero(I::new_branch_not_equal_zero, I::new_branch_equal_zero, 0),
            test_branch_compare_zero(I::new_branch_equal_zero, I::new_branch_not_equal_zero, -12),
            // LessThan + GreaterThanOrEqual
            test_branch_compare_zero(
                I::new_branch_less_than_zero,
                I::new_branch_greater_than_or_equal_zero,
                12,
            ),
            test_branch_compare_zero(
                I::new_branch_less_than_zero,
                I::new_branch_greater_than_or_equal_zero,
                0,
            ),
            test_branch_compare_zero(
                I::new_branch_greater_than_or_equal_zero,
                I::new_branch_less_than_zero,
                -12,
            ),
            // LessThanOrEqual + GreaterThan
            test_branch_compare_zero(
                I::new_branch_less_than_or_equal_zero,
                I::new_branch_greater_than_zero,
                12,
            ),
            test_branch_compare_zero(
                I::new_branch_greater_than_zero,
                I::new_branch_less_than_or_equal_zero,
                0,
            ),
            test_branch_compare_zero(
                I::new_branch_greater_than_zero,
                I::new_branch_less_than_or_equal_zero,
                -12,
            ),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_unknown, F, {
        let scenarios: &[Scenario<F>] = &[ScenarioBuilder::default()
            .set_expected_steps(2)
            .set_instructions(&[
                I::new_nop(Uncompressed),
                I::new_unknown(Compressed),
                I::new_nop(Uncompressed),
            ])
            .build()];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_ecall, F, {
        let scenario: Scenario<F> = ScenarioBuilder::default()
            .set_expected_steps(1)
            .set_instructions(&[
                I::new_nop(Uncompressed),
                I::new_ecall(),
                I::new_nop(Uncompressed),
            ])
            .build();

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        scenario.run(&mut jit, &mut interpreted_bb);
    });

    backend_test!(test_jit_recovers_from_compilation_failure, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        // Arrange
        let failure_scenarios: &[&[I]] = &[
            &[
                // does not currently support lowering.
                I::new_fadds(x1, x1, x1, Uncompressed),
            ],
            &[
                I::new_nop(Uncompressed),
                // does not currently support lowering.
                I::new_fadds(x1, x1, x1, Uncompressed),
            ],
        ];

        let success: &[I] = &[I::new_nop(Compressed)];

        let mut manager = F::manager();

        for failure in failure_scenarios.iter() {
            let mut jit = JIT::<M4K, F::Manager>::new().unwrap();

            let mut jitted = MachineCoreState::<M4K, _>::new(&mut manager);
            let mut block = Interpreted::<M4K, _>::new(&mut manager);

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

            let mut jitted_res = Ok(());
            unsafe {
                // # Safety - the jit is not dropped until after we
                //            exit the block.
                (fun)(
                    null(),
                    &mut jitted,
                    initial_pc,
                    &mut jitted_steps,
                    &mut jitted_res,
                    null(),
                )
            };

            assert!(jitted_res.is_ok());
            assert_eq!(jitted_steps, success.len());
        }
    });

    backend_test!(test_add_immediate_to_pc, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let scenarios: &[Scenario<F>] = &[
            ScenarioBuilder::default()
                .set_initial_pc(1000)
                .set_instructions(&[I::new_add_immediate_to_pc(x1, 4096, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x1), 5096);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_instructions(&[I::new_add_immediate_to_pc(x1, 0xFFFFF000, Uncompressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x1), 0xFFFFF000);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_initial_pc(1000)
                .set_instructions(&[I::new_add_immediate_to_pc(x1, -4096, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x1), -3096_i64 as u64);
                }))
                .build(),
            ScenarioBuilder::default()
                .set_initial_pc(1000)
                .set_instructions(&[I::new_add_immediate_to_pc(x1, 20, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x1), 1020);
                }))
                .build(),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_shift_reg, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let shift_reg = |constructor: fn(
            NonZeroXRegister,
            NonZeroXRegister,
            NonZeroXRegister,
            InstrWidth,
        ) -> I,
                         lhs: (NonZeroXRegister, i64),
                         rhs: (NonZeroXRegister, i64),
                         expected: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.hart.xregisters.write_nz(lhs.0, lhs.1 as u64);
                    core.hart.xregisters.write_nz(rhs.0, rhs.1 as u64);
                }))
                .set_instructions(&[constructor(x2, lhs.0, rhs.0, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(
                        expected,
                        core.hart.xregisters.read_nz(x2),
                        "Expected {expected} for Shift* lhs: {lhs:?}, rhs: {rhs:?}"
                    )
                }))
                .build()
        };

        let shift_reg_word =
            |constructor: fn(NonZeroXRegister, XRegister, XRegister, InstrWidth) -> I,
             lhs: (XRegister, i64),
             rhs: (XRegister, i64),
             expected: u64|
             -> Scenario<F> {
                ScenarioBuilder::default()
                    .set_setup_hook(setup_hook!(core, F, {
                        core.hart.xregisters.write(lhs.0, lhs.1 as u64);
                        core.hart.xregisters.write(rhs.0, rhs.1 as u64);
                    }))
                    .set_instructions(&[constructor(x2, lhs.0, rhs.0, Compressed)])
                    .set_assert_hook(assert_hook!(core, F, {
                        assert_eq!(
                            expected,
                            core.hart.xregisters.read_nz(x2),
                            "Expected {expected} for Shift* lhs: {lhs:?}, rhs: {rhs:?}"
                        )
                    }))
                    .build()
            };

        let scenarios: &[Scenario<F>] = &[
            shift_reg(I::new_shift_left, (x1, 1), (x3, 1), 2),
            shift_reg(I::new_shift_left, (x1, 1), (x3, 63), 0x8000_0000_0000_0000),
            shift_reg(I::new_shift_left, (x1, 2), (x3, 63), 0),
            shift_reg(I::new_shift_left, (x1, 1), (x3, 126), 0x4000_0000_0000_0000),
            shift_reg(I::new_shift_left, (x1, -16), (x3, 2), -64_i64 as u64),
            shift_reg(I::new_shift_right_unsigned, (x1, 2), (x3, 1), 1),
            shift_reg(I::new_shift_right_unsigned, (x1, !0), (x3, 63), 1),
            shift_reg(
                I::new_shift_right_unsigned,
                (x1, 0x7FFF_FFFF_FFFF_FFFF),
                (x3, 63),
                0,
            ),
            shift_reg(I::new_shift_right_unsigned, (x1, !0), (x3, 126), 3),
            shift_reg(
                I::new_shift_right_unsigned,
                (x1, -8),
                (x3, 2),
                0x3FFF_FFFF_FFFF_FFFE,
            ),
            shift_reg(I::new_shift_right_signed, (x1, 2), (x3, 1), 1),
            shift_reg(I::new_shift_right_signed, (x1, !0), (x3, 63), !0),
            shift_reg(
                I::new_shift_right_signed,
                (x1, 0x7FFF_FFFF_FFFF_FFFF),
                (x3, 62),
                1,
            ),
            shift_reg(I::new_shift_right_signed, (x1, !0), (x3, 126), !0),
            shift_reg(I::new_shift_right_signed, (x1, -8), (x3, 2), -2_i64 as u64),
            // X32ShiftLeft tests
            shift_reg_word(
                I::new_x32_shift_left,
                (XRegister::x1, 1),
                (XRegister::x3, 1),
                2,
            ),
            shift_reg_word(
                I::new_x32_shift_left,
                (XRegister::x1, 1),
                (XRegister::x3, 31),
                0xFFFF_FFFF_8000_0000,
            ),
            shift_reg_word(
                I::new_x32_shift_left,
                (XRegister::x1, 2),
                (XRegister::x3, 31),
                0,
            ),
            shift_reg_word(
                I::new_x32_shift_left,
                (XRegister::x1, 1),
                (XRegister::x3, 95),
                0xFFFF_FFFF_8000_0000,
            ),
            // X32ShiftRightUnsigned tests
            shift_reg_word(
                I::new_x32_shift_right_unsigned,
                (XRegister::x1, 2),
                (XRegister::x3, 1),
                1,
            ),
            shift_reg_word(
                I::new_x32_shift_right_unsigned,
                (XRegister::x1, 0x80000000),
                (XRegister::x3, 31),
                1,
            ),
            shift_reg_word(
                I::new_x32_shift_right_unsigned,
                (XRegister::x1, 1),
                (XRegister::x3, 31),
                0,
            ),
            shift_reg_word(
                I::new_x32_shift_right_unsigned,
                (XRegister::x1, 0x80000000),
                (XRegister::x3, 95),
                1,
            ),
            // X32ShiftRightSigned tests
            shift_reg_word(
                I::new_x32_shift_right_signed,
                (XRegister::x1, 2),
                (XRegister::x3, 1),
                1,
            ),
            shift_reg_word(
                I::new_x32_shift_right_signed,
                (XRegister::x1, 0x80000000),
                (XRegister::x3, 31),
                0xFFFF_FFFF_FFFF_FFFF,
            ),
            shift_reg_word(
                I::new_x32_shift_right_signed,
                (XRegister::x1, 0x40000000),
                (XRegister::x3, 31),
                0,
            ),
            shift_reg_word(
                I::new_x32_shift_right_signed,
                (XRegister::x1, 0x80000000),
                (XRegister::x3, 95),
                0xFFFF_FFFF_FFFF_FFFF,
            ),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_shift_imm, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let shift_imm =
            |constructor: fn(NonZeroXRegister, NonZeroXRegister, i64, InstrWidth) -> I,
             lhs: (NonZeroXRegister, i64),
             imm: i64,
             expected: u64|
             -> Scenario<F> {
                ScenarioBuilder::default()
                    .set_setup_hook(setup_hook!(core, F, {
                        core.hart.xregisters.write_nz(lhs.0, lhs.1 as u64);
                    }))
                    .set_instructions(&[constructor(x2, lhs.0, imm, Compressed)])
                    .set_assert_hook(assert_hook!(core, F, {
                        assert_eq!(
                            expected,
                            core.hart.xregisters.read_nz(x2),
                            "Expected {expected} for Shift* lhs: {lhs:?}, imm: {imm}"
                        )
                    }))
                    .build()
            };

        let scenarios: &[Scenario<F>] = &[
            shift_imm(I::new_shift_left_immediate, (x1, 1), 1, 2),
            shift_imm(
                I::new_shift_left_immediate,
                (x1, 1),
                63,
                0x8000_0000_0000_0000,
            ),
            shift_imm(I::new_shift_left_immediate, (x1, 2), 63, 0),
            shift_imm(I::new_shift_left_immediate, (x1, -16), 2, -64_i64 as u64),
            shift_imm(I::new_shift_right_immediate_unsigned, (x1, 2), 1, 1),
            shift_imm(I::new_shift_right_immediate_unsigned, (x1, !0), 63, 1),
            shift_imm(
                I::new_shift_right_immediate_unsigned,
                (x1, 0x7FFF_FFFF_FFFF_FFFF),
                63,
                0,
            ),
            shift_imm(
                I::new_shift_right_immediate_unsigned,
                (x1, -8),
                2,
                0x3FFF_FFFF_FFFF_FFFE,
            ),
            shift_imm(I::new_shift_right_immediate_signed, (x1, 2), 1, 1),
            shift_imm(I::new_shift_right_immediate_signed, (x1, !0), 63, !0),
            shift_imm(
                I::new_shift_right_immediate_signed,
                (x1, 0x7FFF_FFFF_FFFF_FFFF),
                62,
                1,
            ),
            shift_imm(
                I::new_shift_right_immediate_signed,
                (x1, -8),
                2,
                -2_i64 as u64,
            ),
            // X32ShiftLeftImmediate tests
            shift_imm(I::new_x32_shift_left_immediate, (x1, 1), 1, 2),
            shift_imm(
                I::new_x32_shift_left_immediate,
                (x1, 1),
                31,
                0xFFFF_FFFF_8000_0000,
            ),
            shift_imm(I::new_x32_shift_left_immediate, (x1, 2), 31, 0),
            // X32ShiftRightImmediateUnsigned tests
            shift_imm(I::new_x32_shift_right_immediate_unsigned, (x1, 2), 1, 1),
            shift_imm(
                I::new_x32_shift_right_immediate_unsigned,
                (x1, 0x80000000),
                31,
                1,
            ),
            shift_imm(I::new_x32_shift_right_immediate_unsigned, (x1, 1), 31, 0),
            // X32ShiftRightImmediateSigned tests
            shift_imm(I::new_x32_shift_right_immediate_signed, (x1, 2), 1, 1),
            shift_imm(
                I::new_x32_shift_right_immediate_signed,
                (x1, 0x80000000),
                31,
                0xFFFF_FFFF_FFFF_FFFF,
            ),
            shift_imm(
                I::new_x32_shift_right_immediate_signed,
                (x1, 0x40000000),
                31,
                0,
            ),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_store, F, {
        use crate::machine_state::registers::NonZeroXRegister as NZ;
        use crate::machine_state::registers::XRegister::*;

        type ConstructStoreFn =
            fn(rs1: XRegister, rs2: XRegister, imm: i64, width: InstrWidth) -> I;

        const MEMORY_SIZE: u64 = M4K::TOTAL_BYTES as u64;
        const XREG_VALUE: u64 = 0xFFEEDDCCBBAA9988;

        let valid_store = |constructor: ConstructStoreFn, imm: u64, expected: u64| {
            const STORE_ADDRESS_BASE: u64 = MEMORY_SIZE / 2;

            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(NZ::x1, STORE_ADDRESS_BASE as i64, InstrWidth::Compressed),
                    I::new_li(NZ::x2, XREG_VALUE as i64, InstrWidth::Compressed),
                    constructor(x1, x2, imm as i64, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                .set_expected_steps(4)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.main_memory.read(STORE_ADDRESS_BASE + imm).unwrap();

                    assert_eq!(value, expected, "Found {value:x}, expected {expected:x}");
                }))
                .build()
        };

        let invalid_store = |constructor: ConstructStoreFn, width: LoadStoreWidth| {
            // an address that, with the immediate of 16, will be out of bounds by one byte
            let store_address_base = MEMORY_SIZE - 15 - width as u64;
            let store_address_offset = 16;

            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(NZ::x1, store_address_base as i64, InstrWidth::Compressed),
                    I::new_li(NZ::x2, XREG_VALUE as i64, InstrWidth::Compressed),
                    constructor(
                        x1,
                        x2,
                        store_address_offset as i64,
                        InstrWidth::Uncompressed,
                    ),
                    I::new_nop(InstrWidth::Compressed),
                ])
                // the load will fail due to being out of bounds
                .set_expected_steps(3)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.main_memory.read(MEMORY_SIZE - 8).unwrap();

                    assert_eq!(value, 0, "Found {value:x}, but expected store to fail");
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // check stores - differing imm value to ensure both
            // aligned & unaligned stores are supported
            valid_store(I::new_x64_store, 8, XREG_VALUE),
            valid_store(I::new_x64_store, 5, XREG_VALUE),
            valid_store(I::new_x32_store, 4, XREG_VALUE as u32 as u64),
            valid_store(I::new_x32_store, 3, XREG_VALUE as u32 as u64),
            valid_store(I::new_x16_store, 2, XREG_VALUE as u16 as u64),
            valid_store(I::new_x16_store, 1, XREG_VALUE as u16 as u64),
            // byte load always aligned
            valid_store(I::new_x8_store, 0, XREG_VALUE as u8 as u64),
            // invalid stores: out of bounds
            invalid_store(I::new_x64_store, LoadStoreWidth::Double),
            invalid_store(I::new_x32_store, LoadStoreWidth::Word),
            invalid_store(I::new_x16_store, LoadStoreWidth::Half),
            invalid_store(I::new_x8_store, LoadStoreWidth::Byte),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_load, F, {
        use crate::machine_state::registers::NonZeroXRegister as NZ;
        use crate::machine_state::registers::XRegister::*;

        type ConstructLoadFn = fn(rd: XRegister, rs1: XRegister, imm: i64, width: InstrWidth) -> I;

        const MEMORY_SIZE: u64 = M4K::TOTAL_BYTES as u64;

        let valid_load = |new_load: ConstructLoadFn, imm: u64, expected: u64| {
            const LOAD_ADDRESS_BASE: u64 = MEMORY_SIZE / 2;

            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.main_memory
                        .write(LOAD_ADDRESS_BASE + imm, expected)
                        .unwrap();
                }))
                .set_instructions(&[
                    I::new_li(NZ::x1, LOAD_ADDRESS_BASE as i64, InstrWidth::Compressed),
                    new_load(x2, x1, imm as i64, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                .set_expected_steps(3)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.hart.xregisters.read(x2);
                    assert_eq!(value, expected, "Found {value:x}, expected {expected:x}");
                }))
                .build()
        };

        let invalid_load = |new_load: ConstructLoadFn, width: LoadStoreWidth| {
            // an address that, with the immediate of 16, will be out of bounds by one byte
            let load_address_base = MEMORY_SIZE - 15 - width as u64;
            let load_address_offset = 16;

            ScenarioBuilder::default()
                .set_instructions(&[
                    I::new_li(NZ::x1, load_address_base as i64, InstrWidth::Compressed),
                    new_load(x2, x1, load_address_offset as i64, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                // the load will fail due to being out of bounds
                .set_expected_steps(2)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.hart.xregisters.read(x2);
                    assert_eq!(value, 0, "Found {value:x}, but expected load to fail");
                }))
                .build()
        };

        const XREG_VALUE: u64 = 0xFFEEDDCCBBAA9988;

        let scenarios: &[Scenario<F>] = &[
            // check loads - differing imm value to ensure both
            // aligned & unaligned loads are supported
            valid_load(I::new_x64_load_signed, 8, XREG_VALUE),
            valid_load(I::new_x64_load_signed, 5, XREG_VALUE),
            valid_load(I::new_x32_load_signed, 4, XREG_VALUE as i32 as u64),
            valid_load(I::new_x32_load_unsigned, 4, XREG_VALUE as u32 as u64),
            valid_load(I::new_x32_load_signed, 3, XREG_VALUE as i32 as u64),
            valid_load(I::new_x32_load_unsigned, 3, XREG_VALUE as u32 as u64),
            valid_load(I::new_x16_load_signed, 2, XREG_VALUE as i16 as u64),
            valid_load(I::new_x16_load_unsigned, 2, XREG_VALUE as u16 as u64),
            valid_load(I::new_x16_load_signed, 1, XREG_VALUE as i16 as u64),
            valid_load(I::new_x16_load_unsigned, 1, XREG_VALUE as u16 as u64),
            // byte load always aligned
            valid_load(I::new_x8_load_signed, 0, XREG_VALUE as i8 as u64),
            valid_load(I::new_x8_load_unsigned, 0, XREG_VALUE as u8 as u64),
            // invalid loads: out of bounds
            invalid_load(I::new_x64_load_signed, LoadStoreWidth::Double),
            invalid_load(I::new_x32_load_signed, LoadStoreWidth::Word),
            invalid_load(I::new_x32_load_unsigned, LoadStoreWidth::Word),
            invalid_load(I::new_x16_load_signed, LoadStoreWidth::Half),
            invalid_load(I::new_x16_load_unsigned, LoadStoreWidth::Half),
            invalid_load(I::new_x8_load_signed, LoadStoreWidth::Byte),
            invalid_load(I::new_x8_load_unsigned, LoadStoreWidth::Byte),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_xor, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let bitwise_test_xor_immediate =
            |lhs_reg: NonZeroXRegister, lhs_val: u64, imm: i64, expected: u64| -> Scenario<F> {
                ScenarioBuilder::default()
                    .set_setup_hook(setup_hook!(core, F, {
                        core.hart.xregisters.write_nz(lhs_reg, lhs_val);
                    }))
                    .set_instructions(&[I::new_x64_xor_immediate(x2, lhs_reg, imm, Compressed)])
                    .set_assert_hook(assert_hook!(core, F, {
                        assert_eq!(core.hart.xregisters.read_nz(x2), expected);
                    }))
                    .build()
            };

        let bitwise_test_xor = |lhs_reg: NonZeroXRegister,
                                lhs_val: u64,
                                rhs_reg: NonZeroXRegister,
                                rhs_val: u64,
                                expected: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.hart.xregisters.write_nz(lhs_reg, lhs_val);
                    core.hart.xregisters.write_nz(rhs_reg, rhs_val);
                }))
                .set_instructions(&[I::new_x64_xor(x2, lhs_reg, rhs_reg, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), expected);
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // XOR immediate tests
            bitwise_test_xor_immediate(x1, 0xF0F0, 0x0F0F, 0xFFFF),
            bitwise_test_xor_immediate(x1, 0xAAAA, 0x5555, 0xFFFF),
            bitwise_test_xor_immediate(x1, 0xFFF0, 0x0FFF, 0xF00F),
            // XOR register tests
            bitwise_test_xor(x1, 0xF0F0, x3, 0x0F0F, 0xFFFF),
            bitwise_test_xor(x1, 0xAAAA, x3, 0x5555, 0xFFFF),
            bitwise_test_xor(x1, 0xFFF0, x3, 0x0FFF, 0xF00F),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_x64_atomic, F, {
        use crate::machine_state::registers::NonZeroXRegister as NZ;
        use crate::machine_state::registers::XRegister::*;

        type ConstructAtomicFn = fn(
            rd: XRegister,
            rs1: XRegister,
            rs2: XRegister,
            aq: bool,
            rl: bool,
            width: InstrWidth,
        ) -> I;

        const MEMORY_SIZE: u64 = M4K::TOTAL_BYTES as u64;

        const ADDRESS_BASE_ATOMICS: u64 = MEMORY_SIZE / 2;

        let valid_x64_atomic = |constructor: ConstructAtomicFn,
                                val1: u64,
                                val2: u64,
                                fun: fn(u64, u64) -> u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.main_memory.write(ADDRESS_BASE_ATOMICS, val1).unwrap();
                }))
                .set_instructions(&[
                    I::new_li(NZ::x1, ADDRESS_BASE_ATOMICS as i64, InstrWidth::Compressed),
                    I::new_li(NZ::x2, val2 as i64, InstrWidth::Compressed),
                    constructor(x3, x1, x2, false, false, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                .set_expected_steps(4)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.hart.xregisters.read(x3);
                    assert_eq!(value, val1);

                    let res: u64 = core.main_memory.read(ADDRESS_BASE_ATOMICS).unwrap();
                    let expected = fun(val1, val2);
                    assert_eq!(res, expected, "Found {value:x}, expected {expected:x}");
                }))
                .build()
        };

        let invalid_x64_atomic = |constructor: ConstructAtomicFn,
                                  val1: u64,
                                  val2: u64,
                                  fun: fn(u64, u64) -> u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.main_memory
                        .write(ADDRESS_BASE_ATOMICS + 4, val1)
                        .unwrap();
                }))
                .set_instructions(&[
                    I::new_li(
                        NZ::x1,
                        (ADDRESS_BASE_ATOMICS + 4) as i64,
                        InstrWidth::Compressed,
                    ),
                    I::new_li(NZ::x2, val2 as i64, InstrWidth::Compressed),
                    constructor(x3, x1, x2, false, false, InstrWidth::Uncompressed),
                    I::new_nop(InstrWidth::Compressed),
                ])
                .set_expected_steps(3)
                .set_assert_hook(assert_hook!(core, F, {
                    let value: u64 = core.hart.xregisters.read(x3);
                    assert_eq!(value, 0);

                    let res: u64 = core.main_memory.read(ADDRESS_BASE_ATOMICS + 4).unwrap();
                    let expected = fun(val1, val2);
                    assert_eq!(res, val1, "Found {value:x}, expected {expected:x}");
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            valid_x64_atomic(I::new_x64_atomic_add, 10, 30, u64::wrapping_add),
            invalid_x64_atomic(I::new_x64_atomic_add, 10, 30, u64::wrapping_add),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });

    backend_test!(test_mul_high, F, {
        use crate::machine_state::registers::NonZeroXRegister::*;

        let test_mul_high = |constructor: fn(
            NonZeroXRegister,
            NonZeroXRegister,
            NonZeroXRegister,
            InstrWidth,
        ) -> I,
                             lhs_reg: NonZeroXRegister,
                             lhs_val: u64,
                             rhs_reg: NonZeroXRegister,
                             rhs_val: u64,
                             expected: u64|
         -> Scenario<F> {
            ScenarioBuilder::default()
                .set_setup_hook(setup_hook!(core, F, {
                    core.hart.xregisters.write_nz(lhs_reg, lhs_val);
                    core.hart.xregisters.write_nz(rhs_reg, rhs_val);
                }))
                .set_instructions(&[constructor(x2, lhs_reg, rhs_reg, Compressed)])
                .set_assert_hook(assert_hook!(core, F, {
                    assert_eq!(core.hart.xregisters.read_nz(x2), expected);
                }))
                .build()
        };

        let scenarios: &[Scenario<F>] = &[
            // MULH (Signed × Signed)
            test_mul_high(
                I::new_x64_mul_high_signed,
                x1,
                i64::MAX as u64,
                x3,
                i64::MAX as u64,
                (((i64::MAX as i128) * (i64::MAX as i128)) >> 64) as u64,
            ),
            test_mul_high(
                I::new_x64_mul_high_signed,
                x1,
                i64::MIN as u64,
                x3,
                i64::MIN as u64,
                (((i64::MIN as i128) * (i64::MIN as i128)) >> 64) as u64,
            ),
            test_mul_high(
                I::new_x64_mul_high_signed,
                x1,
                i64::MIN as u64,
                x3,
                i64::MAX as u64,
                (((i64::MIN as i128) * (i64::MAX as i128)) >> 64) as u64,
            ),
            // MULHSU (Signed × Unsigned)
            test_mul_high(
                I::new_x64_mul_high_signed_unsigned,
                x1,
                i64::MAX as u64,
                x3,
                u64::MAX,
                (((i64::MAX as i128) * (u64::MAX as u128) as i128) >> 64) as u64,
            ),
            test_mul_high(
                I::new_x64_mul_high_signed_unsigned,
                x1,
                i64::MIN as u64,
                x3,
                u64::MAX,
                (((i64::MIN as i128) * (u64::MAX as u128) as i128) >> 64) as u64,
            ),
            test_mul_high(
                I::new_x64_mul_high_signed_unsigned,
                x1,
                -1i64 as u64,
                x3,
                u64::MAX,
                (-((u64::MAX as u128) as i128) >> 64) as u64,
            ),
            // MULHU (Unsigned × Unsigned)
            test_mul_high(
                I::new_x64_mul_high_unsigned,
                x1,
                u64::MAX,
                x3,
                u64::MAX,
                (((u64::MAX as u128) * (u64::MAX as u128)) >> 64) as u64,
            ),
            test_mul_high(
                I::new_x64_mul_high_unsigned,
                x1,
                i64::MIN as u64,
                x3,
                2u64,
                (((i64::MIN as u64 as u128) * (2u128)) >> 64) as u64,
            ),
            test_mul_high(I::new_x64_mul_high_unsigned, x1, 0u64, x3, u64::MAX, 0u64),
        ];

        let mut jit = JIT::<M4K, F::Manager>::new().unwrap();
        let mut interpreted_bb = InterpretedBlockBuilder;

        for scenario in scenarios {
            scenario.run(&mut jit, &mut interpreted_bb);
        }
    });
}
