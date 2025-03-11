// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fs;
use std::io::Write;
use std::ops::Bound;

use octez_riscv::jit::JIT;
use octez_riscv::machine_state::DefaultCacheLayouts;
use octez_riscv::machine_state::block_cache::bcall::Block;
use octez_riscv::machine_state::block_cache::bcall::InlineJit;
use octez_riscv::machine_state::block_cache::bcall::Interpreted;
use octez_riscv::machine_state::block_cache::bcall::InterpretedBlockBuilder;
use octez_riscv::machine_state::memory::M64M;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::state_backend::owned_backend::Owned;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use tezos_smart_rollup_utils::inbox::InboxBuilder;

const GOLDEN_DIR: &str = "tests/expected/jstz";

fn capture_debug_log(mint: &mut goldenfile::Mint) -> PvmHooks<'_> {
    let mut log_capture = mint.new_goldenfile("log").unwrap();
    let hooks = PvmHooks::new(move |c| log_capture.write_all(&[c]).unwrap());
    hooks
}

#[test]
fn test_jstz_regression_interpreted() {
    let block_builder = InterpretedBlockBuilder;
    test_jstz_regression::<Interpreted<M64M, Owned>>(block_builder);
}

#[test]
fn test_jstz_regression_inline_jit() {
    let block_buider = (JIT::<M64M, Owned>::new().unwrap(), InterpretedBlockBuilder);
    test_jstz_regression::<InlineJit<_, _>>(block_buider)
}

fn test_jstz_regression<B: Block<M64M, Owned>>(block_builder: B::BlockBuilder) {
    let mut mint = goldenfile::Mint::new(GOLDEN_DIR);

    let (result, initial_hash, final_hash) = {
        let boot_program = fs::read("../assets/hermit-loader").unwrap();
        let main_program = fs::read("../assets/jstz").unwrap();

        let inbox = {
            let mut inbox = InboxBuilder::new();
            inbox
                .load_from_file("../assets/regression-inbox.json")
                .unwrap();
            inbox.build()
        };

        let hooks = capture_debug_log(&mut mint);

        const ROLLUP_ADDRESS: [u8; 20] = [0; 20];
        const ORIGINATION_LEVEL: u32 = 1;

        let mut stepper = PvmStepper::<'_, M64M, DefaultCacheLayouts, Owned, B>::new(
            &boot_program,
            Some(&main_program),
            inbox,
            hooks,
            ROLLUP_ADDRESS,
            ORIGINATION_LEVEL,
            None,
            block_builder,
        )
        .unwrap();

        let initial_hash = stepper.hash();

        let result = stepper.step_max(Bound::Unbounded);
        let final_hash = stepper.hash();

        (result, initial_hash, final_hash)
    };

    assert!(matches!(result, StepperStatus::Exited { .. }));

    let mut initial_hash_capture = mint.new_goldenfile("state_hash_initial").unwrap();
    writeln!(initial_hash_capture, "{initial_hash:?}").unwrap();

    let mut result_capture = mint.new_goldenfile("result").unwrap();
    writeln!(result_capture, "{result:#?}").unwrap();

    let mut final_hash_capture = mint.new_goldenfile("state_hash_final").unwrap();
    writeln!(final_hash_capture, "{final_hash:?}").unwrap();
}
