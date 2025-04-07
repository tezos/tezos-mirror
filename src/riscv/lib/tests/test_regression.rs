// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use octez_riscv::{
    machine_state::{
        DefaultCacheLayouts, block_cache::bcall::InterpretedBlockBuilder, main_memory::M64M,
    },
    pvm::PvmHooks,
    state_backend::owned_backend::Owned,
    stepper::{Stepper, StepperStatus, pvm::PvmStepper},
};
use std::{fs, io::Write, ops::Bound};
use tezos_smart_rollup_utils::inbox::InboxBuilder;

const GOLDEN_DIR: &str = "tests/expected/jstz";

fn capture_debug_log(mint: &mut goldenfile::Mint) -> PvmHooks<'_> {
    let mut log_capture = mint.new_goldenfile("log").unwrap();
    let hooks = PvmHooks::new(move |c| log_capture.write_all(&[c]).unwrap());
    hooks
}

#[test]
fn test_jstz_regression() {
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

        let block_builder = InterpretedBlockBuilder;

        let mut stepper = PvmStepper::<'_, M64M, DefaultCacheLayouts, Owned>::new(
            &boot_program,
            Some(&main_program),
            inbox,
            hooks,
            ROLLUP_ADDRESS,
            ORIGINATION_LEVEL,
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
