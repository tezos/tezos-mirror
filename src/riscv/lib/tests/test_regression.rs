// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fs;
use std::io::Write;
use std::ops::Bound;
use std::path::Path;
use std::path::PathBuf;

use octez_riscv::machine_state::DefaultCacheLayouts;
use octez_riscv::machine_state::block_cache::block::Block;
use octez_riscv::machine_state::block_cache::block::Interpreted;
use octez_riscv::machine_state::block_cache::block::InterpretedBlockBuilder;
use octez_riscv::machine_state::block_cache::block::Jitted;
use octez_riscv::machine_state::block_cache::block::OutlineCompiler;
use octez_riscv::machine_state::memory::M64M;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::state_backend::owned_backend::Owned;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use tezos_smart_rollup_utils::inbox::InboxBuilder;

fn capture_debug_log(mint: &mut goldenfile::Mint) -> PvmHooks<'_> {
    let mut log_capture = mint.new_goldenfile("log").unwrap();
    let hooks = PvmHooks::new(move |c| log_capture.write_all(&[c]).unwrap());
    hooks
}

#[test]
fn regression_frozen_jstz() {
    test_regression(
        "tests/expected/jstz",
        "../assets/jstz",
        "../assets/regression-inbox.json",
        true,
    )
}

#[test]
fn regression_frozen_dummy_kernel() {
    test_regression(
        "tests/expected/dummy",
        "../assets/riscv-dummy.elf",
        "../assets/dummy-kernel-inbox.json",
        true,
    )
}

#[test]
fn regression_dummy_kernel() {
    test_regression(
        "tests/expected/dummy-volatile",
        "../riscv-dummy.elf",
        "../assets/dummy-kernel-inbox.json",
        false,
    )
}

fn test_regression(
    golden_dir: impl AsRef<Path>,
    kernel_path: impl AsRef<Path>,
    inbox_path: impl AsRef<Path>,
    capture_volatile_properties: bool,
) {
    test_regression_for_block::<Interpreted<M64M, Owned>>(
        InterpretedBlockBuilder,
        &golden_dir,
        &kernel_path,
        &inbox_path,
        capture_volatile_properties,
    );

    // This needs to run *after* the previous *interpreted* test. Otherwise, we run into trouble when
    // checking and updating the golden files.
    test_regression_for_block::<Jitted<_, _, _>>(
        (
            OutlineCompiler::<M64M, Owned>::default(),
            InterpretedBlockBuilder,
        ),
        &golden_dir,
        &kernel_path,
        &inbox_path,
        capture_volatile_properties,
    );
}

fn test_regression_for_block<B: Block<M64M, Owned>>(
    block_builder: B::BlockBuilder,
    golden_dir: impl AsRef<Path>,
    kernel_path: impl AsRef<Path>,
    inbox_path: impl AsRef<Path>,
    capture_volatile_properties: bool,
) {
    let mut mint = goldenfile::Mint::new(golden_dir);

    let (result, initial_hash, final_hash) = {
        // We need to read the kernel in any case
        let program = fs::read(kernel_path)
            .expect("Failed to read kernel from disk. Try running `make build`.");
        let initrd = None::<Vec<u8>>;

        let inbox = {
            let mut inbox = InboxBuilder::new();
            inbox.load_from_file(inbox_path).unwrap();
            inbox.build()
        };

        let hooks = capture_debug_log(&mut mint);

        const ROLLUP_ADDRESS: [u8; 20] = [
            244, 228, 124, 179, 196, 58, 104, 176, 212, 142, 48, 148, 9, 44, 164, 45, 113, 58, 221,
            181,
        ];
        const ORIGINATION_LEVEL: u32 = 1;

        let mut stepper = PvmStepper::<'_, M64M, DefaultCacheLayouts, Owned, B>::new(
            &program,
            initrd.as_deref(),
            inbox,
            hooks,
            ROLLUP_ADDRESS,
            ORIGINATION_LEVEL,
            Some(PathBuf::from("../assets/preimages").into_boxed_path()),
            block_builder,
        )
        .unwrap();

        let initial_hash = stepper.hash();

        let result = stepper.step_max(Bound::Unbounded);
        let final_hash = stepper.hash();

        (result, initial_hash, final_hash)
    };

    assert!(
        matches!(result, StepperStatus::Exited { .. }),
        "Unexpected result: {result:?}"
    );

    if capture_volatile_properties {
        let mut initial_hash_capture = mint.new_goldenfile("state_hash_initial").unwrap();
        writeln!(initial_hash_capture, "{initial_hash:?}").unwrap();

        let mut result_capture = mint.new_goldenfile("result").unwrap();
        writeln!(result_capture, "{result:#?}").unwrap();

        let mut final_hash_capture = mint.new_goldenfile("state_hash_final").unwrap();
        writeln!(final_hash_capture, "{final_hash:?}").unwrap();
    }
}
