// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use octez_riscv::{
    machine_state::{
        DefaultCacheLayouts, block_cache::bcall::InterpretedBlockBuilder, main_memory::M64M,
    },
    pvm::PvmHooks,
    stepper::pvm::PvmStepper,
};
use rand::{Rng, seq::SliceRandom};
use std::fs;
use tezos_smart_rollup_utils::inbox::InboxBuilder;

pub fn make_stepper_factory() -> impl Fn() -> PvmStepper<'static, M64M, DefaultCacheLayouts> {
    const BOOT_PROGRAM_PATH: &str = "../assets/hermit-loader";
    let boot_program = fs::read(BOOT_PROGRAM_PATH).unwrap();

    const MAIN_PROGRAM_PATH: &str = "../assets/jstz";
    let main_program = fs::read(MAIN_PROGRAM_PATH).unwrap();

    let mut inbox = InboxBuilder::new();
    inbox
        .load_from_file("../assets/regression-inbox.json")
        .unwrap();
    let inbox = inbox.build();

    let address = [0; 20];

    move || {
        let hooks = PvmHooks::none();
        let block_builder = InterpretedBlockBuilder;

        PvmStepper::<'_, M64M, _>::new(
            &boot_program,
            Some(&main_program),
            inbox.clone(),
            hooks,
            address,
            1,
            block_builder,
        )
        .unwrap()
    }
}

pub fn dissect_steps(mut max_steps: usize, min_step_size: usize) -> Vec<usize> {
    let mut rng = rand::thread_rng();
    let mut steps: Vec<usize> = std::iter::from_fn(|| {
        if max_steps == 0 {
            return None;
        }

        let steps = max_steps.div_euclid(2).max(min_step_size + 1);
        let steps = rng.gen_range(min_step_size..=steps);

        max_steps = max_steps.saturating_sub(steps);

        Some(steps)
    })
    .collect();
    steps.shuffle(&mut rng);
    steps
}
