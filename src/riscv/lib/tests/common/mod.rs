// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use octez_riscv::{
    machine_state::{bus::main_memory::M100M, DefaultCacheLayouts},
    pvm::PvmHooks,
    stepper::pvm::PvmStepper,
};
use rand::{seq::SliceRandom, Rng};
use std::fs;
use tezos_smart_rollup_utils::inbox::InboxBuilder;

pub fn make_stepper_factory() -> impl Fn() -> PvmStepper<'static, M100M, DefaultCacheLayouts> {
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
        PvmStepper::<'_, M100M, _>::new(
            &boot_program,
            Some(&main_program),
            inbox.clone(),
            hooks,
            address,
            1,
        )
        .unwrap()
    }
}

pub fn dissect_steps(mut max_steps: usize) -> Vec<usize> {
    let mut rng = rand::thread_rng();
    let mut steps: Vec<usize> = std::iter::from_fn(|| {
        if max_steps == 0 {
            return None;
        }

        let steps = max_steps.div_euclid(2).max(1);
        let steps = rng.gen_range(0..=steps);

        max_steps = max_steps.saturating_sub(steps);

        Some(steps)
    })
    .collect();
    steps.shuffle(&mut rng);
    steps
}
