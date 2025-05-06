// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::fs;

use octez_riscv::machine_state::CacheLayouts;
use octez_riscv::machine_state::block_cache::block::InterpretedBlockBuilder;
use octez_riscv::machine_state::memory::M64M;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::stepper::pvm::PvmStepper;
use rand::Rng;
use rand::seq::SliceRandom;
use tezos_smart_rollup_utils::inbox::InboxBuilder;

pub fn make_stepper_factory<CL: CacheLayouts>() -> impl Fn() -> PvmStepper<'static, M64M, CL> {
    let program = fs::read("../assets/jstz").unwrap();
    let initrd = None::<Vec<u8>>;

    let mut inbox = InboxBuilder::new();
    inbox
        .load_from_file("../assets/regression-inbox.json")
        .unwrap();
    let inbox = inbox.build();

    let address = [0; 20];

    move || {
        let hooks = PvmHooks::none();
        let block_builder = InterpretedBlockBuilder;

        PvmStepper::<'_, M64M, CL>::new(
            &program,
            initrd.as_deref(),
            inbox.clone(),
            hooks,
            address,
            1,
            None,
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
