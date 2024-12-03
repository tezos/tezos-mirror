// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod common;

use common::*;
use octez_riscv::{
    machine_state::{bus::main_memory::M100M, DefaultCacheLayouts},
    state_backend::hash,
    stepper::{pvm::PvmStepper, Stepper, StepperStatus},
};
use std::ops::Bound;

#[test]
#[ignore]
fn test_jstz_proofs() {
    let make_stepper = make_stepper_factory();

    let mut base_stepper = make_stepper();
    let base_result = base_stepper.step_max(Bound::Unbounded);
    assert!(matches!(base_result, StepperStatus::Exited { .. }));

    let steps = base_result.steps();
    let base_hash = base_stepper.hash();

    let ladder = dissect_steps(steps);
    run_steps_ladder(&make_stepper, &ladder, base_hash);
}

fn run_steps_ladder<F>(make_stepper: F, ladder: &[usize], expected_hash: hash::Hash)
where
    F: Fn() -> PvmStepper<'static, M100M, DefaultCacheLayouts>,
{
    let expected_steps = ladder.iter().sum::<usize>();
    let mut stepper = make_stepper();

    let mut steps_done = 0;
    for &steps in ladder {
        eprintln!("> Running {} steps ...", steps);
        let StepperStatus::Running { .. } = stepper.step_max(Bound::Included(steps)) else {
            panic!("Unexpected stepper result")
        };
        steps_done += steps;

        eprintln!(
            "> Done {:.2}%",
            (steps_done as f64 / expected_steps as f64) * 100.0
        );

        let proof = stepper.produce_proof();
        assert!(stepper.verify_proof(proof))
    }

    assert_eq!(stepper.hash(), expected_hash);
}
