// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod common;

use common::*;
use octez_riscv::{
    machine_state::{main_memory::M100M, DefaultCacheLayouts},
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

    // For each step `s`, the stepper will initially step `s-1` steps, then
    // produce a proof of the `s` step. The minimum step size thus needs to be 1.
    let ladder = dissect_steps(steps, 1);
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
        // Run one step short of `steps`, then produce a proof of the following step.
        let steps = steps.checked_sub(1).expect("minimum step size is 1");
        eprintln!("> Running {} steps ...", steps);
        let result = stepper.step_max(Bound::Included(steps));
        steps_done += steps;

        if steps_done != expected_steps {
            assert!(matches!(result, StepperStatus::Running { .. }));

            eprintln!("> Producing proof");
            let proof = stepper.produce_proof().unwrap();

            assert_eq!(proof.initial_state_hash(), stepper.hash());

            // Run one final step, which is the step proven by `proof`, and check that its
            // state hash matches the final state hash of `proof`.
            eprintln!("> Running 1 step ...");
            stepper.eval_one();
            steps_done += 1;
            // TODO RV-373 : Proof-generating backend should also compute final state hash
            assert_eq!(proof.final_state_hash(), &stepper.hash());

            assert!(stepper.verify_proof(proof))
        } else {
            // Can't generate a proof on the next step if execution has ended
            assert!(matches!(result, StepperStatus::Exited { .. }));
            assert!(ladder.is_empty())
        };

        eprintln!(
            "> Done {:.2}%",
            (steps_done as f64 / expected_steps as f64) * 100.0
        );
    }

    assert_eq!(stepper.hash(), expected_hash);
}
