// SPDX-FileCopyrightText: 2024-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod common;

use std::ops::Bound;
use std::time::Instant;

use common::*;
use octez_riscv::machine_state::DefaultCacheLayouts;
use octez_riscv::machine_state::memory::M64M;
use octez_riscv::state_backend::hash;
use octez_riscv::state_backend::proof_backend::proof::serialise_proof;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use rand::Rng;

#[test]
#[ignore]
fn test_jstz_proofs_one_step() {
    test_jstz_proofs(false)
}

#[test]
#[ignore]
fn test_jstz_proofs_full() {
    test_jstz_proofs(true)
}

fn test_jstz_proofs(full: bool) {
    let make_stepper = make_stepper_factory();

    let mut base_stepper = make_stepper();
    let base_result = base_stepper.step_max(Bound::Unbounded);
    assert!(matches!(base_result, StepperStatus::Exited { .. }));

    let steps = base_result.steps();
    let base_hash = base_stepper.hash();

    if full {
        // For each step `s`, the stepper will initially step `s-1` steps, then
        // produce a proof of the `s` step. The minimum step size thus needs to be 1.
        let ladder = dissect_steps(steps, 1);
        run_steps_ladder(&make_stepper, &ladder, Some(base_hash));
    } else {
        // Run a number of steps `s` and produce a proof
        let mut rng = rand::thread_rng();
        let step = [rng.gen_range(1..steps)];
        run_steps_ladder(&make_stepper, &step, None)
    }
}

fn run_steps_ladder<F>(make_stepper: F, ladder: &[usize], expected_hash: Option<hash::Hash>)
where
    F: Fn() -> PvmStepper<'static, M64M, DefaultCacheLayouts>,
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

            eprintln!("> Producing proof ...");
            let start = Instant::now();
            let proof = stepper.produce_proof().unwrap();
            let time = start.elapsed();
            let serialisation: Vec<u8> = serialise_proof(&proof).collect();
            eprintln!(
                "> Proof of size {} KiB produced in {:?}",
                serialisation.len() / 1024,
                time
            );

            eprintln!("> Checking initial proof hash ...");
            assert_eq!(proof.initial_state_hash(), stepper.hash());

            let final_state_hash = proof.final_state_hash();

            eprintln!("> Verifying ...");
            assert!(stepper.verify_proof(proof));

            // Run one final step, which is the step proven by `proof`, and check that its
            // state hash matches the final state hash of `proof`.
            eprintln!("> Running 1 step ...");
            stepper.eval_one();
            steps_done += 1;

            eprintln!("> Checking final proof hash ...");
            assert_eq!(final_state_hash, stepper.hash());
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

    if let Some(hash) = expected_hash {
        assert_eq!(stepper.hash(), hash)
    }
}
