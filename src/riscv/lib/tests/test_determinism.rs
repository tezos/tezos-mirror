// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod common;

use std::ops::Bound;

use common::*;
use octez_riscv::machine_state::DefaultCacheLayouts;
use octez_riscv::machine_state::block_cache::block::InterpretedBlockBuilder;
use octez_riscv::machine_state::memory::M64M;
use octez_riscv::pvm::PvmLayout;
use octez_riscv::state_backend::RefOwnedAlloc;
use octez_riscv::state_backend::hash;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;

#[test]
#[ignore]
fn test_jstz_determinism() {
    let make_stepper = make_stepper_factory();

    let mut base_stepper = make_stepper();
    let base_result = base_stepper.step_max(Bound::Unbounded);
    assert!(matches!(base_result, StepperStatus::Exited { .. }));

    let steps = base_result.steps();
    let base_hash = base_stepper.hash();

    // If we re-do it with identical number of steps, that should work exactly the same.
    let mut stepper = make_stepper();
    let result = stepper.step_max(Bound::Included(steps));
    assert_eq!(base_result, result);
    assert_eq!(base_hash, stepper.hash());

    eprintln!("Final hash is {base_hash}");
    eprintln!("Final result is {base_result:?}");

    let base_refs = base_stepper.struct_ref();

    // Create multiple series of bisections that we will evaluate.
    let ladder = dissect_steps(steps, 0);
    run_steps_ladder(&make_stepper, &ladder, &base_refs, base_hash);
}

fn run_steps_ladder<F>(
    make_stepper: F,
    ladder: &[usize],
    expected_refs: &RefOwnedAlloc<PvmLayout<M64M, DefaultCacheLayouts>>,
    expected_hash: hash::Hash,
) where
    F: Fn() -> PvmStepper<'static, M64M, DefaultCacheLayouts>,
{
    let expected_steps = ladder.iter().sum::<usize>();
    let mut stepper_lhs = make_stepper();
    let mut stepper_rhs = make_stepper();

    assert_eq_struct(&stepper_lhs.struct_ref(), &stepper_rhs.struct_ref());

    let mut steps_done = 0;
    for &steps in ladder {
        eprintln!("> Running {} steps ...", steps);
        let result_lhs = stepper_lhs.step_max(Bound::Included(steps));
        let result_rhs = stepper_rhs.step_max(Bound::Included(steps));
        steps_done += steps;

        eprintln!(
            "> Done {:.2}%",
            (steps_done as f64 / expected_steps as f64) * 100.0
        );

        assert_eq!(result_lhs, result_rhs);
        assert_eq!(
            result_lhs.steps(),
            steps,
            "Expected {} steps to be run, but got {}",
            steps,
            result_lhs.steps()
        );
        assert_eq_struct(&stepper_lhs.struct_ref(), &stepper_rhs.struct_ref());

        let block_builder = InterpretedBlockBuilder;
        stepper_lhs.rebind_via_serde(block_builder);
    }

    assert_eq_struct_wrapper(stepper_lhs.struct_ref(), expected_refs);
    assert_eq!(stepper_lhs.hash(), expected_hash);
    assert_eq!(stepper_rhs.hash(), expected_hash);
}

fn assert_eq_struct<A, B>(lhs: &A, rhs: &B)
where
    A: serde::Serialize + PartialEq<B>,
    B: serde::Serialize,
{
    if lhs != rhs {
        eprintln!("> State mismatch, generating diff ...");

        let (file_lhs, path_lhs) = tempfile::NamedTempFile::new().unwrap().keep().unwrap();
        serde_json::to_writer(file_lhs, lhs).unwrap();
        eprintln!("Lhs is located at {}", path_lhs.display());

        let (file_rhs, path_rhs) = tempfile::NamedTempFile::new().unwrap().keep().unwrap();
        serde_json::to_writer(file_rhs, rhs).unwrap();
        eprintln!("Rhs is located at {}", path_rhs.display());

        eprintln!("Run the following to diff them:");
        eprintln!("jd {} {}", path_lhs.display(), path_rhs.display());

        panic!("Assertion failed: values are different");
    }
}

fn assert_eq_struct_wrapper<'a, 'regions1, 'regions2>(
    refs: RefOwnedAlloc<'regions1, PvmLayout<M64M, DefaultCacheLayouts>>,
    expected: &'a RefOwnedAlloc<'regions2, PvmLayout<M64M, DefaultCacheLayouts>>,
) {
    // SAFETY: Rust does not allow us to compare two references with different lifetimes.
    // Theoretically this should be possible and safe thanks to `PartialEq`. However, Rust's
    // subtyping rules seem to influence trait-implementation selection for our `AllocatedOf<...>`
    // in a way that make it more human-friendly but ultimately ends up selecting `A: PartialEq<A>`
    // (where `A` is our `AllocatedOf`-struct) such that left-hand and right-hand side operands of
    // the `==` operator need to be identical in type. This also means lifetimes are forcibly
    // unified. We can work around this by transmuting the references to the same lifetime. This is
    // safe because lifetimes are not violated as dictated by the interface of this function.
    let refs: RefOwnedAlloc<'regions2, PvmLayout<M64M, DefaultCacheLayouts>> =
        unsafe { std::mem::transmute(refs) };
    assert_eq_struct(&refs, expected);
}
