// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

use crate::ast::IntoMicheline;
use crate::gas::CompareError;

use super::*;

/// Resource-exhaustion `TcError`s, mirroring the kernel's
/// `classify_tc_error` (etherlink `view.rs`). Every one of these is
/// classified as out-of-gas at the live runtime boundary, so a TZT
/// typecheck-error expectation pinned to the out-of-gas message must
/// accept any of them — not only the bare `TcError::OutOfGas` whose
/// `Display` happens to be that message. Without this the matcher and
/// the kernel would disagree on a (typecheck-time) cost overflow.
fn tc_is_resource_exhaustion(e: &TcError) -> bool {
    matches!(
        e,
        TcError::OutOfGas(_)
            | TcError::CostOverflow(_)
            | TcError::CompareError(CompareError::Cost(_))
    )
}

fn check_error_expectation<'a>(
    ctx: &mut Ctx<'a>,
    err_exp: ErrorExpectation<'a>,
    err: TestError<'a>,
) -> Result<(), TztTestError<'a>> {
    use ErrorExpectation as Ex;
    use TestError as Er;
    use TztTestError::*;
    match (err_exp, err) {
        // Typecheck error expectation with exact error unspecified.
        (Ex::TypecheckerError(None), Er::TypecheckerError(_)) => Ok(()),

        (Ex::TypecheckerError(Some(tc_exp)), Er::TypecheckerError(tc_real))
            if tc_real.to_string() == tc_exp =>
        {
            Ok(())
        }
        // Mirror `classify_tc_error`: any resource-exhaustion `TcError`
        // answers an out-of-gas typecheck expectation, even though only
        // bare `OutOfGas` `Display`s as that message.
        (Ex::TypecheckerError(Some(tc_exp)), Er::TypecheckerError(tc_real))
            if tc_exp == TcError::OutOfGas(crate::gas::OutOfGas).to_string()
                && tc_is_resource_exhaustion(&tc_real) =>
        {
            Ok(())
        }
        (Ex::InterpreterError(i_error), Er::InterpreterError(res_i_error))
            if unify_interpreter_error(ctx, &i_error, &res_i_error) =>
        {
            Ok(())
        }
        (err_exp, err) => Err(ExpectedDifferentError(Box::new((err_exp, err)))),
    }
}

/// Both `TypedValue`s are untyped and the untyped representations are compared to get the result.
/// This is required to avoid the Eq trait of TypedValue. It is relevant primarily for
/// paritaly-applied lambdas, as those are represented quite differently from Micheline internally.
fn compare_typed_values(v1: TypedValue, v2: TypedValue) -> bool {
    let arena = typed_arena::Arena::new();
    let mut gas = Gas::default();
    v1.into_micheline_optimized_legacy(&arena, &mut gas)
        == v2.into_micheline_optimized_legacy(&arena, &mut gas)
}

/// Compare two typed stacks by comparing types and values. Values are compared using
/// `compare_typed_values`.
fn compare_typed_stacks(
    t1: &FailingTypeStack,
    s1: IStack,
    t2: &FailingTypeStack,
    s2: IStack,
) -> bool {
    t1 == t2
        && s1.len() == s2.len()
        && std::iter::zip(s1, s2).all(|(v1, v2)| {
            compare_typed_values(TypedValue::unwrap_rc(v1), TypedValue::unwrap_rc(v2))
        })
}

fn unify_interpreter_error<'a>(
    ctx: &mut Ctx<'a>,
    exp: &InterpreterErrorExpectation<'a>,
    err: &InterpretError,
) -> bool {
    use InterpreterErrorExpectation::*;
    match (exp, err) {
        (FailedWith(value), InterpretError::FailedWith(typ, failed_typed_value)) => {
            // Here we typecheck the untyped value from the expectation using the
            // typed of the failed value we get from the interpreter.
            match typecheck_value(value, ctx, typ) {
                Ok(exp_typed_val) => {
                    compare_typed_values(exp_typed_val, failed_typed_value.clone())
                }
                Err(_) => false,
            }
        }
        (MutezOverflow(_, _), InterpretError::MutezOverflow) => true,
        (Overflow, InterpretError::Overflow) => true,
        // The MIR interpreter currently doesn't distinguish a "general overflow"
        // error from other failures. Treat this expectation as unmet rather
        // than panicking via todo!().
        (GeneralOverflow(_, _), _) => false,
        // Mirror the kernel's `classify_interpret_error` (view.rs): every
        // flavour of resource exhaustion answers an `OutOfGas` expectation
        // — direct budget exhaustion, the nested-`TcError` forms, a
        // cost-arithmetic overflow, and the comparison-cost path. Keeping
        // this in sync with the kernel classifier avoids a TZT conformance
        // test and the live runtime disagreeing on the same failure.
        // `CompareError::Incomparable` is a deterministic type failure and
        // is intentionally excluded.
        (
            OutOfGas,
            InterpretError::OutOfGas
            | InterpretError::CostOverflow(_)
            | InterpretError::CompareError(CompareError::Cost(_))
            | InterpretError::TcError(TcError::OutOfGas(_))
            | InterpretError::TcError(TcError::CostOverflow(_))
            | InterpretError::TcError(TcError::CompareError(CompareError::Cost(_))),
        ) => true,
        (_, _) => false, //Some error that we didn't expect happened.
    }
}

pub fn check_expectation<'a>(
    ctx: &mut Ctx<'a>,
    expected: TestExpectation<'a>,
    real: Result<(FailingTypeStack, IStack<'a>), TestError<'a>>,
) -> Result<(), TztTestError<'a>> {
    use TestExpectation::*;
    use TztTestError::*;
    match (expected, real) {
        (ExpectSuccess(stk_exp), Ok((res_type_stack, result_stack))) => {
            let (exp_stk_types, exp_stk_values): (Vec<Type>, Vec<TypedValue>) =
                stk_exp.into_iter().unzip();

            let expected_type_stack = FailingTypeStack::Ok(TopIsFirst::from(exp_stk_types).0);
            let expected_stack = TopIsFirst::from(exp_stk_values).0;
            // If the run was success, and the expectation is also of success check the expected
            // stack. Stack types and values should match.
            if compare_typed_stacks(
                &res_type_stack,
                result_stack.clone(),
                &expected_type_stack,
                expected_stack.clone(),
            ) {
                Ok(())
            } else {
                Err(StackMismatch(
                    (expected_type_stack, expected_stack),
                    (res_type_stack, result_stack),
                ))
            }
        }
        (ExpectSuccess(_), Err(e)) => {
            // If the run was failed, but the expectation expected
            // a success, fail the test with appropriate error..
            Err(UnexpectedError(e))
        }
        (ExpectError(e), Ok((_, i_stack))) => {
            // If the run was success, but the expectation expected
            // a failure, fail the test.
            Err(UnexpectedSuccess(e, i_stack))
        }
        (ExpectError(err_exp), Err(t_error)) => check_error_expectation(ctx, err_exp, t_error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gas::{CompareError, CostOverflow};
    use InterpreterErrorExpectation::OutOfGas as OutOfGasExp;

    /// Real gas exhaustion still surfaces as the bare `InterpretError::OutOfGas`
    /// (it always comes from the outer `ctx.gas().consume(..)?`, never from a
    /// cost helper), so a TZT test expecting `OutOfGas` keeps matching.
    #[test]
    fn bare_out_of_gas_still_unifies() {
        let mut ctx = Ctx::default();
        assert!(unify_interpreter_error(
            &mut ctx,
            &OutOfGasExp,
            &InterpretError::OutOfGas,
        ));
    }

    /// A cost-arithmetic overflow used to surface as `OutOfGas`
    /// (`as_gas_cost`/`log2i` returned `OutOfGas`); it now surfaces as
    /// `CostOverflow`. The kernel HTTP classifier (`view.rs`) keeps mapping
    /// that to `OutOfGas`, and this TZT matcher must agree — otherwise a TZT
    /// conformance test and the live runtime would classify the same failure
    /// differently. This guards that symmetry.
    #[test]
    fn cost_overflow_unifies_with_out_of_gas() {
        let mut ctx = Ctx::default();
        assert!(unify_interpreter_error(
            &mut ctx,
            &OutOfGasExp,
            &InterpretError::CostOverflow(CostOverflow),
        ));
        assert!(unify_interpreter_error(
            &mut ctx,
            &OutOfGasExp,
            &InterpretError::CompareError(CompareError::Cost(CostOverflow)),
        ));
    }

    /// `CompareError::Incomparable` is a deterministic type failure, not
    /// resource exhaustion, so it must NOT answer an `OutOfGas` expectation
    /// (kept in sync with `view.rs`, which routes it to `BadRequest`).
    #[test]
    fn incomparable_does_not_unify_with_out_of_gas() {
        let mut ctx = Ctx::default();
        assert!(!unify_interpreter_error(
            &mut ctx,
            &OutOfGasExp,
            &InterpretError::CompareError(CompareError::Incomparable),
        ));
    }

    /// tc-side symmetry with the kernel's `classify_tc_error`.
    ///
    /// The interpreter half of this matcher already mirrors
    /// `classify_interpret_error`; `check_error_expectation` now mirrors
    /// `classify_tc_error` too. A typecheck-time cost overflow surfaces as
    /// `TcError::CostOverflow` (Display `"arithmetic overflow in cost
    /// computation"`), which the kernel classifies as out-of-gas. A TZT
    /// typecheck-error expectation pinned to the out-of-gas message must
    /// therefore accept it, exactly as the kernel does — while a genuine
    /// type failure (`CompareError::Incomparable`) must still be rejected.
    #[test]
    fn tc_side_cost_overflow_unifies_with_out_of_gas() {
        let mut ctx = Ctx::default();
        let oog = || {
            ErrorExpectation::TypecheckerError(Some(
                TcError::OutOfGas(crate::gas::OutOfGas).to_string(),
            ))
        };

        // Positive control: a genuine typecheck OOG matches.
        assert!(check_error_expectation(
            &mut ctx,
            oog(),
            TestError::TypecheckerError(TcError::OutOfGas(crate::gas::OutOfGas)),
        )
        .is_ok());

        // Resolved: a cost-overflow (kernel-classified as OutOfGas) is now
        // accepted against the out-of-gas typecheck expectation.
        assert!(check_error_expectation(
            &mut ctx,
            oog(),
            TestError::TypecheckerError(TcError::CostOverflow(CostOverflow)),
        )
        .is_ok());

        // Negative control: `Incomparable` is a deterministic type failure
        // (kernel → BadRequest), so it must NOT answer the OOG expectation.
        assert!(check_error_expectation(
            &mut ctx,
            oog(),
            TestError::TypecheckerError(TcError::CompareError(CompareError::Incomparable)),
        )
        .is_err());
    }
}
