// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

use crate::ast::IntoMicheline;

use super::*;

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
    v1.into_micheline_optimized_legacy(&arena) == v2.into_micheline_optimized_legacy(&arena)
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
        && std::iter::zip(s1, s2).all(|(v1, v2)| compare_typed_values(v1, v2))
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
        (GeneralOverflow(_, _), _) => todo!("General overflow is unsupported on interpreter"),
        (OutOfGas(_), InterpretError::OutOfGas(gas::OutOfGas)) => true,
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
