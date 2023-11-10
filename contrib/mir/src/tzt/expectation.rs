/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::*;

fn check_error_expectation(
    ctx: &mut Ctx,
    err_exp: ErrorExpectation,
    err: TestError,
) -> Result<(), TztTestError> {
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
        (err_exp, err) => Err(ExpectedDifferentError(err_exp, err)),
    }
}

fn unify_interpreter_error(
    ctx: &mut Ctx,
    exp: &InterpreterErrorExpectation,
    err: &InterpretError,
) -> bool {
    use InterpreterErrorExpectation::*;
    match (exp, err) {
        (FailedWith(value), InterpretError::FailedWith(typ, failed_typed_value)) => {
            // Here we typecheck the untyped value from the expectation using the
            // typed of the failed value we get from the interpreter.
            match value.clone().typecheck(ctx, typ) {
                Ok(exp_typed_val) => {
                    // Then both `Typedvalue`s are untyped and compared to get the result. Here
                    // untyping is done before comparing so that we are not using the Eq trait of
                    // TypedValue. It is thought to be a bit unsafe to use it generally outside the
                    // context of the interpreter, though here we have full type information for
                    // both values being compared, so it is probably safe to compare typed
                    // representation as well.
                    let arena = typed_arena::Arena::new();
                    typed_value_to_value_optimized(&arena, exp_typed_val)
                        == typed_value_to_value_optimized(&arena, failed_typed_value.clone())
                }
                Err(_) => false,
            }
        }
        (MutezOverflow(_, _), InterpretError::MutezOverflow) => true,
        (GeneralOverflow(_, _), _) => todo!("General overflow is unsupported on interpreter"),
        (_, _) => false, //Some error that we didn't expect happened.
    }
}

pub fn check_expectation(
    ctx: &mut Ctx,
    expected: TestExpectation,
    real: Result<(FailingTypeStack, IStack), TestError>,
) -> Result<(), TztTestError> {
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
            if res_type_stack == expected_type_stack && result_stack == expected_stack {
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
