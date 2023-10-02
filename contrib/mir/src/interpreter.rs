/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::context::Ctx;
use crate::gas::{interpret_cost, OutOfGas};
use crate::stack::*;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum InterpretError {
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
    #[error("mutez overflow")]
    MutezOverflow,
    #[error("failed with: {0:?}")]
    FailedWith(TypedValue),
}

#[allow(dead_code)]
pub fn interpret(
    ast: &TypecheckedAST,
    ctx: &mut Ctx,
    stack: &mut IStack,
) -> Result<(), InterpretError> {
    for i in ast {
        interpret_one(&i, ctx, stack)?;
    }
    ctx.gas.consume(interpret_cost::INTERPRET_RET)?;
    Ok(())
}

fn unreachable_state() -> ! {
    // If the typechecking of the program being interpreted was successful and if this is reached
    // during interpreting, then the typechecking should be broken, and needs to be fixed.
    panic!("Unreachable state reached during interpreting, possibly broken typechecking!")
}

fn interpret_one(
    i: &TypecheckedInstruction,
    ctx: &mut Ctx,
    stack: &mut IStack,
) -> Result<(), InterpretError> {
    use Instruction as I;
    use TypedValue as V;

    let gas = &mut ctx.gas;

    match i {
        I::Add(overload) => match overload {
            overloads::Add::IntInt => match stack.as_slice() {
                [.., V::Int(o2), V::Int(o1)] => {
                    gas.consume(interpret_cost::add_int(*o1, *o2)?)?;
                    let sum = *o1 + *o2;
                    stack.pop();
                    stack[0] = V::Int(sum);
                }
                _ => unreachable_state(),
            },
            overloads::Add::NatNat => match stack.as_slice() {
                [.., V::Nat(o2), V::Nat(o1)] => {
                    gas.consume(interpret_cost::add_int(*o1, *o2)?)?;
                    let sum = *o1 + *o2;
                    stack.pop();
                    stack[0] = V::Nat(sum);
                }
                _ => unreachable_state(),
            },
            overloads::Add::MutezMutez => match stack.as_slice() {
                [.., V::Mutez(o2), V::Mutez(o1)] => {
                    gas.consume(interpret_cost::ADD_TEZ)?;
                    let sum = o1.checked_add(*o2).ok_or(InterpretError::MutezOverflow)?;
                    stack.pop();
                    stack[0] = V::Mutez(sum);
                }
                _ => unreachable_state(),
            },
        },
        I::Dip(opt_height, nested) => {
            gas.consume(interpret_cost::dip(*opt_height)?)?;
            let protected_height: u16 = opt_height.unwrap_or(1);
            let mut protected = stack.split_off(protected_height as usize);
            interpret(nested, ctx, stack)?;
            ctx.gas.consume(interpret_cost::undip(protected_height)?)?;
            stack.append(&mut protected);
        }
        I::Drop(opt_height) => {
            gas.consume(interpret_cost::drop(*opt_height)?)?;
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            stack.drop_top(drop_height);
        }
        I::Dup(opt_height) => {
            gas.consume(interpret_cost::dup(*opt_height)?)?;
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            stack.push(stack[dup_height - 1].clone());
        }
        I::Gt => {
            gas.consume(interpret_cost::GT)?;
            match stack.as_slice() {
                [.., V::Int(i)] => {
                    stack[0] = V::Bool(*i > 0);
                }
                _ => unreachable_state(),
            }
        }
        I::If(nested_t, nested_f) => {
            gas.consume(interpret_cost::IF)?;
            if let Some(V::Bool(b)) = stack.pop() {
                if b {
                    interpret(nested_t, ctx, stack)?;
                } else {
                    interpret(nested_f, ctx, stack)?;
                }
            } else {
                unreachable_state();
            }
        }
        I::IfNone(when_none, when_some) => {
            gas.consume(interpret_cost::IF_NONE)?;
            match stack.pop() {
                Some(V::Option(x)) => match x {
                    Some(x) => {
                        stack.push(*x);
                        interpret(when_some, ctx, stack)?
                    }
                    None => interpret(when_none, ctx, stack)?,
                },
                _ => unreachable_state(),
            }
        }
        I::Int => match stack.as_slice() {
            [.., V::Nat(i)] => {
                gas.consume(interpret_cost::INT_NAT)?;
                stack[0] = V::Int(*i as _);
            }
            _ => {
                unreachable_state();
            }
        },
        I::Loop(nested) => {
            gas.consume(interpret_cost::LOOP_ENTER)?;
            loop {
                ctx.gas.consume(interpret_cost::LOOP)?;
                if let Some(V::Bool(b)) = stack.pop() {
                    if b {
                        interpret(nested, ctx, stack)?;
                    } else {
                        ctx.gas.consume(interpret_cost::LOOP_EXIT)?;
                        break;
                    }
                } else {
                    unreachable_state();
                }
            }
        }
        I::Push(v) => {
            gas.consume(interpret_cost::PUSH)?;
            stack.push(v.clone());
        }
        I::Swap => {
            gas.consume(interpret_cost::SWAP)?;
            stack.swap(0, 1);
        }
        I::Failwith => match stack.pop() {
            Some(x) => return Err(InterpretError::FailedWith(x)),
            None => unreachable_state(),
        },
        I::Unit => {
            gas.consume(interpret_cost::UNIT)?;
            stack.push(V::Unit);
        }
        I::Car => {
            gas.consume(interpret_cost::CAR)?;
            match stack.pop() {
                Some(V::Pair(l, _)) => stack.push(*l),
                _ => unreachable_state(),
            }
        }
        I::Cdr => {
            gas.consume(interpret_cost::CDR)?;
            match stack.pop() {
                Some(V::Pair(_, r)) => stack.push(*r),
                _ => unreachable_state(),
            }
        }
        I::Pair => {
            gas.consume(interpret_cost::PAIR)?;
            match (stack.pop(), stack.pop()) {
                (Some(l), Some(r)) => stack.push(V::new_pair(l, r)),
                _ => unreachable_state(),
            }
        }
        I::ISome => {
            gas.consume(interpret_cost::SOME)?;
            match stack.pop() {
                v @ Some(..) => stack.push(V::new_option(v)),
                _ => unreachable_state(),
            }
        }
        I::Compare => match stack.as_slice() {
            [.., r, l] => {
                gas.consume(interpret_cost::compare(l, r)?)?;
                let cmp = l.partial_cmp(&r);
                let val = match cmp {
                    Some(v) => v as i8,
                    _ => unreachable_state(),
                };
                stack.pop();
                stack[0] = V::Int(val.into());
            }
            _ => unreachable_state(),
        },
        I::Amount => {
            gas.consume(interpret_cost::AMOUNT)?;
            stack.push(V::Mutez(ctx.amount));
        }
        I::Nil(..) => {
            gas.consume(interpret_cost::NIL)?;
            stack.push(V::List(vec![]));
        }
    }
    Ok(())
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use crate::gas::Gas;
    use Instruction::*;
    use TypedValue as V;

    #[test]
    fn test_add() {
        let mut stack = stk![V::Nat(10), V::Nat(20)];
        let expected_stack = stk![V::Nat(30)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::NatNat), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_add_mutez() {
        let mut stack = stk![V::Mutez(2i64.pow(62)), V::Mutez(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::MutezMutez), &mut ctx, &mut stack).is_ok());
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 20);
        assert_eq!(stack, stk![V::Mutez(2i64.pow(62) + 20)]);
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![V::Mutez(2i64.pow(62)), V::Mutez(2i64.pow(62))]
            ),
            Err(InterpretError::MutezOverflow)
        );
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![
                    V::Mutez((2u64.pow(63) - 1).try_into().unwrap()),
                    V::Mutez(1)
                ]
            ),
            Err(InterpretError::MutezOverflow)
        );
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![
                    V::Mutez(1),
                    V::Mutez((2u64.pow(63) - 1).try_into().unwrap())
                ]
            ),
            Err(InterpretError::MutezOverflow)
        );
    }

    #[test]
    fn test_dip() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(25), V::Nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Dip(None, vec![Add(overloads::Add::NatNat)]),
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dip2() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(5), V::Nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dip(Some(2), vec![Drop(None)]), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_drop() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(20), V::Nat(5)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Drop(None), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_drop2() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Drop(Some(2)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dup() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(20), V::Nat(5), V::Nat(10), V::Nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dup(None), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dup2() {
        let mut stack = stk![V::Nat(20), V::Nat(5), V::Nat(10)];
        let expected_stack = stk![V::Nat(20), V::Nat(5), V::Nat(10), V::Nat(5)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dup(Some(2)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_gt() {
        let mut stack = stk![V::Int(20), V::Int(10)];
        let expected_stack = stk![V::Int(20), V::Bool(true)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Gt, &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_if_t() {
        let mut stack = stk![V::Int(20), V::Int(5), V::Bool(true)];
        let expected_stack = stk![V::Int(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)]),
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_if_f() {
        let mut stack = stk![V::Int(20), V::Int(5), V::Bool(false)];
        let expected_stack = stk![V::Int(25)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)]),
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_int() {
        let mut stack = stk![V::Nat(20), V::Nat(10)];
        let expected_stack = stk![V::Nat(20), V::Int(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Int, &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_push() {
        let mut stack = stk![V::Nat(20), V::Nat(10)];
        let expected_stack = stk![V::Nat(20), V::Nat(10), V::Nat(0)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Push(V::Nat(0)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_0() {
        let mut stack = stk![V::Nat(20), V::Nat(10), V::Bool(false)];
        let expected_stack = stk![V::Nat(20), V::Nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Loop(vec![
                Push(V::Nat(1)),
                Add(overloads::Add::NatNat),
                Push(V::Bool(false))
            ]),
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_1() {
        let mut stack = stk![V::Nat(20), V::Nat(10), V::Bool(true)];
        let expected_stack = stk![V::Nat(20), V::Nat(11)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Loop(vec![
                Push(V::Nat(1)),
                Add(overloads::Add::NatNat),
                Push(V::Bool(false))
            ]),
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_many() {
        let mut stack = stk![V::Nat(20), V::Int(10), V::Bool(true)];
        let expected_stack = stk![V::Nat(20), V::Int(0)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Loop(vec![
                Push(V::Int(-1)),
                Add(overloads::Add::IntInt),
                Dup(None),
                Gt
            ]),
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_swap() {
        let mut stack = stk![V::Nat(20), V::Int(10)];
        let expected_stack = stk![V::Int(10), V::Nat(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Swap, &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            interpret_one(&Failwith, &mut Ctx::default(), &mut stk![V::Nat(20)]),
            Err(InterpretError::FailedWith(V::Nat(20)))
        );
    }

    #[test]
    fn push_string_value() {
        let mut stack = stk![];
        assert_eq!(
            interpret(
                &vec![Push(V::String("foo".to_owned()))],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::String("foo".to_owned())]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = stk![];
        assert_eq!(
            interpret(&vec![Push(V::Unit)], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(&vec![Unit], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::Unit]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::UNIT - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_pair() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &vec![Push(V::new_pair(
                V::Int(-5),
                V::new_pair(V::Nat(3), V::Bool(false))
            ))],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(
            stack,
            stk![V::new_pair(
                V::Int(-5),
                V::new_pair(V::Nat(3), V::Bool(false))
            )]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_option() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &vec![Push(V::new_option(Some(V::Int(-5))))],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::new_option(Some(V::Int(-5)))]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn car() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &vec![
                Push(V::new_pair(
                    V::Int(-5),
                    V::new_pair(V::Nat(3), V::Bool(false))
                )),
                Car
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::Int(-5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::PUSH
                - interpret_cost::CAR
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn cdr() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &vec![
                Push(V::new_pair(
                    V::new_pair(V::Nat(3), V::Bool(false)),
                    V::Int(-5),
                )),
                Cdr
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::Int(-5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::PUSH
                - interpret_cost::CDR
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn pair() {
        let mut stack = stk![V::Nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&vec![Pair], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_pair(V::Bool(false), V::Nat(42))]);
    }

    #[test]
    fn pair_car() {
        let mut stack = stk![V::Nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&vec![Pair, Car], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::Bool(false)]);
    }

    #[test]
    fn pair_cdr() {
        let mut stack = stk![V::Nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&vec![Pair, Cdr], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::Nat(42)]);
    }

    #[test]
    fn if_none_1() {
        let code = vec![IfNone(vec![Push(TypedValue::Int(5))], vec![])];
        // with Some
        let mut stack = stk![V::new_option(Some(V::Int(42)))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Int(42)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::IF_NONE - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_none_2() {
        let code = vec![IfNone(vec![Push(TypedValue::Int(5))], vec![])];
        // with None
        let mut stack = stk![V::new_option(None)];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Int(5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::IF_NONE
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn some() {
        let mut stack = stk![V::Int(5)];
        let mut ctx = Ctx::default();
        assert!(interpret(&vec![ISome], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_option(Some(V::Int(5)))]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::SOME - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn compare() {
        macro_rules! test {
            ($expr:tt, $res:tt) => {
                let mut stack = stk!$expr;
                let expected_cost = interpret_cost::compare(&stack[0], &stack[1]).unwrap()
                    + interpret_cost::INTERPRET_RET;
                let mut ctx = Ctx::default();
                assert!(interpret(&vec![Compare], &mut ctx, &mut stack).is_ok());
                assert_eq!(stack, stk!$res);
                assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - expected_cost);
            };
        }
        test!([V::Int(5), V::Int(6)], [V::Int(1)]);
        test!([V::Int(5), V::Int(5)], [V::Int(0)]);
        test!([V::Int(6), V::Int(5)], [V::Int(-1)]);
        test!([V::Bool(true), V::Bool(false)], [V::Int(-1)]);
        test!([V::Bool(true), V::Bool(true)], [V::Int(0)]);
        test!([V::Bool(false), V::Bool(true)], [V::Int(1)]);
        test!([V::Bool(false), V::Bool(false)], [V::Int(0)]);
        test!(
            [V::String("foo".to_owned()), V::String("bar".to_owned())],
            [V::Int(-1)]
        );
        test!([V::Unit, V::Unit], [V::Int(0)]);
        test!(
            [V::new_option(Some(V::Int(5))), V::Option(None)],
            [V::Int(-1)]
        );
    }

    #[test]
    fn amount() {
        let mut stack = stk![];
        let mut ctx = &mut Ctx::default();
        ctx.amount = 100500;
        assert_eq!(interpret(&vec![Amount], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![TypedValue::Mutez(100500)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::INTERPRET_RET - interpret_cost::AMOUNT,
        )
    }

    #[test]
    fn push_int_list() {
        let mut stack = stk![];
        let mut ctx = &mut Ctx::default();
        assert_eq!(
            interpret(
                &vec![Push(TypedValue::List(vec![
                    TypedValue::Int(1),
                    TypedValue::Int(2),
                    TypedValue::Int(3),
                ]))],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::List(vec![
                TypedValue::Int(1),
                TypedValue::Int(2),
                TypedValue::Int(3),
            ])]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn nil() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&vec![Nil(())], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![TypedValue::List(vec![])]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::NIL - interpret_cost::INTERPRET_RET,
        )
    }
}
