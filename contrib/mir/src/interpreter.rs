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
        interpret_one(i, ctx, stack)?;
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

    // helper to reduce boilerplate. Usage:
    // `pop!()` force-pops the top elements from the stack (panics if nothing to
    // pop), returning it
    // `let x = pop!()` is roughly equivalent to `let x = stack.pop().unwrap()`
    // but with a clear error message
    // `pop!(T::Foo)` force-pops the stack and unwraps `T::Foo(x)`, returning
    // `x`
    // `let x = pop!(T::Foo)` is roughly equivalent to `let T::Foo(x) = pop!()
    // else {panic!()}` but with a clear error message
    // `pop!(T::Bar, x, y, z)` force-pops the stack, unwraps `T::Bar(x, y, z)`
    // and binds those to `x, y, z` in the surrounding scope
    // `pop!(T::Bar, x, y, z)` is roughly equivalent to `let T::Bar(x, y, z) =
    // pop!() else {panic!()}` but with a clear error message.
    macro_rules! pop {
        ($($args:tt)*) => {
            crate::irrefutable_match::irrefutable_match!(
              stack.pop().unwrap_or_else(|| unreachable_state());
              $($args)*
            )
        };
    }

    match i {
        I::Add(overload) => match overload {
            overloads::Add::IntInt => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::add_int(o1, o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::add_int(o1, o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Nat(sum));
            }
            overloads::Add::IntNat => {
                let o1 = pop!(V::Int);
                // this potentially overflows on i128, but can't overflow on
                // bigints, unwrap for now
                let o2 = pop!(V::Nat).try_into().unwrap();
                ctx.gas.consume(interpret_cost::add_int(o1, o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::NatInt => {
                // this potentially overflows on i128, but can't overflow on
                // bigints, unwrap for now
                let o1 = pop!(V::Nat).try_into().unwrap();
                let o2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::add_int(o1, o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::MutezMutez => {
                let o1 = pop!(V::Mutez);
                let o2 = pop!(V::Mutez);
                ctx.gas.consume(interpret_cost::ADD_TEZ)?;
                let sum = o1.checked_add(o2).ok_or(InterpretError::MutezOverflow)?;
                stack.push(V::Mutez(sum));
            }
        },
        I::Dip(opt_height, nested) => {
            ctx.gas.consume(interpret_cost::dip(*opt_height)?)?;
            let protected_height: u16 = opt_height.unwrap_or(1);
            let mut protected = stack.split_off(protected_height as usize);
            interpret(nested, ctx, stack)?;
            ctx.gas.consume(interpret_cost::undip(protected_height)?)?;
            stack.append(&mut protected);
        }
        I::Drop(opt_height) => {
            ctx.gas.consume(interpret_cost::drop(*opt_height)?)?;
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            stack.drop_top(drop_height);
        }
        I::Dup(opt_height) => {
            ctx.gas.consume(interpret_cost::dup(*opt_height)?)?;
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            stack.push(stack[dup_height - 1].clone());
        }
        I::Gt => {
            ctx.gas.consume(interpret_cost::GT)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i > 0));
        }
        I::If(nested_t, nested_f) => {
            ctx.gas.consume(interpret_cost::IF)?;
            if pop!(V::Bool) {
                interpret(nested_t, ctx, stack)?;
            } else {
                interpret(nested_f, ctx, stack)?;
            }
        }
        I::IfNone(when_none, when_some) => {
            ctx.gas.consume(interpret_cost::IF_NONE)?;
            match pop!(V::Option) {
                Some(x) => {
                    stack.push(*x);
                    interpret(when_some, ctx, stack)?
                }
                None => interpret(when_none, ctx, stack)?,
            }
        }
        I::Int => {
            let i = pop!(V::Nat);
            ctx.gas.consume(interpret_cost::INT_NAT)?;
            stack.push(V::Int(i.try_into().unwrap()));
        }
        I::Loop(nested) => {
            ctx.gas.consume(interpret_cost::LOOP_ENTER)?;
            loop {
                ctx.gas.consume(interpret_cost::LOOP)?;
                if pop!(V::Bool) {
                    interpret(nested, ctx, stack)?;
                } else {
                    ctx.gas.consume(interpret_cost::LOOP_EXIT)?;
                    break;
                }
            }
        }
        I::Push(v) => {
            ctx.gas.consume(interpret_cost::PUSH)?;
            stack.push(v.clone());
        }
        I::Swap => {
            ctx.gas.consume(interpret_cost::SWAP)?;
            stack.swap(0, 1);
        }
        I::Failwith => {
            let x = pop!();
            return Err(InterpretError::FailedWith(x));
        }
        I::Unit => {
            ctx.gas.consume(interpret_cost::UNIT)?;
            stack.push(V::Unit);
        }
        I::Car => {
            ctx.gas.consume(interpret_cost::CAR)?;
            pop!(V::Pair, l, _r);
            stack.push(*l);
        }
        I::Cdr => {
            ctx.gas.consume(interpret_cost::CDR)?;
            pop!(V::Pair, _l, r);
            stack.push(*r);
        }
        I::Pair => {
            ctx.gas.consume(interpret_cost::PAIR)?;
            let l = pop!();
            let r = pop!();
            stack.push(V::new_pair(l, r));
        }
        I::ISome => {
            ctx.gas.consume(interpret_cost::SOME)?;
            let v = pop!();
            stack.push(V::new_option(Some(v)));
        }
        I::Compare => {
            let l = pop!();
            let r = pop!();
            ctx.gas.consume(interpret_cost::compare(&l, &r)?)?;
            let cmp = l.partial_cmp(&r).expect("comparison failed") as i8;
            stack.push(V::Int(cmp.into()));
        }
        I::Amount => {
            ctx.gas.consume(interpret_cost::AMOUNT)?;
            stack.push(V::Mutez(ctx.amount));
        }
        I::Nil(..) => {
            ctx.gas.consume(interpret_cost::NIL)?;
            stack.push(V::List(vec![]));
        }
        I::Get(overload) => match overload {
            overloads::Get::Map => {
                let key = pop!();
                let map = pop!(V::Map);
                ctx.gas.consume(interpret_cost::map_get(&key, map.len())?)?;
                let result = map.get(&key);
                stack.push(V::new_option(result.cloned()));
            }
        },
        I::Update(overload) => match overload {
            overloads::Update::Map => {
                let key = pop!();
                let opt_new_val = pop!(V::Option);
                let mut map = pop!(V::Map);
                ctx.gas
                    .consume(interpret_cost::map_update(&key, map.len())?)?;
                match opt_new_val {
                    None => map.remove(&key),
                    Some(val) => map.insert(key, *val),
                };
                stack.push(V::Map(map));
            }
        },
        I::Seq(nested) => interpret(nested, ctx, stack)?,
    }
    Ok(())
}

#[cfg(test)]
mod interpreter_tests {
    use std::collections::BTreeMap;

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
        let mut ctx = Ctx {
            amount: 100500,
            ..Ctx::default()
        };
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
        let mut ctx = Ctx::default();
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

    #[test]
    fn push_map() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
            (TypedValue::Int(1), TypedValue::String("foo".to_owned())),
            (TypedValue::Int(2), TypedValue::String("bar".to_owned())),
        ]);
        assert_eq!(
            interpret(
                &vec![Push(TypedValue::Map(map.clone()))],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Map(map)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_map() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
            (TypedValue::Int(1), TypedValue::String("foo".to_owned())),
            (TypedValue::Int(2), TypedValue::String("bar".to_owned())),
        ]);
        let mut stack = stk![TypedValue::Map(map), TypedValue::Int(1)];
        assert_eq!(
            interpret(&vec![Get(overloads::Get::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::new_option(Some(TypedValue::String(
                "foo".to_owned()
            )))]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_get(&TypedValue::Int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_map_none() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
            (TypedValue::Int(1), TypedValue::String("foo".to_owned())),
            (TypedValue::Int(2), TypedValue::String("bar".to_owned())),
        ]);
        let mut stack = stk![TypedValue::Map(map), TypedValue::Int(100500)];
        assert_eq!(
            interpret(&vec![Get(overloads::Get::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Option(None)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_get(&TypedValue::Int(100500), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_insert() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::new();
        let mut stack = stk![
            TypedValue::Map(map),
            TypedValue::new_option(Some(TypedValue::String("foo".to_owned()))),
            TypedValue::Int(1)
        ];
        assert_eq!(
            interpret(&vec![Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::Map(BTreeMap::from([(
                TypedValue::Int(1),
                TypedValue::String("foo".to_owned())
            )])),]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_update(&TypedValue::Int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_update() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([(TypedValue::Int(1), TypedValue::String("bar".to_owned()))]);
        let mut stack = stk![
            TypedValue::Map(map),
            TypedValue::new_option(Some(TypedValue::String("foo".to_owned()))),
            TypedValue::Int(1)
        ];
        assert_eq!(
            interpret(&vec![Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::Map(BTreeMap::from([(
                TypedValue::Int(1),
                TypedValue::String("foo".to_owned())
            )])),]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_update(&TypedValue::Int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_remove() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([(TypedValue::Int(1), TypedValue::String("bar".to_owned()))]);
        let mut stack = stk![
            TypedValue::Map(map),
            TypedValue::new_option(None),
            TypedValue::Int(1)
        ];
        assert_eq!(
            interpret(&vec![Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Map(BTreeMap::new())]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_update(&TypedValue::Int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn seq() {
        let mut stack = stk![TypedValue::Int(1), TypedValue::Nat(2)];
        assert_eq!(
            interpret(
                &vec![
                    Seq(vec![Pair]),
                    Seq(vec![Seq(vec![Car])]),
                    Seq(vec![]),
                    Seq(vec![Seq(vec![Seq(vec![])])]),
                ],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Nat(2)]);
    }

    #[test]
    fn add_int_nat() {
        let mut stack = stk![TypedValue::Nat(123), TypedValue::Int(456)];
        assert_eq!(
            interpret(
                &vec![Add(overloads::Add::IntNat)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Int(579)]);
    }

    #[test]
    fn add_int_nat_2() {
        let mut stack = stk![TypedValue::Nat(123), TypedValue::Int(-456)];
        assert_eq!(
            interpret(
                &vec![Add(overloads::Add::IntNat)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Int(-333)]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = stk![TypedValue::Int(789), TypedValue::Nat(42)];
        assert_eq!(
            interpret(
                &vec![Add(overloads::Add::NatInt)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Int(831)]);
    }

    #[test]
    fn add_nat_int_2() {
        let mut stack = stk![TypedValue::Int(-789), TypedValue::Nat(42)];
        assert_eq!(
            interpret(
                &vec![Add(overloads::Add::NatInt)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Int(-747)]);
    }
}