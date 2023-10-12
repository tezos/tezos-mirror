/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::num::TryFromIntError;

use crate::ast::*;
use crate::gas;
use crate::gas::{Gas, OutOfGas};
use crate::stack::*;

/// Typechecker error type.
#[derive(Debug, PartialEq, Eq)]
pub enum TcError {
    GenericTcError,
    StackTooShort,
    StacksNotEqual,
    OutOfGas,
    FailNotInTail,
}

impl From<OutOfGas> for TcError {
    fn from(_: OutOfGas) -> Self {
        TcError::OutOfGas
    }
}

impl From<TryFromIntError> for TcError {
    fn from(_: TryFromIntError) -> Self {
        TcError::GenericTcError
    }
}

#[allow(dead_code)]
pub fn typecheck(
    ast: ParsedAST,
    gas: &mut Gas,
    stack: &mut TypeStack,
) -> Result<TypecheckedAST, TcError> {
    ast.into_iter()
        .map(|i| typecheck_instruction(i, gas, stack))
        .collect()
}

fn typecheck_instruction(
    i: ParsedInstruction,
    gas: &mut Gas,
    stack: &mut TypeStack,
) -> Result<TypecheckedInstruction, TcError> {
    use Instruction as I;
    use Type as T;

    if stack.is_failed() {
        return Err(TcError::FailNotInTail);
    }

    gas.consume(gas::tc_cost::INSTR_STEP)?;

    Ok(match i {
        I::Add(..) => match stack.as_slice() {
            [.., T::Nat, T::Nat] => {
                stack.pop();
                I::Add(overloads::Add::NatNat)
            }
            [.., T::Int, T::Int] => {
                stack.pop();
                I::Add(overloads::Add::IntInt)
            }
            [.., T::Mutez, T::Mutez] => {
                stack.pop();
                I::Add(overloads::Add::MutezMutez)
            }
            _ => unimplemented!(),
        },
        I::Dip(opt_height, nested) => {
            let protected_height = opt_height.unwrap_or(1) as usize;

            gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;

            ensure_stack_len(stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, gas, stack)?;
            if stack.is_failed() {
                return Err(TcError::FailNotInTail);
            }
            stack.append(&mut protected);
            I::Dip(opt_height, nested)
        }
        I::Drop(opt_height) => {
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            gas.consume(gas::tc_cost::drop_n(&opt_height)?)?;
            ensure_stack_len(&stack, drop_height)?;
            stack.drop_top(drop_height);
            I::Drop(opt_height)
        }
        I::Dup(Some(0)) => {
            // DUP instruction requires an argument that is > 0.
            return Err(TcError::GenericTcError);
        }
        I::Dup(opt_height) => {
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            ensure_stack_len(stack, dup_height)?;
            stack.push(stack[dup_height - 1].clone());
            I::Dup(opt_height)
        }
        I::Gt => match stack.pop() {
            Some(T::Int) => {
                stack.push(T::Bool);
                I::Gt
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::If(nested_t, nested_f) => match stack.pop() {
            // Check if top is bool
            Some(T::Bool) => {
                // Clone the stack so that we have a copy to run one branch on.
                // We can run the other branch on the live stack.
                let mut t_stack: TypeStack = stack.clone();
                let nested_t = typecheck(nested_t, gas, &mut t_stack)?;
                let nested_f = typecheck(nested_f, gas, stack)?;
                // If both stacks are same after typecheck, all is good.
                ensure_stacks_eq(gas, &t_stack, &stack)?;
                // Replace stack with other branch's stack if it's failed, as
                // one branch might've been successful.
                if stack.is_failed() {
                    *stack = t_stack;
                }
                I::If(nested_t, nested_f)
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::IfNone(when_none, when_some) => match stack.pop() {
            // Check if top is option 'ty
            Some(T::Option(ty)) => {
                // Clone the some_stack as we need to push a type on top of it
                let mut some_stack: TypeStack = stack.clone();
                some_stack.push(*ty);
                let when_none = typecheck(when_none, gas, stack)?;
                let when_some = typecheck(when_some, gas, &mut some_stack)?;
                // If both stacks are same after typecheck, all is good.
                ensure_stacks_eq(gas, &some_stack, &stack)?;
                // Replace stack with other branche's stack if it's failed, as
                // one branch might've been successful.
                if stack.is_failed() {
                    *stack = some_stack;
                }
                I::IfNone(when_none, when_some)
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::Int => match stack.pop() {
            Some(T::Nat) => {
                stack.push(Type::Int);
                I::Int
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::Loop(nested) => match stack.as_slice() {
            // Check if top is bool and bind the tail to `t`.
            [t @ .., T::Bool] => {
                let mut live: TypeStack = TopIsLast::from(t).0;
                // Clone the tail and typecheck the nested body using it.
                let nested = typecheck(nested, gas, &mut live)?;
                // If the starting stack and result stack match
                // then the typecheck is complete. pop the bool
                // off the original stack to form the final result.
                ensure_stacks_eq(gas, &live, &stack)?;
                stack.pop();
                I::Loop(nested)
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::Push((t, v)) => {
            let v = typecheck_value(gas, &t, v)?;
            stack.push(t.to_owned());
            I::Push(v)
        }
        I::Swap => {
            ensure_stack_len(stack, 2)?;
            stack.swap(0, 1);
            I::Swap
        }
        I::Failwith => {
            ensure_stack_len(stack, 1)?;
            stack.pop();
            stack.fail();
            I::Failwith
        }
        I::Unit => {
            stack.push(T::Unit);
            I::Unit
        }
        I::Car => {
            match stack.pop() {
                Some(T::Pair(l, _)) => stack.push(*l),
                _ => return Err(TcError::GenericTcError),
            }
            I::Car
        }
        I::Cdr => {
            match stack.pop() {
                Some(T::Pair(_, r)) => stack.push(*r),
                _ => return Err(TcError::GenericTcError),
            }
            I::Cdr
        }
        I::Pair => {
            ensure_stack_len(stack, 2)?;
            match (stack.pop(), stack.pop()) {
                (Some(l), Some(r)) => {
                    stack.push(Type::new_pair(l, r));
                    I::Pair
                }
                _ => return Err(TcError::GenericTcError),
            }
        }
        I::ISome => match stack.pop() {
            Some(ty) => {
                stack.push(T::new_option(ty));
                I::ISome
            }
            _ => return Err(TcError::StackTooShort),
        },
    })
}

fn typecheck_value(gas: &mut Gas, t: &Type, v: Value) -> Result<TypedValue, TcError> {
    use Type::*;
    use TypedValue as TV;
    use Value::*;
    gas.consume(gas::tc_cost::VALUE_STEP)?;
    Ok(match (t, v) {
        (Nat, NumberValue(n)) => TV::Nat(n.try_into()?),
        (Int, NumberValue(n)) => TV::Int(n),
        (Bool, BooleanValue(b)) => TV::Bool(b),
        (Mutez, NumberValue(n)) if n >= 0 => TV::Mutez(n.try_into()?),
        (String, StringValue(s)) => TV::String(s),
        (Unit, UnitValue) => TV::Unit,
        (Pair(tl, tr), PairValue(vl, vr)) => {
            let l = typecheck_value(gas, tl, *vl)?;
            let r = typecheck_value(gas, tr, *vr)?;
            TV::new_pair(l, r)
        }
        (Option(ty), OptionValue(v)) => match v {
            Some(v) => {
                let v = typecheck_value(gas, ty, *v)?;
                TV::new_option(Some(v))
            }
            None => TV::new_option(None),
        },
        _ => return Err(TcError::GenericTcError),
    })
}

/// Ensures type stack is at least of the required length, otherwise returns
/// `Err(StackTooShort)`.
fn ensure_stack_len(stack: &TypeStack, l: usize) -> Result<(), TcError> {
    if stack.len() >= l {
        Ok(())
    } else {
        Err(TcError::StackTooShort)
    }
}

/// Ensures two type stacks compare equal, otherwise returns
/// `Err(StacksNotEqual)`. If runs out of gas, returns `Err(OutOfGas)` instead.
///
/// Failed stacks compare equal with anything.
fn ensure_stacks_eq(gas: &mut Gas, stack1: &TypeStack, stack2: &TypeStack) -> Result<(), TcError> {
    if stack1.is_failed() || stack2.is_failed() {
        return Ok(());
    }
    if stack1.len() != stack2.len() {
        return Err(TcError::StacksNotEqual);
    }
    for (ty1, ty2) in stack1.iter().zip(stack2.iter()) {
        ensure_ty_eq(gas, ty1, ty2)?;
    }
    Ok(())
}

fn ensure_ty_eq(gas: &mut Gas, ty1: &Type, ty2: &Type) -> Result<(), TcError> {
    gas.consume(gas::tc_cost::ty_eq(ty1.size_for_gas(), ty2.size_for_gas())?)?;
    if ty1 != ty2 {
        Err(TcError::StacksNotEqual)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod typecheck_tests {
    use crate::parser::*;
    use crate::typechecker::*;
    use Instruction::*;

    #[test]
    fn test_dup() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![Type::Nat, Type::Nat];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Dup(Some(1)), &mut gas, &mut stack),
            Ok(Dup(Some(1)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_dup_n() {
        let mut stack = stk![Type::Int, Type::Nat];
        let expected_stack = stk![Type::Int, Type::Nat, Type::Int];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Dup(Some(2)), &mut gas, &mut stack),
            Ok(Dup(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_swap() {
        let mut stack = stk![Type::Nat, Type::Int];
        let expected_stack = stk![Type::Int, Type::Nat];
        let mut gas = Gas::new(10000);
        assert_eq!(typecheck_instruction(Swap, &mut gas, &mut stack), Ok(Swap));
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_int() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        assert_eq!(typecheck_instruction(Int, &mut gas, &mut stack), Ok(Int));
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_drop() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck(vec![Drop(None)], &mut gas, &mut stack),
            Ok(vec![Drop(None)])
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_drop_n() {
        let mut stack = stk![Type::Nat, Type::Int];
        let expected_stack = stk![];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Drop(Some(2)), &mut gas, &mut stack),
            Ok(Drop(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440 - 2 * 50);
    }

    #[test]
    fn test_push() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![Type::Nat, Type::Int];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(
                Push((Type::Int, Value::NumberValue(1))),
                &mut gas,
                &mut stack
            ),
            Ok(Push(TypedValue::Int(1)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440 - 100);
    }

    #[test]
    fn test_gt() {
        let mut stack = stk![Type::Int];
        let expected_stack = stk![Type::Bool];
        let mut gas = Gas::new(10000);
        assert_eq!(typecheck_instruction(Gt, &mut gas, &mut stack), Ok(Gt));
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_dip() {
        let mut stack = stk![Type::Int, Type::Bool];
        let expected_stack = stk![Type::Int, Type::Nat, Type::Bool];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(
                Dip(Some(1), parse("{PUSH nat 6}").unwrap()),
                &mut gas,
                &mut stack,
            ),
            Ok(Dip(Some(1), vec![Push(TypedValue::Nat(6))]))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440 - 440 - 100 - 50);
    }

    #[test]
    fn test_add_int_int() {
        let mut stack = stk![Type::Int, Type::Int];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Add(()), &mut gas, &mut stack),
            Ok(Add(overloads::Add::IntInt))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_add_nat_nat() {
        let mut stack = stk![Type::Nat, Type::Nat];
        let expected_stack = stk![Type::Nat];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Add(()), &mut gas, &mut stack),
            Ok(Add(overloads::Add::NatNat))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_add_mutez_mutez() {
        let mut stack = stk![Type::Mutez, Type::Mutez];
        let expected_stack = stk![Type::Mutez];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(Add(()), &mut gas, &mut stack),
            Ok(Add(overloads::Add::MutezMutez))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440);
    }

    #[test]
    fn test_loop() {
        let mut stack = stk![Type::Int, Type::Bool];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(
                Loop(parse("{PUSH bool True}").unwrap()),
                &mut gas,
                &mut stack
            ),
            Ok(Loop(vec![Push(TypedValue::Bool(true))]))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(gas.milligas(), 10000 - 440 - 440 - 100 - 60 * 2);
    }

    #[test]
    fn test_loop_stacks_not_equal_length() {
        let mut stack = stk![Type::Int, Type::Bool];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(
                Loop(parse("{PUSH int 1; PUSH bool True}").unwrap()),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual
        );
    }

    #[test]
    fn test_loop_stacks_not_equal_types() {
        let mut stack = stk![Type::Int, Type::Bool];
        let mut gas = Gas::new(10000);
        assert_eq!(
            typecheck_instruction(
                Loop(parse("{DROP; PUSH bool False; PUSH bool True}").unwrap()),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual
        );
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            typecheck_instruction(Failwith, &mut Gas::default(), &mut stk![Type::Int]),
            Ok(Failwith)
        );
    }

    #[test]
    fn test_failed_stacks() {
        macro_rules! test_fail {
            ($code:expr) => {
                assert_eq!(
                    typecheck(parse($code).unwrap(), &mut Gas::default(), &mut stk![]),
                    Err(TcError::FailNotInTail)
                );
            };
        }
        test_fail!("{ PUSH int 1; FAILWITH; PUSH int 1 }");
        test_fail!("{ PUSH int 1; DIP { PUSH int 1; FAILWITH } }");
        test_fail!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1; FAILWITH }; GT }");
        macro_rules! test_ok {
            ($code:expr) => {
                assert!(typecheck(parse($code).unwrap(), &mut Gas::default(), &mut stk![]).is_ok());
            };
        }
        test_ok!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1 }; GT }");
        test_ok!("{ PUSH bool True; IF { PUSH int 1 } { PUSH int 1; FAILWITH }; GT }");
        test_ok!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1; FAILWITH } }");
        test_ok!("{ PUSH bool True; LOOP { PUSH int 1; FAILWITH }; PUSH int 1 }");
    }

    #[test]
    fn string_values() {
        assert_eq!(
            typecheck_value(
                &mut Gas::default(),
                &Type::String,
                Value::StringValue("foo".to_owned())
            ),
            Ok(TypedValue::String("foo".to_owned()))
        )
    }

    #[test]
    fn push_string_value() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse(r#"{ PUSH string "foo"; }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::String("foo".to_owned()))])
        );
        assert_eq!(stack, stk![Type::String]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH unit Unit; }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::Unit)])
        );
        assert_eq!(stack, stk![Type::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(parse("{ UNIT }").unwrap(), &mut Gas::default(), &mut stack),
            Ok(vec![Unit])
        );
        assert_eq!(stack, stk![Type::Unit]);
    }

    #[test]
    fn push_pair_value() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False) }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::new_pair(
                TypedValue::Int(-5),
                TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
            ))])
        );
        assert_eq!(
            stack,
            stk![Type::new_pair(
                Type::Int,
                Type::new_pair(Type::Nat, Type::Bool)
            )]
        );
    }

    #[test]
    fn push_option_value() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (option nat) (Some 3) }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::new_option(Some(TypedValue::Nat(3))))])
        );
        assert_eq!(stack, stk![Type::new_option(Type::Nat)]);
    }

    #[test]
    fn car() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CAR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
                )),
                Car
            ])
        );
        assert_eq!(stack, stk![Type::Int]);
    }

    #[test]
    fn cdr() {
        let mut stack = stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CDR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
                )),
                Cdr
            ])
        );
        assert_eq!(stack, stk![Type::new_pair(Type::Nat, Type::Bool)]);
    }

    #[test]
    fn pair() {
        let mut stack = stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(parse("{ PAIR }").unwrap(), &mut Gas::default(), &mut stack),
            Ok(vec![Pair])
        );
        assert_eq!(stack, stk![Type::new_pair(Type::Nat, Type::Int)]);
    }

    #[test]
    fn pair_car() {
        let mut stack = stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(
                parse("{ PAIR; CAR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Pair, Car])
        );
        assert_eq!(stack, stk![Type::Nat]);
    }

    #[test]
    fn pair_cdr() {
        let mut stack = stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(
                parse("{ PAIR; CDR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![Pair, Cdr])
        );
        assert_eq!(stack, stk![Type::Int]);
    }

    #[test]
    fn if_none() {
        let mut stack = stk![Type::new_option(Type::Int)];
        assert_eq!(
            typecheck(
                parse("{ IF_NONE { PUSH int 5; } {} }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(vec![IfNone(vec![Push(TypedValue::Int(5))], vec![])])
        );
        assert_eq!(stack, stk![Type::Int]);
    }

    #[test]
    fn some() {
        let mut stack = stk![Type::Int];
        assert_eq!(
            typecheck(parse("{ SOME }").unwrap(), &mut Gas::default(), &mut stack),
            Ok(vec![ISome])
        );
        assert_eq!(stack, stk![Type::new_option(Type::Int)]);
    }
}
