/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

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
}

impl From<OutOfGas> for TcError {
    fn from(_: OutOfGas) -> Self {
        TcError::OutOfGas
    }
}

#[allow(dead_code)]
pub fn typecheck(ast: AST, gas: &mut Gas, stack: &mut TypeStack) -> Result<AST, TcError> {
    ast.into_iter()
        .map(|i| typecheck_instruction(i, gas, stack))
        .collect()
}

fn typecheck_instruction(
    i: ParsedInstruction,
    gas: &mut Gas,
    stack: &mut TypeStack,
) -> Result<ParsedInstruction, TcError> {
    use Instruction as I;
    use Type as T;

    gas.consume(gas::tc_cost::INSTR_STEP)?;

    Ok(match i {
        I::Add(..) => match stack.as_slice() {
            [.., T::Nat, T::Nat] => {
                stack.pop();
                I::Add(())
            }
            [.., T::Int, T::Int] => {
                stack.pop();
                I::Add(())
            }
            _ => unimplemented!(),
        },
        I::Dip(opt_height, nested) => {
            let protected_height: usize = opt_height.unwrap_or(1);

            gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;

            ensure_stack_len(stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, gas, stack)?;
            stack.append(&mut protected);
            I::Dip(opt_height, nested)
        }
        I::Drop(opt_height) => {
            let drop_height: usize = opt_height.unwrap_or(1);
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
            let dup_height: usize = opt_height.unwrap_or(1);
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
                ensure_stacks_eq(gas, t_stack.as_slice(), stack.as_slice())?;
                I::If(nested_t, nested_f)
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
                ensure_stacks_eq(gas, live.as_slice(), stack.as_slice())?;
                stack.pop();
                I::Loop(nested)
            }
            _ => return Err(TcError::GenericTcError),
        },
        I::Push(t, v) => {
            typecheck_value(gas, &t, &v)?;
            stack.push(t.to_owned());
            I::Push(t, v)
        }
        I::Swap => {
            ensure_stack_len(stack, 2)?;
            stack.swap(0, 1);
            I::Swap
        }
    })
}

fn typecheck_value(gas: &mut Gas, t: &Type, v: &Value) -> Result<(), TcError> {
    use Type::*;
    use Value::*;
    gas.consume(gas::tc_cost::VALUE_STEP)?;
    match (t, v) {
        (Nat, NumberValue(n)) if *n >= 0 => Ok(()),
        (Int, NumberValue(_)) => Ok(()),
        (Bool, BooleanValue(_)) => Ok(()),
        _ => Err(TcError::GenericTcError),
    }
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
fn ensure_stacks_eq(gas: &mut Gas, stack1: &[Type], stack2: &[Type]) -> Result<(), TcError> {
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
            typecheck_instruction(Push(Type::Int, Value::NumberValue(1)), &mut gas, &mut stack),
            Ok(Push(Type::Int, Value::NumberValue(1)))
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
            Ok(Dip(Some(1), vec![Push(Type::Nat, Value::NumberValue(6))]))
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
            Ok(Add(()))
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
            Ok(Add(()))
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
            Ok(Loop(vec![Push(Type::Bool, Value::BooleanValue(true))]))
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
}
