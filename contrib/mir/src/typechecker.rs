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
pub fn typecheck(ast: &AST, gas: &mut Gas, stack: &mut TypeStack) -> Result<(), TcError> {
    for i in ast {
        typecheck_instruction(&i, gas, stack)?;
    }
    Ok(())
}

fn typecheck_instruction(
    i: &Instruction,
    gas: &mut Gas,
    stack: &mut TypeStack,
) -> Result<(), TcError> {
    use Instruction::*;
    use Type::*;

    gas.consume(gas::tc_cost::INSTR_STEP)?;

    match i {
        Add => match stack.as_slice() {
            [.., Type::Nat, Type::Nat] => {
                stack.pop();
            }
            [.., Type::Int, Type::Int] => {
                stack.pop();
            }
            _ => unimplemented!(),
        },
        Dip(opt_height, nested) => {
            let protected_height: usize = opt_height.unwrap_or(1);

            gas.consume(gas::tc_cost::dip_n(opt_height)?)?;

            ensure_stack_len(stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            typecheck(nested, gas, stack)?;
            stack.append(&mut protected);
        }
        Drop(opt_height) => {
            let drop_height: usize = opt_height.unwrap_or(1);
            gas.consume(gas::tc_cost::drop_n(opt_height)?)?;
            ensure_stack_len(&stack, drop_height)?;
            stack.drop_top(drop_height);
        }
        Dup(Some(0)) => {
            // DUP instruction requires an argument that is > 0.
            return Err(TcError::GenericTcError);
        }
        Dup(opt_height) => {
            let dup_height: usize = opt_height.unwrap_or(1);
            ensure_stack_len(stack, dup_height)?;
            stack.push(stack[dup_height - 1].clone());
        }
        Gt => match stack.pop() {
            Some(Type::Int) => stack.push(Type::Bool),
            _ => return Err(TcError::GenericTcError),
        },
        If(nested_t, nested_f) => match stack.pop() {
            // Check if top is bool
            Some(Type::Bool) => {
                // Clone the stack so that we have a copy to run one branch on.
                // We can run the other branch on the live stack.
                let mut t_stack: TypeStack = stack.clone();
                typecheck(nested_t, gas, &mut t_stack)?;
                typecheck(nested_f, gas, stack)?;
                // If both stacks are same after typecheck, all is good.
                ensure_stacks_eq(gas, t_stack.as_slice(), stack.as_slice())?;
            }
            _ => return Err(TcError::GenericTcError),
        },
        Instruction::Int => match stack.pop() {
            Some(Type::Nat) => stack.push(Type::Int),
            _ => return Err(TcError::GenericTcError),
        },
        Loop(nested) => match stack.as_slice() {
            // Check if top is bool and bind the tail to `t`.
            [t @ .., Bool] => {
                let mut live: TypeStack = TopIsLast::from(t).0;
                // Clone the tail and typecheck the nested body using it.
                typecheck(nested, gas, &mut live)?;
                // If the starting stack and result stack match
                // then the typecheck is complete. pop the bool
                // off the original stack to form the final result.
                ensure_stacks_eq(gas, live.as_slice(), stack.as_slice())?;
                stack.pop();
            }
            _ => return Err(TcError::GenericTcError),
        },
        Push(t, v) => {
            typecheck_value(gas, &t, &v)?;
            stack.push(t.to_owned());
        }
        Swap => {
            ensure_stack_len(stack, 2)?;
            stack.swap(0, 1);
        }
    }
    Ok(())
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
        typecheck_instruction(&Dup(Some(1)), &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_dup_n() {
        let mut stack = stk![Type::Int, Type::Nat];
        let expected_stack = stk![Type::Int, Type::Nat, Type::Int];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Dup(Some(2)), &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_swap() {
        let mut stack = stk![Type::Nat, Type::Int];
        let expected_stack = stk![Type::Int, Type::Nat];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Swap, &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_int() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Int, &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_drop() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![];
        let mut gas = Gas::new(10000);
        typecheck(&parse("{DROP}").unwrap(), &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_drop_n() {
        let mut stack = stk![Type::Nat, Type::Int];
        let expected_stack = stk![];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Drop(Some(2)), &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440 - 2 * 50);
    }

    #[test]
    fn test_push() {
        let mut stack = stk![Type::Nat];
        let expected_stack = stk![Type::Nat, Type::Int];
        let mut gas = Gas::new(10000);
        typecheck_instruction(
            &Push(Type::Int, Value::NumberValue(1)),
            &mut gas,
            &mut stack,
        )
        .unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440 - 100);
    }

    #[test]
    fn test_gt() {
        let mut stack = stk![Type::Int];
        let expected_stack = stk![Type::Bool];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Gt, &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_dip() {
        let mut stack = stk![Type::Int, Type::Bool];
        let expected_stack = stk![Type::Int, Type::Nat, Type::Bool];
        let mut gas = Gas::new(10000);
        typecheck_instruction(
            &Dip(Some(1), parse("{PUSH nat 6}").unwrap()),
            &mut gas,
            &mut stack,
        )
        .unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440 - 440 - 100 - 50);
    }

    #[test]
    fn test_add() {
        let mut stack = stk![Type::Int, Type::Int];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        typecheck_instruction(&Add, &mut gas, &mut stack).unwrap();
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440);
    }

    #[test]
    fn test_loop() {
        let mut stack = stk![Type::Int, Type::Bool];
        let expected_stack = stk![Type::Int];
        let mut gas = Gas::new(10000);
        assert!(typecheck_instruction(
            &Loop(parse("{PUSH bool True}").unwrap()),
            &mut gas,
            &mut stack
        )
        .is_ok());
        assert!(stack == expected_stack);
        assert!(gas.milligas() == 10000 - 440 - 440 - 100 - 60 * 2);
    }

    #[test]
    fn test_loop_stacks_not_equal_length() {
        let mut stack = stk![Type::Int, Type::Bool];
        let mut gas = Gas::new(10000);
        assert!(
            typecheck_instruction(
                &Loop(parse("{PUSH int 1; PUSH bool True}").unwrap()),
                &mut gas,
                &mut stack
            ) == Err(TcError::StacksNotEqual)
        );
    }

    #[test]
    fn test_loop_stacks_not_equal_types() {
        let mut stack = stk![Type::Int, Type::Bool];
        let mut gas = Gas::new(10000);
        assert!(
            typecheck_instruction(
                &Loop(parse("{DROP; PUSH bool False; PUSH bool True}").unwrap()),
                &mut gas,
                &mut stack
            ) == Err(TcError::StacksNotEqual)
        );
    }
}
