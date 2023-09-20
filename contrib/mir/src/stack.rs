/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::VecDeque;

use crate::ast::*;
use crate::gas;
use crate::gas::Gas;
use crate::typechecker::TcError;

pub type TypeStack = VecDeque<Type>;

/// Ensures type stack is at least of the required length, otherwise returns
/// `Err(StackTooShort)`.
pub fn ensure_stack_len(stack: &TypeStack, l: usize) -> Result<(), TcError> {
    if stack.len() >= l {
        Ok(())
    } else {
        Err(TcError::StackTooShort)
    }
}

/// Ensures two type stacks compare equal, otherwise returns
/// `Err(StacksNotEqual)`. If runs out of gas, returns `Err(OutOfGas)` instead.
pub fn ensure_stacks_eq(gas: &mut Gas, stack1: &[Type], stack2: &[Type]) -> Result<(), TcError> {
    if stack1.len() != stack2.len() {
        return Err(TcError::StacksNotEqual);
    }
    for (ty1, ty2) in stack1.iter().zip(stack2.iter()) {
        ensure_ty_eq(gas, ty1, ty2)?;
    }
    Ok(())
}

fn ensure_ty_eq(gas: &mut Gas, ty1: &Type, ty2: &Type) -> Result<(), TcError> {
    gas.consume(gas::tc_cost::ty_eq(ty1.size_for_gas(), ty2.size_for_gas()))?;
    if ty1 != ty2 {
        Err(TcError::StacksNotEqual)
    } else {
        Ok(())
    }
}
