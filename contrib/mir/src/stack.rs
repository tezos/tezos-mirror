/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::VecDeque;

use crate::ast::*;

pub type TypeStack = VecDeque<Type>;

pub struct StackTooShort;

/// Ensures type stack is at least of the required length, otherwise returns
/// `Err(StackTooShort)`.
pub fn ensure_stack_len(stack: &TypeStack, l: usize) -> Result<(), StackTooShort> {
    if stack.len() >= l {
        Ok(())
    } else {
        Err(StackTooShort)
    }
}

pub struct StacksNotEqual;

/// Ensures two type stacks compare equal, otherwise returns `Err(StacksNotEqual)`.
pub fn ensure_stacks_eq(stack1: &[Type], stack2: &[Type]) -> Result<(), StacksNotEqual> {
    if stack1 == stack2 {
        Ok(())
    } else {
        Err(StacksNotEqual)
    }
}
