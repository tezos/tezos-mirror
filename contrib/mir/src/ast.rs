/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Nat,
    Int,
    Bool,
}

impl Type {
    /// Returns abstract size of the type representation. Used for gas cost
    /// estimation.
    pub fn size_for_gas(&self) -> usize {
        match self {
            Type::Nat => 1,
            Type::Int => 1,
            Type::Bool => 1,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    NumberValue(i32),
    BooleanValue(bool),
}

pub type InstructionBlock = Vec<Instruction>;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Add,
    Dip(Option<usize>, InstructionBlock),
    Drop(Option<usize>),
    Dup(Option<usize>),
    Gt,
    If(InstructionBlock, InstructionBlock),
    Int,
    Loop(InstructionBlock),
    Push(Type, Value),
    Swap,
}

pub type AST = Vec<Instruction>;
