/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod parsed;

pub use parsed::{ParsedInstruction, ParsedStage};

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

pub type InstructionBlock = Vec<ParsedInstruction>;

pub trait Stage {
    type AddMeta;
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction<T: Stage> {
    Add(T::AddMeta),
    Dip(Option<usize>, Vec<Instruction<T>>),
    Drop(Option<usize>),
    Dup(Option<usize>),
    Gt,
    If(Vec<Instruction<T>>, Vec<Instruction<T>>),
    Int,
    Loop(Vec<Instruction<T>>),
    Push(Type, Value),
    Swap,
}

pub type AST = Vec<ParsedInstruction>;
