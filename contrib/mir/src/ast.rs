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

#[derive(Debug, Eq, PartialEq)]
pub enum Value {
    NumberValue(i32),
    BooleanValue(bool)
}

pub type InstructionBlock = Vec<Instruction>;

#[derive(Debug, Eq, PartialEq)]
pub enum Instruction {
    Add,
    Dip(InstructionBlock),
    DipN(usize, InstructionBlock),
    Drop,
    DropN(usize),
    Dup,
    DupN(usize),
    Gt,
    If(InstructionBlock, InstructionBlock),
    Int,
    Loop(InstructionBlock),
    Push(Type, Value),
    Swap,
}

pub type AST = Vec<Instruction>;
