/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug)]
pub enum Type {
    Nat,
    Int,
}

#[derive(Debug)]
pub enum Value {
    NumberValue(i32),
}

pub type InstructionBlock = Vec<Instruction>;

#[derive(Debug)]
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
