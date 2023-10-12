/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod parsed;
pub mod typechecked;

pub use parsed::{ParsedInstruction, ParsedStage};
pub use typechecked::{overloads, TypecheckedInstruction, TypecheckedStage};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Nat,
    Int,
    Bool,
    Mutez,
    String,
    Unit,
    Pair(Box<Type>, Box<Type>),
    Option(Box<Type>),
}

impl Type {
    /// Returns abstract size of the type representation. Used for gas cost
    /// estimation.
    pub fn size_for_gas(&self) -> usize {
        match self {
            Type::Nat => 1,
            Type::Int => 1,
            Type::Bool => 1,
            Type::Mutez => 1,
            Type::String => 1,
            Type::Unit => 1,
            Type::Pair(l, r) => 1 + l.size_for_gas() + r.size_for_gas(),
            Type::Option(x) => 1 + x.size_for_gas(),
        }
    }

    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new(l), Box::new(r))
    }

    pub fn new_option(x: Self) -> Self {
        Self::Option(Box::new(x))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    NumberValue(i128),
    BooleanValue(bool),
    StringValue(String),
    UnitValue,
    PairValue(Box<Value>, Box<Value>),
    OptionValue(Option<Box<Value>>),
}

impl Value {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::PairValue(Box::new(l), Box::new(r))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::OptionValue(x.map(Box::new))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypedValue {
    Int(i128),
    Nat(u128),
    Mutez(i64),
    Bool(bool),
    String(String),
    Unit,
    Pair(Box<TypedValue>, Box<TypedValue>),
    Option(Option<Box<TypedValue>>),
}

impl TypedValue {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new(l), Box::new(r))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Box::new))
    }
}

pub type ParsedInstructionBlock = Vec<ParsedInstruction>;

pub trait Stage {
    type AddMeta;
    type PushValue;
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction<T: Stage> {
    Add(T::AddMeta),
    Dip(Option<u16>, Vec<Instruction<T>>),
    Drop(Option<u16>),
    Dup(Option<u16>),
    Gt,
    If(Vec<Instruction<T>>, Vec<Instruction<T>>),
    IfNone(Vec<Instruction<T>>, Vec<Instruction<T>>),
    Int,
    Loop(Vec<Instruction<T>>),
    Push(T::PushValue),
    Swap,
    Failwith,
    Unit,
    Car,
    Cdr,
    Pair,
    /// `ISome` because `Some` is already taken
    ISome,
}

pub type ParsedAST = Vec<ParsedInstruction>;

pub type TypecheckedAST = Vec<TypecheckedInstruction>;
