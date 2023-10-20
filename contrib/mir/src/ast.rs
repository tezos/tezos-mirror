/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod comparable;
pub mod parsed;
pub mod typechecked;

use std::collections::BTreeMap;

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
    List(Box<Type>),
    Operation,
    Map(Box<Type>, Box<Type>),
}

impl Type {
    pub fn is_comparable(&self) -> bool {
        use Type::*;
        match &self {
            List(..) | Map(..) => false,
            Operation => false,
            Nat | Int | Bool | Mutez | String | Unit => true,
            Pair(l, r) => l.is_comparable() && r.is_comparable(),
            Option(x) => x.is_comparable(),
        }
    }

    pub fn is_packable(&self) -> bool {
        use Type::*;
        match self {
            Operation => false,
            Nat | Int | Bool | Mutez | String | Unit => true,
            Pair(l, r) => l.is_packable() && r.is_packable(),
            Option(x) | List(x) | Map(_, x) => x.is_packable(),
        }
    }

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
            Type::Operation => 1,
            Type::Pair(l, r) => 1 + l.size_for_gas() + r.size_for_gas(),
            Type::Option(x) => 1 + x.size_for_gas(),
            Type::List(x) => 1 + x.size_for_gas(),
            Type::Map(k, v) => 1 + k.size_for_gas() + v.size_for_gas(),
        }
    }

    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new(l), Box::new(r))
    }

    pub fn new_option(x: Self) -> Self {
        Self::Option(Box::new(x))
    }

    pub fn new_list(x: Self) -> Self {
        Self::List(Box::new(x))
    }

    pub fn new_map(k: Self, v: Self) -> Self {
        Self::Map(Box::new(k), Box::new(v))
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
    Seq(Vec<Value>),
    Elt(Box<Value>, Box<Value>),
}

impl Value {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::PairValue(Box::new(l), Box::new(r))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::OptionValue(x.map(Box::new))
    }

    pub fn new_elt(k: Self, v: Self) -> Self {
        Self::Elt(Box::new(k), Box::new(v))
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
    List(Vec<TypedValue>),
    Map(BTreeMap<TypedValue, TypedValue>),
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
    type NilType;
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
    Compare,
    Amount,
    Nil(T::NilType),
}

pub type ParsedAST = Vec<ParsedInstruction>;

pub type TypecheckedAST = Vec<TypecheckedInstruction>;
