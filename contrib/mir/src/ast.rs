/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod comparable;
pub mod michelson_list;
pub mod parsed;
pub mod typechecked;

use std::collections::BTreeMap;

pub use michelson_list::MichelsonList;
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
    Pair(Box<(Type, Type)>),
    Option(Box<Type>),
    List(Box<Type>),
    Operation,
    Map(Box<(Type, Type)>),
    Or(Box<(Type, Type)>),
}

impl Type {
    pub fn is_comparable(&self) -> bool {
        use Type::*;
        match self {
            List(..) | Map(..) => false,
            Operation => false,
            Nat | Int | Bool | Mutez | String | Unit => true,
            Pair(p) | Or(p) => p.0.is_comparable() && p.1.is_comparable(),
            Option(x) => x.is_comparable(),
        }
    }

    pub fn is_packable(&self) -> bool {
        use Type::*;
        match self {
            Operation => false,
            Nat | Int | Bool | Mutez | String | Unit => true,
            Pair(p) | Or(p) => p.0.is_packable() && p.1.is_packable(),
            Option(x) | List(x) => x.is_packable(),
            Map(m) => m.1.is_packable(),
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
            Type::Pair(p) | Type::Or(p) => 1 + p.0.size_for_gas() + p.1.size_for_gas(),
            Type::Option(x) => 1 + x.size_for_gas(),
            Type::List(x) => 1 + x.size_for_gas(),
            Type::Map(m) => 1 + m.0.size_for_gas() + m.1.size_for_gas(),
        }
    }

    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new((l, r)))
    }

    pub fn new_option(x: Self) -> Self {
        Self::Option(Box::new(x))
    }

    pub fn new_list(x: Self) -> Self {
        Self::List(Box::new(x))
    }

    pub fn new_map(k: Self, v: Self) -> Self {
        Self::Map(Box::new((k, v)))
    }

    pub fn new_or(l: Self, r: Self) -> Self {
        Self::Or(Box::new((l, r)))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    Number(i128),
    Boolean(bool),
    String(String),
    Unit,
    Pair(Box<(Value, Value)>),
    Option(Option<Box<Value>>),
    Seq(Vec<Value>),
    Elt(Box<(Value, Value)>),
}

impl Value {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new((l, r)))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Box::new))
    }

    pub fn new_elt(k: Self, v: Self) -> Self {
        Self::Elt(Box::new((k, v)))
    }
}

macro_rules! valuefrom {
    ($( <$($gs:ident),*> $ty:ty, $cons:expr );* $(;)*) => {
        $(
        impl<$($gs),*> From<$ty> for Value where $($gs: Into<Value>),* {
            fn from(x: $ty) -> Self {
                $cons(x)
            }
        }
        )*
    };
}

/// Simple helper for constructing Elt values:
///
/// ```text
/// let val: Value = Elt("foo", 3).into()
/// ```
pub struct Elt<K, V>(pub K, pub V);

valuefrom! {
  <> i128, Value::Number;
  <> bool, Value::Boolean;
  <> String, Value::String;
  <> (), |_| Value::Unit;
  <L, R> (L, R), |(l, r): (L, R)| Value::new_pair(l.into(), r.into());
  <L, R> Elt<L, R>, |Elt(l, r): Elt<L, R>| Value::new_elt(l.into(), r.into());
  <T> Option<T>, |x: Option<T>| Value::new_option(x.map(Into::into));
  <T> Vec<T>, |x: Vec<T>| Value::Seq(x.into_iter().map(Into::into).collect());
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_owned())
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
    Pair(Box<(TypedValue, TypedValue)>),
    Option(Option<Box<TypedValue>>),
    List(MichelsonList<TypedValue>),
    Map(BTreeMap<TypedValue, TypedValue>),
}

pub fn typed_value_to_value_optimized(tv: TypedValue) -> Value {
    use TypedValue as TV;
    use Value as V;
    match tv {
        TV::Int(i) => V::Number(i),
        TV::Nat(u) => V::Number(u.try_into().unwrap()),
        TV::Mutez(u) => V::Number(u.try_into().unwrap()),
        TV::Bool(b) => V::Boolean(b),
        TV::String(s) => V::String(s),
        TV::Unit => V::Unit,
        // This transformation for pairs deviates from the optimized representation of the
        // reference implementation, because reference implementation optimizes the size of combs
        // and uses an untyped representation that is the shortest.
        TV::Pair(b) => V::new_pair(
            typed_value_to_value_optimized(b.0),
            typed_value_to_value_optimized(b.1),
        ),
        TV::List(l) => V::Seq(l.into_iter().map(typed_value_to_value_optimized).collect()),
        TV::Map(m) => V::Seq(
            m.into_iter()
                .map(|(key, val)| {
                    V::new_elt(
                        typed_value_to_value_optimized(key),
                        typed_value_to_value_optimized(val),
                    )
                })
                .collect(),
        ),
        TV::Option(None) => V::Option(None),
        TV::Option(Some(r)) => V::new_option(Some(typed_value_to_value_optimized(*r))),
    }
}

// Note that there are more than one way to do this conversion. Here we use the optimized untyped
// representation as the target, since that is what the typed to untyped conversion during a
// FAILWITH call does in the reference implementation, and this logic is primarly used in the
// corresponding section of MIR now.
//
// TODO: This implementation will be moved to interpreter in the context of issue,
// https://gitlab.com/tezos/tezos/-/issues/6504
impl From<TypedValue> for Value {
    fn from(tv: TypedValue) -> Self {
        typed_value_to_value_optimized(tv)
    }
}

impl TypedValue {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new((l, r)))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Box::new))
    }
}

pub type ParsedInstructionBlock = Vec<ParsedInstruction>;

macro_rules! meta_types {
    ($($i:ident),* $(,)*) => {
        $(type $i: std::fmt::Debug + PartialEq + Clone;)*
    };
}

pub trait Stage {
    meta_types! {
      AddMeta,
      PushValue,
      NilType,
      GetOverload,
      UpdateOverload,
      FailwithType,
      IterOverload,
    }
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
    Failwith(T::FailwithType),
    Unit,
    Car,
    Cdr,
    Pair,
    /// `ISome` because `Some` is already taken
    ISome,
    Compare,
    Amount,
    Nil(T::NilType),
    Get(T::GetOverload),
    Update(T::UpdateOverload),
    Seq(Vec<Instruction<T>>),
    Unpair,
    Cons,
    IfCons(Vec<Instruction<T>>, Vec<Instruction<T>>),
    Iter(T::IterOverload, Vec<Instruction<T>>),
}

pub type ParsedAST = Vec<ParsedInstruction>;

pub type TypecheckedAST = Vec<TypecheckedInstruction>;

#[derive(Debug, Clone, PartialEq)]
pub struct ContractScript<T: Stage> {
    pub parameter: Type,
    pub storage: Type,
    pub code: Instruction<T>,
}
