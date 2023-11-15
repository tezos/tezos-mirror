/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod annotations;
pub mod comparable;
pub mod micheline;
pub mod michelson_address;
pub mod michelson_list;
pub mod or;
pub mod overloads;

pub use micheline::Micheline;
use std::collections::BTreeMap;
pub use tezos_crypto_rs::hash::ChainId;
use typed_arena::Arena;

use crate::lexer::Prim;

pub use michelson_address::*;
pub use michelson_list::MichelsonList;
pub use or::Or;

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
    Contract(Box<Type>),
    Address,
    ChainId,
}

impl Type {
    /// Returns abstract size of the type representation. Used for gas cost
    /// estimation.
    pub fn size_for_gas(&self) -> usize {
        use Type::*;
        match self {
            Nat | Int | Bool | Mutez | String | Unit | Operation | Address | ChainId => 1,
            Pair(p) | Or(p) | Map(p) => 1 + p.0.size_for_gas() + p.1.size_for_gas(),
            Option(x) | List(x) | Contract(x) => 1 + x.size_for_gas(),
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

    pub fn new_contract(ty: Self) -> Self {
        Self::Contract(Box::new(ty))
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
    Or(Box<Or<TypedValue, TypedValue>>),
    Address(Address),
    ChainId(ChainId),
    Contract(Address),
}

pub fn typed_value_to_value_optimized<'a>(
    arena: &'a Arena<Micheline<'a>>,
    tv: TypedValue,
) -> Micheline<'a> {
    use Micheline as V;
    use TypedValue as TV;
    let go = |x| typed_value_to_value_optimized(arena, x);
    match tv {
        TV::Int(i) => V::Int(i),
        TV::Nat(u) => V::Int(u.try_into().unwrap()),
        TV::Mutez(u) => V::Int(u.try_into().unwrap()),
        TV::Bool(true) => V::prim0(Prim::True),
        TV::Bool(false) => V::prim0(Prim::False),
        TV::String(s) => V::String(s),
        TV::Unit => V::prim0(Prim::Unit),
        // This transformation for pairs deviates from the optimized representation of the
        // reference implementation, because reference implementation optimizes the size of combs
        // and uses an untyped representation that is the shortest.
        TV::Pair(b) => V::prim2(arena, Prim::Pair, go(b.0), go(b.1)),
        TV::List(l) => V::Seq(arena.alloc_extend(l.into_iter().map(go))),
        TV::Map(m) => V::Seq(
            arena.alloc_extend(
                m.into_iter()
                    .map(|(key, val)| V::prim2(arena, Prim::Elt, go(key), go(val))),
            ),
        ),
        TV::Option(None) => V::prim0(Prim::None),
        TV::Option(Some(x)) => V::prim1(arena, Prim::Some, go(*x)),
        TV::Or(or) => match *or {
            Or::Left(x) => V::prim1(arena, Prim::Left, go(x)),
            Or::Right(x) => V::prim1(arena, Prim::Right, go(x)),
        },
        TV::Address(x) => V::Bytes(x.to_bytes_vec()),
        TV::ChainId(x) => V::Bytes(x.into()),
        TV::Contract(x) => go(TV::Address(x)),
    }
}

impl TypedValue {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new((l, r)))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Box::new))
    }

    pub fn new_or(x: Or<Self, Self>) -> Self {
        Self::Or(Box::new(x))
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    Add(overloads::Add),
    Dip(Option<u16>, Vec<Self>),
    Drop(Option<u16>),
    Dup(Option<u16>),
    Gt,
    If(Vec<Self>, Vec<Self>),
    IfNone(Vec<Self>, Vec<Self>),
    Int,
    Loop(Vec<Self>),
    Push(TypedValue),
    Swap,
    Failwith(Type),
    Unit,
    Car,
    Cdr,
    Pair,
    /// `ISome` because `Some` is already taken
    ISome,
    Compare,
    Amount,
    Nil,
    Get(overloads::Get),
    Update(overloads::Update),
    Seq(Vec<Self>),
    Unpair,
    Cons,
    IfCons(Vec<Self>, Vec<Self>),
    Iter(overloads::Iter, Vec<Self>),
    IfLeft(Vec<Self>, Vec<Self>),
    ChainId,
    /// `ISelf` because `Self` is a reserved keyword
    ISelf(Entrypoint),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractScript {
    pub parameter: Type,
    pub storage: Type,
    pub code: Instruction,
}
