/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod annotations;
pub mod byte_repr_trait;
pub mod comparable;
pub mod micheline;
pub mod michelson_address;
pub mod michelson_key;
pub mod michelson_key_hash;
pub mod michelson_lambda;
pub mod michelson_list;
pub mod michelson_signature;
pub mod or;
pub mod overloads;

pub use micheline::Micheline;
use num_bigint::{BigInt, BigUint};
use std::collections::{BTreeMap, BTreeSet};
pub use tezos_crypto_rs::hash::ChainId;
use typed_arena::Arena;

use crate::lexer::Prim;

pub use byte_repr_trait::{ByteReprError, ByteReprTrait};
pub use michelson_address::*;
pub use michelson_key::Key;
pub use michelson_key_hash::KeyHash;
pub use michelson_lambda::Lambda;
pub use michelson_list::MichelsonList;
pub use michelson_signature::Signature;
pub use or::Or;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TransferTokens<'a> {
    pub param: TypedValue<'a>,
    pub destination_address: Address,
    pub amount: i64,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SetDelegate(pub Option<KeyHash>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operation<'a> {
    TransferTokens(TransferTokens<'a>),
    SetDelegate(SetDelegate),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OperationInfo<'a> {
    pub operation: Operation<'a>,
    pub counter: u128,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Nat,
    Int,
    Bool,
    Mutez,
    String,
    Unit,
    Never,
    Pair(Box<(Type, Type)>),
    Option(Box<Type>),
    List(Box<Type>),
    Operation,
    Set(Box<Type>),
    Map(Box<(Type, Type)>),
    Or(Box<(Type, Type)>),
    Contract(Box<Type>),
    Address,
    ChainId,
    Bytes,
    Key,
    Signature,
    KeyHash,
    Lambda(Box<(Type, Type)>),
}

impl Type {
    /// Returns abstract size of the type representation. Used for gas cost
    /// estimation.
    pub fn size_for_gas(&self) -> usize {
        use Type::*;
        match self {
            Nat | Int | Bool | Mutez | String | Unit | Never | Operation | Address | ChainId
            | Bytes | Key | Signature | KeyHash => 1,
            Pair(p) | Or(p) | Map(p) | Lambda(p) => 1 + p.0.size_for_gas() + p.1.size_for_gas(),
            Option(x) | List(x) | Set(x) | Contract(x) => 1 + x.size_for_gas(),
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

    pub fn new_set(v: Self) -> Self {
        Self::Set(Box::new(v))
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

    pub fn new_lambda(ty1: Self, ty2: Self) -> Self {
        Self::Lambda(Box::new((ty1, ty2)))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypedValue<'a> {
    Int(BigInt),
    Nat(BigUint),
    Mutez(i64),
    Bool(bool),
    String(String),
    Unit,
    Pair(Box<(Self, Self)>),
    Option(Option<Box<Self>>),
    List(MichelsonList<Self>),
    Set(BTreeSet<Self>),
    Map(BTreeMap<Self, Self>),
    Or(Box<Or<Self, Self>>),
    Address(Address),
    ChainId(ChainId),
    Contract(Address),
    Bytes(Vec<u8>),
    Key(Key),
    Signature(Signature),
    Lambda(Lambda<'a>),
    KeyHash(KeyHash),
    Operation(Box<OperationInfo<'a>>),
}

/// Untypes a value using optimized representation in legacy mode.
///
/// This differs from plain optimized representation in that it always
/// represents tuples as nested binary pairs (right combs). This is, for
/// instance, what `PACK` uses.
pub fn typed_value_to_value_optimized_legacy<'a>(
    arena: &'a Arena<Micheline<'a>>,
    tv: TypedValue<'a>,
) -> Micheline<'a> {
    use Micheline as V;
    use TypedValue as TV;
    let go = |x| typed_value_to_value_optimized_legacy(arena, x);
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
        TV::Set(s) => V::Seq(arena.alloc_extend(s.into_iter().map(go))),
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
        TV::Bytes(x) => V::Bytes(x),
        TV::Key(k) => V::Bytes(k.to_bytes_vec()),
        TV::Signature(s) => V::Bytes(s.to_bytes_vec()),
        TV::Lambda(lam) => match lam {
            Lambda::Lambda { micheline_code, .. } => micheline_code,
            Lambda::LambdaRec { micheline_code, .. } => {
                V::prim1(arena, Prim::Lambda_rec, micheline_code)
            }
        },
        TV::KeyHash(s) => V::Bytes(s.to_bytes_vec()),
        TV::Contract(x) => go(TV::Address(x)),
        TV::Operation(operation_info) => match operation_info.operation {
            Operation::TransferTokens(tt) => Micheline::App(
                Prim::Transfer_tokens,
                arena.alloc_extend([
                    go(tt.param),
                    go(TV::Address(tt.destination_address)),
                    go(TV::Mutez(tt.amount)),
                ]),
                annotations::NO_ANNS,
            ),
            Operation::SetDelegate(sd) => Micheline::App(
                Prim::Set_delegate,
                arena.alloc_extend([match sd.0 {
                    Some(kh) => V::prim1(arena, Prim::Some, go(TV::KeyHash(kh))),
                    None => V::prim0(Prim::None),
                }]),
                annotations::NO_ANNS,
            ),
        },
    }
}

impl<'a> TypedValue<'a> {
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Box::new((l, r)))
    }

    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Box::new))
    }

    pub fn new_or(x: Or<Self, Self>) -> Self {
        Self::Or(Box::new(x))
    }

    pub fn new_operation(o: Operation<'a>, c: u128) -> Self {
        Self::Operation(Box::new(OperationInfo {
            operation: o,
            counter: c,
        }))
    }

    /// Helper for more easily constructing `Int` variant with literals. Mostly
    /// useful in tests.
    pub fn int(n: impl Into<BigInt>) -> Self {
        Self::Int(n.into())
    }

    /// Helper for more easily constructing `Nat` variant with literals. Mostly
    /// useful in tests.
    pub fn nat(n: u32) -> Self {
        Self::Nat(n.into())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction<'a> {
    Add(overloads::Add),
    Dip(Option<u16>, Vec<Self>),
    Drop(Option<u16>),
    Dup(Option<u16>),
    Gt,
    Eq,
    Le,
    If(Vec<Self>, Vec<Self>),
    IfNone(Vec<Self>, Vec<Self>),
    Int,
    Loop(Vec<Self>),
    Push(TypedValue<'a>),
    Swap,
    Failwith(Type),
    Never,
    Unit,
    Car,
    Cdr,
    Pair,
    /// `ISome` because `Some` is already taken
    ISome,
    None,
    Compare,
    Amount,
    Nil,
    EmptySet,
    Mem(overloads::Mem),
    Get(overloads::Get),
    Update(overloads::Update),
    Concat(overloads::Concat),
    Seq(Vec<Self>),
    Unpair,
    Cons,
    And(overloads::And),
    Or(overloads::Or),
    Xor(overloads::Xor),
    IfCons(Vec<Self>, Vec<Self>),
    Iter(overloads::Iter, Vec<Self>),
    IfLeft(Vec<Self>, Vec<Self>),
    ChainId,
    /// `ISelf` because `Self` is a reserved keyword
    ISelf(Entrypoint),
    Pack,
    CheckSignature,
    TransferTokens,
    SetDelegate,
    Address,
    Slice(overloads::Slice),
    Left,
    Right,
    Lambda(Lambda<'a>),
    Exec,
    HashKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractScript<'a> {
    pub parameter: Type,
    pub storage: Type,
    pub code: Instruction<'a>,
}
