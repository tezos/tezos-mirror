// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! AST definitions for raw ([Micheline]) and typed representations of
//! Michelson.

pub mod annotations;
pub mod big_map;
pub mod byte_repr_trait;
mod comparable;
pub mod micheline;
pub mod michelson_address;
pub mod michelson_lambda;
pub mod michelson_list;
pub mod michelson_operation;
pub mod or;
pub mod overloads;

pub use micheline::Micheline;
use num_bigint::{BigInt, BigUint};
use std::collections::HashMap;
use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};
use tezos_crypto_rs::public_key::PublicKey;
/// Reexported from [tezos_crypto_rs::hash]. Typechecked values of the Michelson
/// type `chain_id`.
pub use tezos_crypto_rs::{hash::ChainId, public_key_hash::PublicKeyHash};
pub use tezos_data_encoding::enc::BinWriter;
use typed_arena::Arena;

use crate::lexer::Prim;

#[cfg(feature = "bls")]
use crate::bls;

pub use annotations::{Annotation, Annotations, FieldAnnotation, NO_ANNS};
pub use big_map::BigMap;
pub use byte_repr_trait::{ByteReprError, ByteReprTrait};
pub use micheline::IntoMicheline;
pub use michelson_address::*;
pub use michelson_lambda::{Closure, Lambda};
pub use michelson_list::MichelsonList;
pub use michelson_operation::{
    CreateContract, Emit, Operation, OperationInfo, SetDelegate, TransferTokens,
};
pub use or::Or;
pub use tezos_crypto_rs::signature::Signature;

/// Representation for values of the Michelson `ticket` type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ticket<'a> {
    /// Ticketer, the address of the contract that issued the ticket.
    pub ticketer: AddressHash,
    /// Type of the payload.
    pub content_type: Type,
    /// Ticket payload.
    pub content: TypedValue<'a>,
    /// Ticket amount.
    pub amount: BigUint,
}

/// Representation for a Michelson type. Used primarily in the typechecker. Note
/// this representation doesn't store annotations, as annotations are mostly
/// deprecated and ingored. For entrypoints, see
/// [crate::ast::michelson_address::entrypoint].
///
/// The names of the variants correspond to the names of Michelson types, but
/// snake_case is converted to PascalCase.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum Type {
    Nat,
    Int,
    Bool,
    Mutez,
    String,
    Unit,
    Never,
    Pair(Rc<(Type, Type)>),
    Option(Rc<Type>),
    List(Rc<Type>),
    Operation,
    Set(Rc<Type>),
    Map(Rc<(Type, Type)>),
    BigMap(Rc<(Type, Type)>),
    Or(Rc<(Type, Type)>),
    Contract(Rc<Type>),
    Address,
    ChainId,
    Bytes,
    Key,
    Signature,
    KeyHash,
    Lambda(Rc<(Type, Type)>),
    Ticket(Rc<Type>),
    Timestamp,
    #[cfg(feature = "bls")]
    Bls12381Fr,
    #[cfg(feature = "bls")]
    Bls12381G1,
    #[cfg(feature = "bls")]
    Bls12381G2,
}

impl Type {
    /// Returns abstract size of the type representation. Used for gas cost
    /// estimation.
    pub fn size_for_gas(&self) -> usize {
        use Type::*;
        match self {
            Nat | Int | Bool | Mutez | String | Unit | Never | Operation | Address | ChainId
            | Bytes | Key | Signature | KeyHash | Timestamp => 1,

            #[cfg(feature = "bls")]
            Bls12381Fr | Bls12381G1 | Bls12381G2 => 1,

            Pair(p) | Or(p) | Map(p) | BigMap(p) | Lambda(p) => {
                1 + p.0.size_for_gas() + p.1.size_for_gas()
            }
            Option(x) | List(x) | Set(x) | Contract(x) | Ticket(x) => 1 + x.size_for_gas(),
        }
    }

    /// Convenience function to construct a new [Self::Pair]. Allocates a new [Rc].
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Rc::new((l, r)))
    }

    /// Convenience function to construct a new [Self::Option]. Allocates a new [Rc].
    pub fn new_option(x: Self) -> Self {
        Self::Option(Rc::new(x))
    }

    /// Convenience function to construct a new [Self::List]. Allocates a new [Rc].
    pub fn new_list(x: Self) -> Self {
        Self::List(Rc::new(x))
    }

    /// Convenience function to construct a new [Self::Set]. Allocates a new [Rc].
    pub fn new_set(v: Self) -> Self {
        Self::Set(Rc::new(v))
    }

    /// Convenience function to construct a new [Self::Map]. Allocates a new [Rc].
    pub fn new_map(k: Self, v: Self) -> Self {
        Self::Map(Rc::new((k, v)))
    }

    /// Convenience function to construct a new [Self::BigMap]. Allocates a new [Rc].
    pub fn new_big_map(k: Self, v: Self) -> Self {
        Self::BigMap(Rc::new((k, v)))
    }

    /// Convenience function to construct a new [Self::Or]. Allocates a new [Rc].
    pub fn new_or(l: Self, r: Self) -> Self {
        Self::Or(Rc::new((l, r)))
    }

    /// Convenience function to construct a new [Self::Contract]. Allocates a new [Rc].
    pub fn new_contract(ty: Self) -> Self {
        Self::Contract(Rc::new(ty))
    }

    /// Convenience function to construct a new [Self::Ticket]. Allocates a new [Rc].
    pub fn new_ticket(ty: Self) -> Self {
        Self::Ticket(Rc::new(ty))
    }

    /// Convenience function to construct a new [Self::Lambda]. Allocates a new [Rc].
    pub fn new_lambda(ty1: Self, ty2: Self) -> Self {
        Self::Lambda(Rc::new((ty1, ty2)))
    }
}

impl<'a> IntoMicheline<'a> for &'_ Type {
    fn into_micheline_optimized_legacy(self, arena: &'a Arena<Micheline<'a>>) -> Micheline<'a> {
        use Type::*;

        struct LinearizePairIter<'a>(std::option::Option<&'a Type>);

        impl<'a> Iterator for LinearizePairIter<'a> {
            type Item = &'a Type;
            fn next(&mut self) -> std::option::Option<Self::Item> {
                match self.0 {
                    Some(Type::Pair(x)) => {
                        self.0 = Some(&x.1);
                        Some(&x.0)
                    }
                    ty => {
                        self.0 = None;
                        ty
                    }
                }
            }

            fn size_hint(&self) -> (usize, std::option::Option<usize>) {
                let Some(mut ty) = self.0 else {
                    return (0, Some(0));
                };
                let mut size: usize = 1;
                while let Type::Pair(x) = ty {
                    ty = &x.1;
                    size += 1;
                }
                (size, Some(size))
            }
        }

        impl ExactSizeIterator for LinearizePairIter<'_> {}

        match self {
            Nat => Micheline::prim0(Prim::nat),
            Int => Micheline::prim0(Prim::int),
            Bool => Micheline::prim0(Prim::bool),
            Mutez => Micheline::prim0(Prim::mutez),
            String => Micheline::prim0(Prim::string),
            Unit => Micheline::prim0(Prim::unit),
            Operation => Micheline::prim0(Prim::operation),
            Address => Micheline::prim0(Prim::address),
            ChainId => Micheline::prim0(Prim::chain_id),
            Bytes => Micheline::prim0(Prim::bytes),
            Key => Micheline::prim0(Prim::key),
            Signature => Micheline::prim0(Prim::signature),
            Timestamp => Micheline::prim0(Prim::timestamp),
            KeyHash => Micheline::prim0(Prim::key_hash),
            Never => Micheline::prim0(Prim::never),
            #[cfg(feature = "bls")]
            Bls12381Fr => Micheline::prim0(Prim::bls12_381_fr),
            #[cfg(feature = "bls")]
            Bls12381G1 => Micheline::prim0(Prim::bls12_381_g1),
            #[cfg(feature = "bls")]
            Bls12381G2 => Micheline::prim0(Prim::bls12_381_g2),

            Option(x) => Micheline::prim1(
                arena,
                Prim::option,
                x.into_micheline_optimized_legacy(arena),
            ),
            List(x) => {
                Micheline::prim1(arena, Prim::list, x.into_micheline_optimized_legacy(arena))
            }
            Set(x) => Micheline::prim1(arena, Prim::set, x.into_micheline_optimized_legacy(arena)),
            Contract(x) => Micheline::prim1(
                arena,
                Prim::contract,
                x.into_micheline_optimized_legacy(arena),
            ),
            Ticket(x) => Micheline::prim1(
                arena,
                Prim::ticket,
                x.into_micheline_optimized_legacy(arena),
            ),

            Pair(_) => Micheline::App(
                Prim::pair,
                Micheline::alloc_iter(
                    arena,
                    LinearizePairIter(Some(self)).map(|x| x.into_micheline_optimized_legacy(arena)),
                ),
                NO_ANNS,
            ),
            Map(x) => Micheline::prim2(
                arena,
                Prim::map,
                x.0.into_micheline_optimized_legacy(arena),
                x.1.into_micheline_optimized_legacy(arena),
            ),
            BigMap(x) => Micheline::prim2(
                arena,
                Prim::big_map,
                x.0.into_micheline_optimized_legacy(arena),
                x.1.into_micheline_optimized_legacy(arena),
            ),
            Or(x) => Micheline::prim2(
                arena,
                Prim::or,
                x.0.into_micheline_optimized_legacy(arena),
                x.1.into_micheline_optimized_legacy(arena),
            ),
            Lambda(x) => Micheline::prim2(
                arena,
                Prim::lambda,
                x.0.into_micheline_optimized_legacy(arena),
                x.1.into_micheline_optimized_legacy(arena),
            ),
        }
    }
}

/// Enum representing an arbitrary typed Michelson value. The name of the
/// variant corresponds to the name of the type, with snake_case converted to
/// PascalCase.
///
/// This is used primarily by the interpreter. It should be noted the type has a
/// few quirks related to ordering and equality:
///
/// 1. Comparing two `TypedValue`s for equality is only well-defined if both are
///    known to have the same type. `TypedValue` itself does not carry its type,
///    so, for instance two empty lists with elements of different types will
///    compare equal.
/// 2. The [Ord] instance can panic if values being compared are of different
///    types and/or are incomparable. This is fine for the interpreter, as the
///    typechecker has verified this invariant holds. However, be mindful of
///    this when comparing `TypedValue` in client code. [PartialOrd] is safe to
///    use, it'll just return [None] for incomparable values.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub enum TypedValue<'a> {
    Int(BigInt),
    Nat(BigUint),
    Mutez(i64),
    Bool(bool),
    String(String),
    Unit,
    Pair(Rc<Self>, Rc<Self>),
    Option(Option<Rc<Self>>),
    List(MichelsonList<Rc<Self>>),
    Set(BTreeSet<Rc<Self>>),
    Map(BTreeMap<Rc<Self>, Rc<Self>>),
    BigMap(BigMap<'a>),
    Or(Or<Rc<Self>, Rc<Self>>),
    Address(Address),
    ChainId(ChainId),
    Contract(Address),
    Bytes(Vec<u8>),
    Key(PublicKey),
    Signature(Signature),
    Lambda(Closure<'a>),
    KeyHash(PublicKeyHash),
    Operation(Box<OperationInfo<'a>>),
    Ticket(Box<Ticket<'a>>),
    Timestamp(BigInt),
    #[cfg(feature = "bls")]
    Bls12381Fr(bls::Fr),
    // G1 and G2 are a bit too large to lug them about on-stack
    #[cfg(feature = "bls")]
    Bls12381G1(Box<bls::G1>),
    #[cfg(feature = "bls")]
    Bls12381G2(Box<bls::G2>),
}

impl<'a> IntoMicheline<'a> for TypedValue<'a> {
    fn into_micheline_optimized_legacy(self, arena: &'a Arena<Micheline<'a>>) -> Micheline<'a> {
        use Micheline as V;
        use TypedValue as TV;
        let go = |x: Self| x.into_micheline_optimized_legacy(arena);
        let go_rc = |x: Rc<Self>| go(TypedValue::unwrap_rc(x));
        let option_into_micheline = |x: Option<Self>| match x {
            None => V::prim0(Prim::None),
            Some(x) => V::prim1(arena, Prim::Some, go(x)),
        };
        match self {
            TV::Int(i) => V::Int(i),
            TV::Nat(u) => V::Int(u.into()),
            TV::Mutez(u) => V::Int(u.into()),
            TV::Bool(true) => V::prim0(Prim::True),
            TV::Bool(false) => V::prim0(Prim::False),
            TV::String(s) => V::String(s),
            TV::Unit => V::prim0(Prim::Unit),
            // This transformation for pairs deviates from the optimized representation of the
            // reference implementation, because reference implementation optimizes the size of combs
            // and uses an untyped representation that is the shortest.
            TV::Pair(l, r) => V::prim2(arena, Prim::Pair, go_rc(l), go_rc(r)),
            TV::List(l) => V::Seq(V::alloc_iter(arena, l.into_iter().map(go_rc))),
            TV::Set(s) => V::Seq(V::alloc_iter(arena, s.into_iter().map(go_rc))),
            TV::Map(m) => V::Seq(V::alloc_iter(
                arena,
                m.into_iter()
                    .map(|(key, val)| V::prim2(arena, Prim::Elt, go_rc(key), go_rc(val))),
            )),
            TV::BigMap(m) => match m.content {
                big_map::BigMapContent::InMemory(m) => V::Seq(V::alloc_iter(
                    arena,
                    m.into_iter()
                        .map(|(key, val)| V::prim2(arena, Prim::Elt, go(key), go(val))),
                )),
                big_map::BigMapContent::FromId(m) => {
                    let id_part = V::Int(m.id.value.into());
                    let overlay_empty = m.overlay.is_empty();
                    let map_part = V::Seq(V::alloc_iter(
                        arena,
                        m.overlay.into_iter().map(|(key, val)| {
                            V::prim2(arena, Prim::Elt, go(key), option_into_micheline(val))
                        }),
                    ));
                    if overlay_empty {
                        id_part
                    } else {
                        V::prim2(arena, Prim::Pair, id_part, map_part)
                    }
                }
            },
            TV::Option(x) => option_into_micheline(x.map(TypedValue::unwrap_rc)),
            TV::Or(or) => match or {
                Or::Left(x) => V::prim1(arena, Prim::Left, go_rc(x)),
                Or::Right(x) => V::prim1(arena, Prim::Right, go_rc(x)),
            },
            TV::Address(x) => V::Bytes(x.to_bytes_vec()),
            TV::ChainId(x) => V::Bytes(x.into()),
            TV::Bytes(x) => V::Bytes(x),
            TV::Key(k) => V::Bytes(k.to_bytes().unwrap()),
            TV::Signature(s) => V::Bytes(s.into()),
            TV::Lambda(lam) => lam.into_micheline_optimized_legacy(arena),
            TV::KeyHash(s) => V::Bytes(s.to_bytes().unwrap()),
            TV::Timestamp(s) => V::Int(s),
            TV::Contract(x) => go(TV::Address(x)),
            TV::Operation(operation_info) => match operation_info.operation {
                Operation::TransferTokens(tt) => Micheline::prim3(
                    arena,
                    Prim::Transfer_tokens,
                    go(tt.param),
                    go(TV::Address(tt.destination_address)),
                    go(TV::Mutez(tt.amount)),
                ),
                Operation::SetDelegate(sd) => Micheline::prim1(
                    arena,
                    Prim::Set_delegate,
                    match sd.0 {
                        Some(kh) => V::prim1(arena, Prim::Some, go(TV::KeyHash(kh))),
                        None => V::prim0(Prim::None),
                    },
                ),
                Operation::Emit(em) => Micheline::App(
                    Prim::Emit,
                    Micheline::alloc_seq(
                        arena,
                        [
                            go(em.value),
                            match em.arg_ty {
                                Or::Right(mich) => mich,
                                Or::Left(typ) => typ.into_micheline_optimized_legacy(arena),
                            },
                        ],
                    ),
                    match em.tag {
                        Some(tag) => [Annotation::Field(tag.into_cow())].into(),
                        None => annotations::NO_ANNS,
                    },
                ),
                Operation::CreateContract(cc) => Micheline::App(
                    Prim::Create_contract,
                    Micheline::alloc_seq(
                        arena,
                        [
                            cc.micheline_code.clone(),
                            go(TypedValue::new_option(cc.delegate.map(|x| {
                                TypedValue::Address(Address {
                                    hash: AddressHash::from(x),
                                    entrypoint: Entrypoint::default(),
                                })
                            }))),
                            go(TypedValue::Mutez(cc.amount)),
                            go(cc.storage),
                        ],
                    ),
                    annotations::NO_ANNS,
                ),
            },
            TV::Ticket(t) => go(unwrap_ticket(t.as_ref().clone())),
            #[cfg(feature = "bls")]
            TV::Bls12381Fr(x) => V::Bytes(x.to_bytes().to_vec()),
            #[cfg(feature = "bls")]
            TV::Bls12381G1(x) => V::Bytes(x.to_bytes().to_vec()),
            #[cfg(feature = "bls")]
            TV::Bls12381G2(x) => V::Bytes(x.to_bytes().to_vec()),
        }
    }
}

pub(crate) fn unwrap_ticket(t: Ticket) -> TypedValue {
    use TypedValue as TV;
    TV::new_pair(
        TV::Address(Address {
            hash: t.ticketer,
            entrypoint: Entrypoint::default(),
        }),
        TV::new_pair(t.content, TV::Nat(t.amount)),
    )
}

impl<'a> TypedValue<'a> {
    pub(crate) fn unwrap_rc(rc: Rc<Self>) -> Self {
        Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
    }

    /// Convenience function to construct a new [Self::Pair].
    pub fn new_pair(l: Self, r: Self) -> Self {
        Self::Pair(Rc::new(l), Rc::new(r))
    }

    /// Convenience function to construct a new [Self::Option].
    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Rc::new))
    }

    /// Convenience function to construct a new [Self::Or].
    pub fn new_or(x: Or<Self, Self>) -> Self {
        Self::Or(match x {
            Or::Left(v) => Or::Left(Rc::new(v)),
            Or::Right(v) => Or::Right(Rc::new(v)),
        })
    }

    /// Convenience function to construct a new [Self::Operation]. Allocates a new [Box].
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
    pub fn nat(n: u64) -> Self {
        Self::Nat(n.into())
    }

    /// Helper for more easily constructing `Timestamp` variant with literals. Mostly
    /// useful in tests.
    pub fn timestamp(n: impl Into<BigInt>) -> Self {
        Self::Timestamp(n.into())
    }

    /// Convenience function to construct a new [Self::Ticket]. Allocates a new [Box].
    pub fn new_ticket(t: Ticket<'a>) -> Self {
        Self::Ticket(Box::new(t))
    }

    /// Convenience function to construct a new [Self::Bls12381G1]. Allocates a new [Box].
    #[cfg(feature = "bls")]
    pub fn new_bls12381_g1(x: bls::G1) -> Self {
        Self::Bls12381G1(Box::new(x))
    }

    /// Convenience function to construct a new [Self::Bls12381G2]. Allocates a new [Box].
    #[cfg(feature = "bls")]
    pub fn new_bls12381_g2(x: bls::G2) -> Self {
        Self::Bls12381G2(Box::new(x))
    }
}

/// Enum representing typechecked Michelson instructions. Some instructions may
/// be applied to different input stacks, for those cases the variant carries a
/// enum specifying the particular version of the instruction (here called
/// "overload"). See [overloads].
///
/// The name of the variant corresponds to the name of the instruction, but with
/// UPPER_SNAKE_CASE converted to PascalCase.
#[derive(Debug, Eq, PartialEq, Clone)]
#[allow(missing_docs)]
pub enum Instruction<'a> {
    Add(overloads::Add),
    Sub(overloads::Sub),
    Mul(overloads::Mul),
    EDiv(overloads::EDiv),
    Neg(overloads::Neg),
    Lsl(overloads::Lsl),
    Lsr(overloads::Lsr),
    SubMutez,
    Dip(Option<u16>, Vec<Self>),
    Drop(Option<u16>),
    Dup(Option<u16>),
    Dig(u16),
    Dug(u16),
    Gt,
    Ge,
    Eq,
    Neq,
    Lt,
    Le,
    If(Vec<Self>, Vec<Self>),
    IfNone(Vec<Self>, Vec<Self>),
    Int(overloads::Int),
    Nat,
    Bytes(overloads::Bytes),
    Abs,
    IsNat,
    Loop(Vec<Self>),
    Push(TypedValue<'a>),
    Swap,
    Failwith(Type),
    Never,
    Unit,
    Car,
    Cdr,
    Pair,
    PairN(u16),
    /// `ISome` because `Some` is already taken
    ISome,
    None,
    Compare,
    Amount,
    Nil,
    EmptySet,
    EmptyMap,
    EmptyBigMap(Type, Type),
    Mem(overloads::Mem),
    Get(overloads::Get),
    GetN(u16),
    Update(overloads::Update),
    GetAndUpdate(overloads::GetAndUpdate),
    Concat(overloads::Concat),
    Size(overloads::Size),
    UpdateN(u16),
    Seq(Vec<Self>),
    Unpair,
    UnpairN(u16),
    Cons,
    And(overloads::And),
    Or(overloads::Or),
    Xor(overloads::Xor),
    Not(overloads::Not),
    IfCons(Vec<Self>, Vec<Self>),
    Iter(overloads::Iter, Vec<Self>),
    IfLeft(Vec<Self>, Vec<Self>),
    ChainId,
    /// `ISelf` because `Self` is a reserved keyword
    ISelf(Entrypoint),
    Pack,
    Unpack(Type),
    CheckSignature,
    TransferTokens,
    SetDelegate,
    Address,
    Slice(overloads::Slice),
    Left,
    Right,
    Lambda(Lambda<'a>),
    Exec,
    Ticket(Type),
    HashKey,
    Apply {
        arg_ty: Type,
    },
    ReadTicket,
    SplitTicket,
    JoinTickets,
    LoopLeft(Vec<Self>),
    Blake2b,
    Keccak,
    Sha256,
    Sha3,
    Sha512,
    Balance,
    Level,
    MinBlockTime,
    SelfAddress,
    Sender,
    Source,
    Now,
    ImplicitAccount,
    IsImplicitAccount,
    TotalVotingPower,
    VotingPower,
    /// Here entrypoint is not an optional value because explicit default entrypoints are forbidden
    /// in concrete syntax, so we can assume that if the entrypoint is the default entrypoint, then
    /// no explicit entrypoint was specified in the instruction.
    Contract(Type, Entrypoint),
    #[cfg(feature = "bls")]
    PairingCheck,
    Emit {
        tag: Option<FieldAnnotation<'a>>,
        arg_ty: Or<Type, Micheline<'a>>,
    },
    CreateContract(Rc<ContractScript<'a>>, &'a Micheline<'a>),
    Map(overloads::Map, Vec<Self>),
    IView {
        name: String,
        return_type: Type,
    },
}

/// An untyped view, as it appears in a script
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct View<'a> {
    /// Input type of the view
    pub input_type: Type,
    /// Output type of the view
    pub output_type: Type,
    /// Code of the view
    pub code: Micheline<'a>,
}

/// A full typechecked contract script.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractScript<'a> {
    /// Parameter type. Corresponds to the script's `parameter` field.
    pub parameter: Type,
    /// Storage type. Corresponds to the script's `storage` field.
    pub storage: Type,
    /// Script code. Corresponds to the script's `code` field.
    pub code: Instruction<'a>,
    /// Script entrypoints.
    pub annotations: HashMap<FieldAnnotation<'a>, (Vec<Direction>, Type)>,
    /// Views. Corresponds to the script's `view` fields.
    pub views: HashMap<String, View<'a>>,
}

/// Enum representing each layer to achieve a given entrypoint
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Direction {
    /// Left
    Left,
    /// Right
    Right,
}

/// A structure mapping from entrypoints to their types. This is simply an alias
/// for a [HashMap].
pub type Entrypoints = HashMap<Entrypoint, Type>;

#[cfg(test)]
#[allow(missing_docs)]
pub mod test_strategies {
    use proptest::prelude::*;

    use crate::{gas::Gas, typechecker::type_props::TypeProperty};

    use super::*;

    /// Generates any type except lambda, big_map, contract, never and operation.
    fn input_ty() -> impl Strategy<Value = Type> {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6755
        // Produce all types.
        use Type::*;
        #[cfg(feature = "bls")]
        let prim = prop_oneof![
            // Cases that we want to see in tests in the first place - go first
            Just(Int),
            Just(Nat),
            Just(Mutez),
            Just(Timestamp),
            Just(String),
            Just(Bytes),
            Just(Unit),
            Just(Bool),
            Just(Address),
            Just(ChainId),
            Just(Key),
            Just(Signature),
            Just(KeyHash),
            Just(Bls12381Fr),
            Just(Bls12381G1),
            Just(Bls12381G2),
        ];
        #[cfg(not(feature = "bls"))]
        let prim = prop_oneof![
            // Cases that we want to see in tests in the first place - go first
            Just(Int),
            Just(Nat),
            Just(Mutez),
            Just(Timestamp),
            Just(String),
            Just(Bytes),
            Just(Unit),
            Just(Bool),
            Just(Address),
            Just(ChainId),
            Just(Key),
            Just(Signature),
            Just(KeyHash),
        ];
        prim.prop_recursive(5, 16, 2, |inner| {
            prop_oneof![
                inner.clone().prop_map(Type::new_option),
                inner.clone().prop_map(Type::new_list),
                (inner.clone(), inner.clone()).prop_map(|(l, r)| Type::new_pair(l, r)),
                (inner.clone(), inner.clone()).prop_map(|(l, r)| Type::new_or(l, r)),
                inner
                    .clone()
                    .prop_filter("Elt must be comparable", |elt| elt
                        .ensure_prop(&mut Gas::default(), TypeProperty::Comparable)
                        .is_ok())
                    .prop_map(Type::new_set),
                inner
                    .clone()
                    .prop_filter("Payload must be comparable", |payload| payload
                        .ensure_prop(&mut Gas::default(), TypeProperty::Comparable)
                        .is_ok())
                    .prop_map(Type::new_ticket),
                (inner.clone(), inner.clone())
                    .prop_filter("Key must be comparable", |(k, _)| k
                        .ensure_prop(&mut Gas::default(), TypeProperty::Comparable)
                        .is_ok())
                    .prop_map(|(k, v)| Type::new_map(k, v)),
                (inner.clone(), inner)
                    .prop_filter("Key must be comparable", |(k, _)| k
                        .ensure_prop(&mut Gas::default(), TypeProperty::Comparable)
                        .is_ok())
                    .prop_map(|(k, v)| Type::new_big_map(k, v)),
            ]
        })
    }

    #[derive(Debug, Clone)]
    pub struct TypedValueAndType<'a> {
        pub ty: Type,
        pub val: TypedValue<'a>,
    }

    pub fn typed_value_and_type<'a>() -> impl Strategy<Value = TypedValueAndType<'a>> {
        input_ty().prop_flat_map(|ty| {
            (Just(ty.clone()), typed_value_by_type(&ty))
                .prop_map(|(ty, val)| TypedValueAndType { ty, val })
        })
    }

    pub fn typed_value_by_type(t: &Type) -> impl Strategy<Value = TypedValue<'static>> {
        use Type as T;
        use TypedValue as V;

        match t {
            // TODO: https://gitlab.com/tezos/tezos/-/issues/6755
            // Sometimes generate really large numbers (should take at least several `u64`s)
            T::Int => (-100..100i128).prop_map(V::int).boxed(),
            T::Nat => (0..100u64).prop_map(V::nat).boxed(),
            T::Mutez => (0..100i64).prop_map(V::Mutez).boxed(),
            T::Timestamp => (-100i128..100i128).prop_map(V::timestamp).boxed(),
            T::Bool => any::<bool>().prop_map(V::Bool).boxed(),
            // TODO: https://gitlab.com/tezos/tezos/-/issues/6755
            // Allow all Michelson strings
            T::String => "[A-Za-z0-9]".prop_map(V::String).boxed(),
            T::Bytes => prop::collection::vec(any::<u8>(), 0..=3)
                .prop_map(V::Bytes)
                .boxed(),
            T::Unit => Just(V::Unit).boxed(),
            T::Pair(t) => {
                let (lt, rt) = t.as_ref();
                (typed_value_by_type(lt), typed_value_by_type(rt))
                    .prop_map(|(l, r)| V::new_pair(l, r))
                    .boxed()
            }
            T::Ticket(t) => {
                (Just(t.as_ref().clone()), typed_value_by_type(t), (1u64..100u64))
                    .prop_map(|(content_type, content, amount)|
                              V::new_ticket(Ticket {
                                  ticketer: AddressHash::from_base58_check("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap(),
                                  content_type,
                                  content,
                                  amount: amount.into(),
                              }
                              ))
                    .boxed()
            }
            T::Option(t) => prop_oneof![
                Just(V::new_option(None)),
                typed_value_by_type(t).prop_map(|v| V::new_option(Some(v)))
            ]
            .boxed(),
            T::Or(t) => {
                let (lt, rt) = t.as_ref();
                prop_oneof![
                    typed_value_by_type(lt).prop_map(|v| V::new_or(Or::Left(v))),
                    typed_value_by_type(rt).prop_map(|v| V::new_or(Or::Right(v)))
                ]
                .boxed()
            }
            T::List(t) => prop::collection::vec(typed_value_by_type(t), 0..=3)
                .prop_map(|x| {
                    V::List(MichelsonList::from(
                        x.into_iter().map(Rc::new).collect::<Vec<_>>(),
                    ))
                })
                .boxed(),
            T::Set(elt) => prop::collection::btree_set(typed_value_by_type(elt), 0..=3)
                .prop_map(|set| V::Set(set.into_iter().map(Rc::new).collect()))
                .boxed(),
            T::Map(m) => {
                let (key_ty, val_ty) = m.as_ref();
                prop::collection::btree_map(
                    typed_value_by_type(key_ty),
                    typed_value_by_type(val_ty),
                    0..=3,
                )
                .prop_map(|map| {
                    V::Map(
                        map.into_iter()
                            .map(|(k, v)| (Rc::new(k), Rc::new(v)))
                            .collect(),
                    )
                })
                .boxed()
            }
            // We don't generate all the allowed syntaxes for big maps
            // but only id-free ones because these are the only ones
            // which can be typechecked in any context.
            T::BigMap(m) => {
                let (key_ty, val_ty) = m.as_ref();
                (Just(key_ty.clone()), Just(val_ty.clone()), prop::collection::btree_map(
                    typed_value_by_type(key_ty),
                    typed_value_by_type(val_ty),
                    0..=3,
                ))
                    .prop_map(|(key_type, value_type, map)|
                              {
                                  let content = big_map::BigMapContent::InMemory(map);
                                  V::BigMap(BigMap {
                                      content,
                                      key_type,
                                      value_type,
                                  })}).boxed()
            }
            T::Address => prop_oneof![
                Just(V::Address(
                    Address::from_base58_check("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw").unwrap()
                )),
                Just(V::Address(
                    Address::from_base58_check("tz1SNL5w4RFRbCWRMB4yDWvoRQrPQxZmNzeQ").unwrap()
                )),
                Just(V::Address(
                    Address::from_base58_check("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap()
                )),
                Just(V::Address(
                    Address::from_base58_check("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo").unwrap()
                )),
                Just(V::Address(
                    Address::from_base58_check("sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf").unwrap()
                )),
                Just(V::Address(
                    Address::from_base58_check("sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo").unwrap()
                )),
            ]
            .boxed(),
            T::ChainId => prop_oneof![
                Just(V::ChainId(
                    ChainId::from_base58_check("NetXgtSLGNJvNye").unwrap()
                )),
                Just(V::ChainId(
                    ChainId::from_base58_check("NetXjD3HPJJjmcd").unwrap()
                )),
            ]
            .boxed(),
            T::Key => prop_oneof![
                Just(V::Key(
                    PublicKey::from_b58check("edpkupxHveP7SFVnBq4X9Dkad5smzLcSxpRx9tpR7US8DPN5bLPFwu").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("sppk7cdA7Afj8MvuBFrP6KsTLfbM5DtH9GwYaRZwCf5tBVCz6UKGQFR").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("p2pk68C6tJr7pNLvgBH63K3hBVoztCPCA36zcWhXFUGywQJTjYBfpxk").unwrap()
                )),
                Just(V::Key(
                    PublicKey::from_b58check("BLpk1yoPpFtFF3jGUSn2GrGzgHVcj1cm5o6HTMwiqSjiTNFSJskXFady9nrdhoZzrG6ybXiTSK5G").unwrap()
                )),
            ]
                .boxed(),
            T::Signature =>
                Just(V::Signature( Signature::from_base58_check("BLsigAmLKnuw12tethjMmotFPaQ6u4XCKrVk6c15dkRXKkjDDjHywbhS3nd4rBT31yrCvvQrS2HntWhDRu7sX8Vvek53zBUwQHqfcHRiVKVj1ehq8CBYs1Z7XW2rkL2XkVNHua4cnvxY7F").unwrap())).boxed(),
            T::KeyHash =>
                Just(V::KeyHash(
                    PublicKeyHash::from_b58check("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw").unwrap()
                )).boxed(),
            #[cfg(feature = "bls")]
            T::Bls12381Fr =>
                Just(V::Bls12381Fr(bls::fr::Fr::from_big_int(&0.into()))).boxed(),
            #[cfg(feature = "bls")]
            T::Bls12381G1 =>
                Just(V::Bls12381G1(Box::new(bls::g1::G1::zero()))).boxed(),
            #[cfg(feature = "bls")]
            T::Bls12381G2 =>
                Just(V::Bls12381G2(Box::new(bls::g2::G2::zero()))).boxed(),
            T::Contract(_) => panic!("Cannot generate typed value for contract"),
            T::Operation => panic!("Cannot generate typed value for operation"),
            T::Lambda(_) => panic!("Cannot generate typed value for lambda"),
            T::Never =>  panic!("Cannot generate typed value for never"),
            // NOTE: if you append clauses here, you likely need to update other generators too
        }
    }
}

#[cfg(test)]
mod test_untypers {
    use proptest::prelude::*;

    use super::*;
    use crate::{ast::test_strategies as TS, context::Ctx, typechecker::typecheck_value};

    proptest! {
        #[test]
        fn value_typecheck_untype_roundtrip(typed in TS::typed_value_and_type()) {
            let arena = Arena::new();
            let mut ctx = Ctx::default();
            let untyped = typed.val.clone().into_micheline_optimized_legacy(&arena);
            let typed_ = typecheck_value(&untyped, &mut ctx, &typed.ty);
            assert_eq!(typed_, Ok(typed.val))
        }
    }

    // We used to have a bug in which a panic was raised in the
    // following case, this test is there to avoid a regression.
    #[test]
    fn test_big_map_without_id() {
        let arena = Arena::new();
        let mut m = BigMap::empty(Type::Nat, Type::Unit);
        m.update(TypedValue::Nat(0u32.into()), None);
        TypedValue::BigMap(m).into_micheline_optimized_legacy(&arena);
    }
}
