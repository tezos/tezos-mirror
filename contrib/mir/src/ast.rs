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

use crate::{
    gas::{Gas, OutOfGas},
    lexer::Prim,
};

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
/// Indirection that owns a shared pair of values via `Rc<(T, T)>`, with
/// the inner Rc held in an [`Option`] so the iterative [`Type::drop`]
/// can extract it via [`Option::take`] without by move destructuring.
/// Outside of that drop the option is always `Some`.
#[derive(Debug)]
pub struct PairBox<T> {
    inner: Option<Rc<(T, T)>>,
}

impl<T> PairBox<T> {
    /// Build a fresh pair box around `(l, r)`.
    pub fn new(l: T, r: T) -> Self {
        PairBox {
            inner: Some(Rc::new((l, r))),
        }
    }
    /// View the inner pair.
    pub fn as_ref(&self) -> &(T, T) {
        self.inner
            .as_ref()
            .expect("PairBox always Some outside iterative Drop")
            .as_ref()
    }
    /// Take the Rc out, leaving `None`. Used by the iterative drop.
    fn take(&mut self) -> Option<Rc<(T, T)>> {
        self.inner.take()
    }
}

impl<T: Clone> PairBox<T> {
    /// Mutable access to the inner pair, cloning the Rc body if shared.
    pub fn make_mut(&mut self) -> &mut (T, T) {
        let rc = self
            .inner
            .as_mut()
            .expect("PairBox always Some outside iterative Drop");
        Rc::make_mut(rc)
    }
}

impl<T> Clone for PairBox<T> {
    fn clone(&self) -> Self {
        PairBox {
            inner: self.inner.clone(),
        }
    }
}

impl<T: PartialEq> PartialEq for PairBox<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Eq> Eq for PairBox<T> {}

impl<T> std::ops::Deref for PairBox<T> {
    type Target = (T, T);
    fn deref(&self) -> &(T, T) {
        self.as_ref()
    }
}

/// Indirection that owns a shared single value via `Rc<T>`. Same role
/// as [`PairBox`] but for single child variants.
#[derive(Debug)]
pub struct SingleBox<T> {
    inner: Option<Rc<T>>,
}

impl<T> SingleBox<T> {
    /// Build a fresh single box around `v`.
    pub fn new(v: T) -> Self {
        SingleBox {
            inner: Some(Rc::new(v)),
        }
    }
    /// Wrap an existing Rc into a SingleBox without cloning the contents.
    pub fn from_rc(rc: Rc<T>) -> Self {
        SingleBox { inner: Some(rc) }
    }
    /// View the inner value.
    pub fn as_ref(&self) -> &T {
        self.inner
            .as_ref()
            .expect("SingleBox always Some outside iterative Drop")
            .as_ref()
    }
    /// Clone the inner Rc (cheap, just bumps refcount).
    pub fn clone_rc(&self) -> Rc<T> {
        Rc::clone(
            self.inner
                .as_ref()
                .expect("SingleBox always Some outside iterative Drop"),
        )
    }
    /// Take the Rc out, leaving `None`. Used by the iterative drop.
    fn take(&mut self) -> Option<Rc<T>> {
        self.inner.take()
    }
}

impl<T> Clone for SingleBox<T> {
    fn clone(&self) -> Self {
        SingleBox {
            inner: self.inner.clone(),
        }
    }
}

impl<T: PartialEq> PartialEq for SingleBox<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Eq> Eq for SingleBox<T> {}

impl<T> std::ops::Deref for SingleBox<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<T> From<Rc<T>> for SingleBox<T> {
    fn from(rc: Rc<T>) -> Self {
        SingleBox { inner: Some(rc) }
    }
}

impl<T> From<T> for SingleBox<T> {
    fn from(v: T) -> Self {
        SingleBox::new(v)
    }
}

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
    Pair(PairBox<Type>),
    Option(SingleBox<Type>),
    List(SingleBox<Type>),
    Operation,
    Set(SingleBox<Type>),
    Map(PairBox<Type>),
    BigMap(PairBox<Type>),
    Or(PairBox<Type>),
    Contract(SingleBox<Type>),
    Address,
    ChainId,
    Bytes,
    Key,
    Signature,
    KeyHash,
    Lambda(PairBox<Type>),
    Ticket(SingleBox<Type>),
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
        Self::Pair(PairBox::new(l, r))
    }

    /// Convenience function to construct a new [Self::Option]. Allocates a new [Rc].
    pub fn new_option(x: Self) -> Self {
        Self::Option(SingleBox::new(x))
    }

    /// Convenience function to construct a new [Self::List]. Allocates a new [Rc].
    pub fn new_list(x: Self) -> Self {
        Self::List(SingleBox::new(x))
    }

    /// Convenience function to construct a new [Self::Set]. Allocates a new [Rc].
    pub fn new_set(v: Self) -> Self {
        Self::Set(SingleBox::new(v))
    }

    /// Convenience function to construct a new [Self::Map]. Allocates a new [Rc].
    pub fn new_map(k: Self, v: Self) -> Self {
        Self::Map(PairBox::new(k, v))
    }

    /// Convenience function to construct a new [Self::BigMap]. Allocates a new [Rc].
    pub fn new_big_map(k: Self, v: Self) -> Self {
        Self::BigMap(PairBox::new(k, v))
    }

    /// Convenience function to construct a new [Self::Or]. Allocates a new [Rc].
    pub fn new_or(l: Self, r: Self) -> Self {
        Self::Or(PairBox::new(l, r))
    }

    /// Convenience function to construct a new [Self::Contract]. Allocates a new [Rc].
    pub fn new_contract(ty: Self) -> Self {
        Self::Contract(SingleBox::new(ty))
    }

    /// Convenience function to construct a new [Self::Ticket]. Allocates a new [Rc].
    pub fn new_ticket(ty: Self) -> Self {
        Self::Ticket(SingleBox::new(ty))
    }

    /// Convenience function to construct a new [Self::Lambda]. Allocates a new [Rc].
    pub fn new_lambda(ty1: Self, ty2: Self) -> Self {
        Self::Lambda(PairBox::new(ty1, ty2))
    }
}

/// Walk an owned tree iteratively. Used by the three manual `Drop` /
/// drain impls below: each extracts the node's children into a heap
/// worklist, leaving the leftover node trivial; this driver then keeps
/// pulling from the worklist until empty so the recursion happens on
/// the heap rather than the WASM call stack.
fn drain_iteratively<T>(root: &mut T, mut extract: impl FnMut(&mut T, &mut Vec<T>)) {
    let mut stack: Vec<T> = Vec::new();
    extract(root, &mut stack);
    while let Some(mut node) = stack.pop() {
        extract(&mut node, &mut stack);
    }
}


/// Manual `Drop` to avoid the recursive destructor that would otherwise
/// blow the call stack on a deeply nested type. Walks an explicit
/// worklist and uses [`Option::take`] on the wrapper's inner `Rc` so the
/// leftover `Type` drops trivially without re entering this function.
impl Drop for Type {
    fn drop(&mut self) {
        drain_iteratively(self, extract_type_children);
    }
}

fn extract_type_children(node: &mut Type, stack: &mut Vec<Type>) {
    match node {
        Type::Pair(rcb)
        | Type::Or(rcb)
        | Type::Lambda(rcb)
        | Type::Map(rcb)
        | Type::BigMap(rcb) => {
            if let Some(rc) = rcb.take() {
                if let Ok((l, r)) = Rc::try_unwrap(rc) {
                    stack.push(l);
                    stack.push(r);
                }
            }
        }
        Type::Option(sb)
        | Type::List(sb)
        | Type::Set(sb)
        | Type::Contract(sb)
        | Type::Ticket(sb) => {
            if let Some(rc) = sb.take() {
                if let Ok(inner) = Rc::try_unwrap(rc) {
                    stack.push(inner);
                }
            }
        }
        _ => {}
    }
}

impl<'a> IntoMicheline<'a> for &'_ Type {
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a Arena<Micheline<'a>>,
        gas: &mut Gas,
    ) -> Result<Micheline<'a>, OutOfGas> {
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
            Nat => Micheline::prim0(Prim::nat, gas),
            Int => Micheline::prim0(Prim::int, gas),
            Bool => Micheline::prim0(Prim::bool, gas),
            Mutez => Micheline::prim0(Prim::mutez, gas),
            String => Micheline::prim0(Prim::string, gas),
            Unit => Micheline::prim0(Prim::unit, gas),
            Operation => Micheline::prim0(Prim::operation, gas),
            Address => Micheline::prim0(Prim::address, gas),
            ChainId => Micheline::prim0(Prim::chain_id, gas),
            Bytes => Micheline::prim0(Prim::bytes, gas),
            Key => Micheline::prim0(Prim::key, gas),
            Signature => Micheline::prim0(Prim::signature, gas),
            Timestamp => Micheline::prim0(Prim::timestamp, gas),
            KeyHash => Micheline::prim0(Prim::key_hash, gas),
            Never => Micheline::prim0(Prim::never, gas),
            #[cfg(feature = "bls")]
            Bls12381Fr => Micheline::prim0(Prim::bls12_381_fr, gas),
            #[cfg(feature = "bls")]
            Bls12381G1 => Micheline::prim0(Prim::bls12_381_g1, gas),
            #[cfg(feature = "bls")]
            Bls12381G2 => Micheline::prim0(Prim::bls12_381_g2, gas),

            Option(x) => Micheline::prim1(
                arena,
                Prim::option,
                x.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            List(x) => Micheline::prim1(
                arena,
                Prim::list,
                x.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            Set(x) => Micheline::prim1(
                arena,
                Prim::set,
                x.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            Contract(x) => Micheline::prim1(
                arena,
                Prim::contract,
                x.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            Ticket(x) => Micheline::prim1(
                arena,
                Prim::ticket,
                x.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),

            Pair(_) => {
                let args = Micheline::alloc_iter(
                    arena,
                    LinearizePairIter(Some(self))
                        .map(|x| x.into_micheline_optimized_legacy(arena, gas)),
                )?;
                Micheline::prim_n(Prim::pair, args, gas)
            }
            Map(x) => Micheline::prim2(
                arena,
                Prim::map,
                x.0.into_micheline_optimized_legacy(arena, gas)?,
                x.1.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            BigMap(x) => Micheline::prim2(
                arena,
                Prim::big_map,
                x.0.into_micheline_optimized_legacy(arena, gas)?,
                x.1.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            Or(x) => Micheline::prim2(
                arena,
                Prim::or,
                x.0.into_micheline_optimized_legacy(arena, gas)?,
                x.1.into_micheline_optimized_legacy(arena, gas)?,
                gas,
            ),
            Lambda(x) => Micheline::prim2(
                arena,
                Prim::lambda,
                x.0.into_micheline_optimized_legacy(arena, gas)?,
                x.1.into_micheline_optimized_legacy(arena, gas)?,
                gas,
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
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a Arena<Micheline<'a>>,
        gas: &mut Gas,
    ) -> Result<Micheline<'a>, OutOfGas> {
        use Micheline as V;
        use TypedValue as TV;
        let go = |x: Self, gas: &mut Gas| x.into_micheline_optimized_legacy(arena, gas);
        let go_rc = |x: Rc<Self>, gas: &mut Gas| go(TypedValue::unwrap_rc(x), gas);
        let option_into_micheline = |x: Option<Self>, gas: &mut Gas| match x {
            None => V::prim0(Prim::None, gas),
            Some(x) => V::prim1(arena, Prim::Some, go(x, gas)?, gas),
        };

        match self {
            TV::Int(i) => V::int(i, gas),
            TV::Nat(u) => V::int(u.into(), gas),
            TV::Mutez(u) => V::int(u.into(), gas),
            TV::Bool(true) => V::prim0(Prim::True, gas),
            TV::Bool(false) => V::prim0(Prim::False, gas),
            TV::String(s) => V::string(s, gas),
            TV::Unit => V::prim0(Prim::Unit, gas),
            // This transformation for pairs deviates from the optimized representation of the
            // reference implementation, because reference implementation optimizes the size of combs
            // and uses an untyped representation that is the shortest.
            TV::Pair(l, r) => V::prim2(arena, Prim::Pair, go_rc(l, gas)?, go_rc(r, gas)?, gas),
            TV::List(l) => {
                let args = V::alloc_iter(arena, l.into_iter().map(|x| go_rc(x, gas)))?;
                V::seq(args, gas)
            }
            TV::Set(s) => {
                let args = V::alloc_iter(arena, s.into_iter().map(|x| go_rc(x, gas)))?;
                V::seq(args, gas)
            }
            TV::Map(m) => {
                let args = V::alloc_iter(
                    arena,
                    m.into_iter().map(|(key, val)| {
                        V::prim2(arena, Prim::Elt, go_rc(key, gas)?, go_rc(val, gas)?, gas)
                    }),
                )?;
                V::seq(args, gas)
            }
            TV::BigMap(m) => match m.content {
                big_map::BigMapContent::InMemory(m) => {
                    let args = V::alloc_iter(
                        arena,
                        m.into_iter().map(|(key, val)| {
                            V::prim2(arena, Prim::Elt, go(key, gas)?, go(val, gas)?, gas)
                        }),
                    )?;
                    V::seq(args, gas)
                }
                big_map::BigMapContent::FromId(m) => {
                    let id_part = V::int(m.id.value.into(), gas)?;
                    let overlay_empty = m.overlay.is_empty();
                    if overlay_empty {
                        Ok(id_part)
                    } else {
                        let args = V::alloc_iter(
                            arena,
                            m.overlay.into_iter().map(|(key, val)| {
                                V::prim2(
                                    arena,
                                    Prim::Elt,
                                    go(key, gas)?,
                                    option_into_micheline(val, gas)?,
                                    gas,
                                )
                            }),
                        )?;
                        let map_part = V::seq(args, gas)?;
                        V::prim2(arena, Prim::Pair, id_part, map_part, gas)
                    }
                }
            },
            TV::Option(x) => option_into_micheline(x.map(TypedValue::unwrap_rc), gas),
            TV::Or(or) => match or {
                Or::Left(x) => V::prim1(arena, Prim::Left, go_rc(x, gas)?, gas),
                Or::Right(x) => V::prim1(arena, Prim::Right, go_rc(x, gas)?, gas),
            },
            TV::Address(x) => V::bytes(x.to_bytes_vec(), gas),
            TV::ChainId(x) => V::bytes(x.into(), gas),
            TV::Bytes(x) => V::bytes(x, gas),
            // to_bytes() cannot fail for a well-formed key.
            TV::Key(k) => V::bytes(k.to_bytes().unwrap(), gas),
            TV::Signature(s) => V::bytes(s.into(), gas),
            TV::Lambda(lam) => lam.into_micheline_optimized_legacy(arena, gas),
            // to_bytes() cannot fail for a well-formed key hash.
            TV::KeyHash(s) => V::bytes(s.to_bytes().unwrap(), gas),
            TV::Timestamp(s) => V::int(s, gas),
            TV::Contract(x) => go(TV::Address(x), gas),
            TV::Operation(operation_info) => match operation_info.operation {
                Operation::TransferTokens(tt) => Micheline::prim3(
                    arena,
                    Prim::Transfer_tokens,
                    go(tt.param, gas)?,
                    go(TV::Address(tt.destination_address), gas)?,
                    go(TV::Mutez(tt.amount), gas)?,
                    gas,
                ),
                Operation::SetDelegate(sd) => Micheline::prim1(
                    arena,
                    Prim::Set_delegate,
                    match sd.0 {
                        Some(kh) => V::prim1(arena, Prim::Some, go(TV::KeyHash(kh), gas)?, gas)?,
                        None => V::prim0(Prim::None, gas)?,
                    },
                    gas,
                ),
                Operation::Emit(em) => {
                    let mut node = Micheline::prim_n_arr(
                        arena,
                        Prim::Emit,
                        [
                            go(em.value, gas)?,
                            match em.arg_ty {
                                Or::Right(mich) => mich,
                                Or::Left(typ) => typ.into_micheline_optimized_legacy(arena, gas)?,
                            },
                        ],
                        gas,
                    )?;
                    if let Some(tag) = em.tag {
                        node.annotate(Annotation::Field(tag.into_cow()), gas)?;
                    }
                    Ok(node)
                }
                Operation::CreateContract(cc) => Micheline::prim_n_arr(
                    arena,
                    Prim::Create_contract,
                    [
                        cc.micheline_code.clone(),
                        go(
                            TypedValue::new_option(cc.delegate.map(|x| {
                                TypedValue::Address(Address {
                                    hash: AddressHash::from(x),
                                    entrypoint: Entrypoint::default(),
                                })
                            })),
                            gas,
                        )?,
                        go(TypedValue::Mutez(cc.amount), gas)?,
                        go(cc.storage, gas)?,
                    ],
                    gas,
                ),
            },
            TV::Ticket(t) => go(unwrap_ticket(t.as_ref().clone()), gas),
            #[cfg(feature = "bls")]
            TV::Bls12381Fr(x) => V::bytes(x.to_bytes().to_vec(), gas),
            #[cfg(feature = "bls")]
            TV::Bls12381G1(x) => V::bytes(x.to_bytes().to_vec(), gas),
            #[cfg(feature = "bls")]
            TV::Bls12381G2(x) => V::bytes(x.to_bytes().to_vec(), gas),
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

    /// Convenience function to construct a new [Self::Pair] from Rc values.
    pub fn new_pair_rc(l: Rc<Self>, r: Rc<Self>) -> Self {
        Self::Pair(l, r)
    }

    /// Convenience function to construct a new [Self::Option].
    pub fn new_option(x: Option<Self>) -> Self {
        Self::Option(x.map(Rc::new))
    }

    /// Convenience function to construct a new [Self::Option] from Rc values.
    pub fn new_option_rc(x: Option<Rc<Self>>) -> Self {
        Self::Option(x)
    }

    /// Convenience function to construct a new [Self::Or].
    pub fn new_or(x: Or<Self, Self>) -> Self {
        Self::Or(match x {
            Or::Left(v) => Or::Left(Rc::new(v)),
            Or::Right(v) => Or::Right(Rc::new(v)),
        })
    }

    /// Convenience function to construct a new [Self::Or] from Rc values.
    pub fn new_or_rc(x: Or<Rc<Self>, Rc<Self>>) -> Self {
        Self::Or(x)
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

/// Drains nested `Rc<TypedValue>` children iteratively when dropping a
/// potentially deep value (e.g. a comb of `Pair (Pair (... (Pair Int Int)))`),
/// so the recursive `Rc<TypedValue>` destructor chain does not blow the
/// WASM stack.
///
/// Not an `impl Drop` on [`TypedValue`] because that would forbid every
/// by move destructure of its variants throughout the codebase. Called from
/// [`Instruction`]'s `Drop` (for `PUSH` constants) and by callers that build
/// deep values explicitly before drop.
///
/// Note: runtime-built deep values (a `PAIR`/`CONS` comb on the value stack,
/// values popped/returned or left on the stack at interpreter teardown) are
/// not yet drained by any production site, so they still recurse on the
/// default destructor. Closing that gap — wiring this in at the release sites
/// or giving `TypedValue` an iterative `Drop` — is tracked in Linear L2-1446.
///
/// `pub(crate)`: the sentinel `replace` it performs assumes the caller is
/// about to drop `v`, so it must not be exposed to callers that might hold the
/// value aliased.
pub(crate) fn drain_deep_typed_value(v: &mut TypedValue<'_>) {
    drain_iteratively(v, extract_tv_children);
}

fn extract_tv_children<'a>(node: &mut TypedValue<'a>, stack: &mut Vec<TypedValue<'a>>) {
    use std::mem::{replace, take};
    use TypedValue as TV;
    let push_rc = |rc: Rc<TV<'a>>, stack: &mut Vec<TV<'a>>| {
        if let Ok(v) = Rc::try_unwrap(rc) {
            stack.push(v);
        }
    };
    match node {
        TV::Pair(l, r) => {
            let old_l = replace(l, Rc::new(TV::Unit));
            let old_r = replace(r, Rc::new(TV::Unit));
            push_rc(old_l, stack);
            push_rc(old_r, stack);
        }
        TV::Option(opt) => {
            if let Some(rc) = opt.take() {
                push_rc(rc, stack);
            }
        }
        TV::Or(or) => {
            let old = replace(or, Or::Left(Rc::new(TV::Unit)));
            let rc = match old {
                Or::Left(r) | Or::Right(r) => r,
            };
            push_rc(rc, stack);
        }
        TV::List(l) => {
            for rc in take(l) {
                push_rc(rc, stack);
            }
        }
        TV::Set(s) => {
            for rc in take(s) {
                push_rc(rc, stack);
            }
        }
        TV::Map(m) => {
            for (k, v) in take(m) {
                push_rc(k, stack);
                push_rc(v, stack);
            }
        }
        TV::Lambda(closure) => {
            // Walk the `Closure::Apply` spine iteratively so the
            // `Box<Closure>` chain (a deep `APPLY` chain) does not recurse on
            // drop, pushing each captured `arg_val` (a possibly-deep value)
            // onto the worklist. The final `Closure::Lambda` drops trivially:
            // its `Type` fields and `Rc<[Instruction]>` code have their own
            // iterative `Drop`.
            let dummy = Closure::Lambda(Lambda::Lambda {
                micheline_code: Micheline::Seq(&[]),
                code: Vec::new().into(),
            });
            let mut cur = replace(closure, dummy);
            while let Closure::Apply {
                arg_val,
                closure: inner,
                ..
            } = cur
            {
                stack.push(*arg_val);
                cur = *inner;
            }
        }
        TV::Ticket(t) => {
            // Move the ticket payload out so it is drained via the worklist
            // rather than recursing through the default `TypedValue` destructor.
            stack.push(replace(&mut t.content, TV::Unit));
        }
        TV::Operation(info) => {
            // Operations carry `TypedValue` payloads (transfer parameter, emit
            // value, originated storage) that may be deep; drain them.
            match &mut info.operation {
                Operation::TransferTokens(tt) => stack.push(replace(&mut tt.param, TV::Unit)),
                Operation::Emit(e) => stack.push(replace(&mut e.value, TV::Unit)),
                Operation::CreateContract(c) => stack.push(replace(&mut c.storage, TV::Unit)),
                Operation::SetDelegate(_) => {}
            }
        }
        TV::BigMap(m) => {
            // The in-memory keys/values (and the lazy-storage overlay diff) are
            // owned `TypedValue`s that may be deep; drain them.
            match &mut m.content {
                big_map::BigMapContent::InMemory(map) => {
                    for (k, v) in take(map) {
                        stack.push(k);
                        stack.push(v);
                    }
                }
                big_map::BigMapContent::FromId(from_id) => {
                    for (k, v) in take(&mut from_id.overlay) {
                        stack.push(k);
                        if let Some(v) = v {
                            stack.push(v);
                        }
                    }
                }
            }
        }
        _ => {}
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
    /// `PUSH ty v`. The literal is wrapped in [`Rc`] so the hot path of the
    /// interpreter is a refcount bump rather than a deep clone of the value
    /// (which can be expensive for big-num literals like `PUSH nat 10^18`).
    Push(Rc<TypedValue<'a>>),
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

/// Manual `Drop` to avoid the recursive destructor on deeply nested
/// instruction trees (DIP, IF, IF_*, LOOP, LOOP_LEFT, ITER, MAP, SEQ).
/// Uses an explicit worklist; nested bodies are extracted via
/// [`std::mem::take`] on each `Vec<Self>` field so the leftover variant
/// drops trivially without re entering this function.
impl<'a> Drop for Instruction<'a> {
    fn drop(&mut self) {
        drain_iteratively(self, extract_instr_children);
    }
}

fn extract_instr_children<'a>(
    node: &mut Instruction<'a>,
    stack: &mut Vec<Instruction<'a>>,
) {
    use std::mem::{replace, take};
    match node {
        Instruction::Dip(_, body)
        | Instruction::Loop(body)
        | Instruction::LoopLeft(body)
        | Instruction::Iter(_, body)
        | Instruction::Map(_, body)
        | Instruction::Seq(body) => {
            for instr in take(body) {
                stack.push(instr);
            }
        }
        Instruction::If(t, f)
        | Instruction::IfNone(t, f)
        | Instruction::IfCons(t, f)
        | Instruction::IfLeft(t, f) => {
            for instr in take(t) {
                stack.push(instr);
            }
            for instr in take(f) {
                stack.push(instr);
            }
        }
        Instruction::Push(rc) => {
            // The pushed constant may be a deep value. It is a `TypedValue`,
            // not an `Instruction`, so it cannot go on this worklist; drain it
            // with its own iterative routine instead (when solely owned here).
            let old = replace(rc, Rc::new(TypedValue::Unit));
            if let Ok(mut tv) = Rc::try_unwrap(old) {
                drain_deep_typed_value(&mut tv);
            }
        }
        Instruction::Lambda(lambda) => {
            // Push the lambda body's instructions onto the worklist so a
            // lexically nested `LAMBDA { LAMBDA { … } }` does not recurse one
            // frame per nesting level through the body `Rc<[Instruction]>`.
            let code = match lambda {
                Lambda::Lambda { code, .. } | Lambda::LambdaRec { code, .. } => code,
            };
            if let Some(slice) = Rc::get_mut(code) {
                for slot in slice.iter_mut() {
                    stack.push(replace(slot, Instruction::Drop(None)));
                }
            }
        }
        _ => {}
    }
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
            let untyped = typed.val.clone().into_micheline_optimized_legacy(&arena, &mut ctx.gas).unwrap();
            let typed_ = typecheck_value(&untyped, &mut ctx, &typed.ty);
            assert_eq!(typed_, Ok(typed.val))
        }
    }

    // We used to have a bug in which a panic was raised in the
    // following case, this test is there to avoid a regression.
    #[test]
    fn test_big_map_without_id() {
        let arena = Arena::new();
        let mut gas = Gas::default();
        let mut m = BigMap::empty(Type::Nat, Type::Unit);
        m.update(TypedValue::Nat(0u32.into()), None);
        TypedValue::BigMap(m)
            .into_micheline_optimized_legacy(&arena, &mut gas)
            .unwrap();
    }

    // Test that converting a deeply Rc-shared TypedValue (constant_dup-shape)
    // to Micheline returns OutOfGas with limited gas, BEFORE allocating
    // significant memory. This prevents OOM in the WASM runtime.
    #[test]
    fn test_constant_dup_shape_gas_bounded() {
        let arena = Arena::new();
        let mut gas = Gas::default();

        // Build a constant_dup-shape value: (list (list (list unit)))
        // where each inner list is shared via Rc.
        // With depth d and width w, this creates w^d virtual entries but only
        // O(w*d) actual allocations due to Rc sharing.
        let unit_list = Rc::new(TypedValue::List(MichelsonList::from(
            vec![] as Vec<Rc<TypedValue>>
        )));

        // Build (list unit) with 1000 shared references to the same empty list
        let inner_lists: Vec<Rc<TypedValue>> = (0..1000).map(|_| Rc::clone(&unit_list)).collect();
        let inner_list = Rc::new(TypedValue::List(MichelsonList::from(inner_lists)));

        // Build (list (list unit)) with 1000 shared references to the same (list unit)
        let middle_lists: Vec<Rc<TypedValue>> = (0..1000).map(|_| Rc::clone(&inner_list)).collect();
        let middle_list = Rc::new(TypedValue::List(MichelsonList::from(middle_lists)));

        // Build (list (list (list unit))) with 1000 shared references
        let outer_lists: Vec<Rc<TypedValue>> = (0..1000).map(|_| Rc::clone(&middle_list)).collect();
        let outer_list = TypedValue::List(MichelsonList::from(outer_lists));

        // This structure has 1000 * 1000 * 1000 = 1,000,000,000 virtual entries to traverse
        // if unshared, but only 1000 + 1000 + 1000 = 3000 actual Rc nodes.

        // Conversion should fail with OutOfGas before allocating
        // significant memory. The per-node charge (100 milligas each)
        // should be triggered during traversal, not only after.
        let result = outer_list.into_micheline_optimized_legacy(&arena, &mut gas);
        assert_eq!(result, Err(OutOfGas));
    }
}

#[cfg(test)]
mod drop_safety {
    //! Regression tests for the iterative `Drop`/drain on deeply nested
    //! values. Each builds a structure deep enough to overflow a recursive
    //! destructor and runs on a 1 MiB worker thread (the WASM kernel stack
    //! budget); completion of the thread is the assertion.
    use super::*;

    const DEPTH: usize = 100_000;

    fn on_kernel_stack(f: impl FnOnce() + Send + 'static) {
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(f)
            .unwrap()
            .join()
            .expect("worker thread completes without stack overflow");
    }

    #[test]
    fn drain_deep_lambda_apply_spine() {
        // A deep `APPLY` chain nests `Closure::Apply { closure: Box<Closure> }`;
        // the `Box<Closure>` spine must be drained iteratively, not recurse.
        on_kernel_stack(|| {
            let mut closure = Closure::Lambda(Lambda::Lambda {
                micheline_code: Micheline::Seq(&[]),
                code: Vec::new().into(),
            });
            for _ in 0..DEPTH {
                closure = Closure::Apply {
                    arg_ty: Type::Unit,
                    arg_val: Box::new(TypedValue::Unit),
                    closure: Box::new(closure),
                };
            }
            let mut tv = TypedValue::Lambda(closure);
            drain_deep_typed_value(&mut tv);
        });
    }

    #[test]
    fn drain_deep_ticket_content() {
        // The ticket payload (`content: TypedValue`) must be drained, not left
        // to recurse through the default destructor.
        on_kernel_stack(|| {
            let mut content = TypedValue::Unit;
            for _ in 0..DEPTH {
                content = TypedValue::new_pair(TypedValue::Unit, content);
            }
            let ticket = Ticket {
                ticketer: AddressHash::from_base58_check(
                    "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye",
                )
                .unwrap(),
                content_type: Type::Unit,
                content,
                amount: 1u32.into(),
            };
            let mut tv = TypedValue::new_ticket(ticket);
            drain_deep_typed_value(&mut tv);
        });
    }

    #[test]
    fn drop_deep_push_constant() {
        // Dropping `PUSH <deep constant>` must drain the pushed value rather
        // than recurse through its `Rc<TypedValue>` spine.
        on_kernel_stack(|| {
            let mut content = TypedValue::Unit;
            for _ in 0..DEPTH {
                content = TypedValue::new_pair(TypedValue::Unit, content);
            }
            let instr = Instruction::Push(Rc::new(content));
            drop(instr);
        });
    }

    #[test]
    fn drop_deeply_nested_lambda_instruction() {
        // A lexically nested `LAMBDA { LAMBDA { … } }` must drain its body
        // instructions onto the worklist, not recurse one frame per level.
        on_kernel_stack(|| {
            let mut instr = Instruction::Drop(None);
            for _ in 0..DEPTH {
                instr = Instruction::Lambda(Lambda::Lambda {
                    micheline_code: Micheline::Seq(&[]),
                    code: vec![instr].into(),
                });
            }
            drop(instr);
        });
    }

    #[test]
    fn drop_shared_deeply_nested_lambda_instruction() {
        // The common runtime case: the lambda body `Rc` is shared (AST
        // instruction + a pushed lambda value hold the same code). The first
        // owner to drop hits `Rc::get_mut` == None and skips draining; the
        // *last* owner finds each body sole-owned and drains it iteratively.
        // Neither drop may overflow.
        on_kernel_stack(|| {
            let mut instr = Instruction::Drop(None);
            for _ in 0..DEPTH {
                instr = Instruction::Lambda(Lambda::Lambda {
                    micheline_code: Micheline::Seq(&[]),
                    code: vec![instr].into(),
                });
            }
            let shared = instr.clone(); // top code Rc now refcount 2
            drop(instr); // get_mut == None at the shared level: skipped
            drop(shared); // last owner: drains iteratively
        });
    }

    fn deep_pair(depth: usize) -> TypedValue<'static> {
        let mut v = TypedValue::Unit;
        for _ in 0..depth {
            v = TypedValue::new_pair(TypedValue::Unit, v);
        }
        v
    }

    #[test]
    fn drain_deep_operation_value() {
        // An operation carries a `TypedValue` payload (here an EMIT value) that
        // may be deep; it must be drained, not recurse on the default destructor.
        on_kernel_stack(|| {
            let mut tv = TypedValue::Operation(Box::new(OperationInfo {
                operation: Operation::Emit(Emit {
                    tag: None,
                    value: deep_pair(DEPTH),
                    arg_ty: Or::Left(Type::Unit),
                }),
                counter: 0,
            }));
            drain_deep_typed_value(&mut tv);
        });
    }

    #[test]
    fn drain_deep_bigmap_value() {
        // A big_map's in-memory values are owned `TypedValue`s that may be deep.
        on_kernel_stack(|| {
            let mut map = std::collections::BTreeMap::new();
            map.insert(TypedValue::Unit, deep_pair(DEPTH));
            let mut tv = TypedValue::BigMap(BigMap::new(Type::Unit, Type::Unit, map));
            drain_deep_typed_value(&mut tv);
        });
    }
}
