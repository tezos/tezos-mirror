// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Definition of Micheline representation and utilities for working with it.

use num_bigint::{BigInt, BigUint};
use typed_arena::Arena;

use super::annotations::{Annotation, Annotations, NO_ANNS};
use crate::{
    gas::{unparsing_cost, Gas, OutOfGas},
    lexer::Prim,
};

/// Representation of a Micheline node. The representation is non-owning by
/// design, so something has to own the child nodes. Generally used with an
/// arena allocator, like, e.g. [typed_arena].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Micheline<'a> {
    /// Micheline integer literal.
    Int(BigInt),
    /// Micheline string literal.
    String(String),
    /// Micheline bytes literal.
    Bytes(Vec<u8>),
    /// Application of a Micheline primitive to some arguments with optional
    /// annotations. The primitive is the first field, arguments are the second
    /// field, annotations are the last field.
    App(Prim, &'a [Micheline<'a>], Annotations<'a>),
    /// Micheline braced sequence.
    Seq(&'a [Micheline<'a>]),
}

/* *** Note: Carbonation of allocations ***

Micheline nodes may be allocated either in the heap or in the arena;
we use gas to bound these allocations. Gas is consumed when the
Micheline nodes are constructed so it is already charged when the
nodes are allocated in the arena. For this reason, the constructors of
the Micheline enum type should be avoided and the public methods
defined in this file are the recommended way to build Micheline nodes.

 */

/* *** Note: alloc_extend ***

Arena has an unfortunate pothole related to alloc_extend: if the iterator
itself tries to allocate in the arena, it will panic.

To avoid triggering it accidentally, the method has an error attached via
clippy. When it's known to be safe, `allow` directive is added on the call.
*/

impl<'a> Micheline<'a> {
    pub(crate) fn alloc_seq<const N: usize>(arena: &'a Arena<Self>, args: [Self; N]) -> &'a [Self] {
        // The call is safe, the iterable, being an array, doesn't allocate in
        // the arena during iteration. See Note: alloc_extend
        #[allow(clippy::disallowed_methods)]
        arena.alloc_extend(args)
    }

    /// Construct the Int case.
    pub fn int(i: BigInt, gas: &mut Gas) -> Result<Micheline<'a>, OutOfGas> {
        // The `unparsing_cost` helpers return `CostOverflow` (a pure
        // cost-arithmetic error). The `IntoMicheline` API these
        // constructors serve is `OutOfGas`-typed, so a cost that is
        // unrepresentably large is surfaced as out-of-gas here (it is
        // catchable like OOG). Folding it into a structured error would
        // mean widening `IntoMicheline` across all its callers — deferred.
        gas.consume(unparsing_cost::int(&i).map_err(|_| OutOfGas)?)?;
        Ok(Self::Int(i))
    }

    /// Construct the String case.
    pub fn string(s: String, gas: &mut Gas) -> Result<Micheline<'a>, OutOfGas> {
        gas.consume(unparsing_cost::string(&s).map_err(|_| OutOfGas)?)?;
        Ok(Self::String(s))
    }

    /// Construct the Bytes case.
    pub fn bytes(s: Vec<u8>, gas: &mut Gas) -> Result<Micheline<'a>, OutOfGas> {
        gas.consume(unparsing_cost::bytes(&s).map_err(|_| OutOfGas)?)?;
        Ok(Self::Bytes(s))
    }

    /// Construct a primitive application with zero arguments.
    pub fn prim0(prim: Prim, gas: &mut Gas) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::prim0_uncarbonated(prim))
    }

    /// Same as [prim0] but does not consume any gas. Use this only in
    /// places which cannot be called from the kernel such as the text
    /// parser, macro expansion, and tests
    pub(crate) fn prim0_uncarbonated(prim: Prim) -> Self {
        Micheline::App(prim, &[], NO_ANNS)
    }

    /// Construct a primitive application with one argument, allocating the
    /// argument in the [Arena].
    pub fn prim1(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg: Micheline<'a>,
        gas: &mut Gas,
    ) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::prim1_uncarbonated(arena, prim, arg))
    }

    /// Same as [prim1] but does not consume any gas. Use this only in
    /// places which cannot be called from the kernel such as the text
    /// parser, macro expansion, and tests
    pub(crate) fn prim1_uncarbonated(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg: Micheline<'a>,
    ) -> Self {
        Micheline::App(prim, Self::alloc_seq(arena, [arg]), NO_ANNS)
    }

    /// Construct a primitive application with two arguments, allocating the
    /// arguments in the [Arena].
    pub fn prim2(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg1: Micheline<'a>,
        arg2: Micheline<'a>,
        gas: &mut Gas,
    ) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::prim2_uncarbonated(arena, prim, arg1, arg2))
    }

    /// Same as [prim2] but does not consume any gas. Use this only in
    /// places which cannot be called from the kernel such as the text
    /// parser, macro expansion, and tests
    pub(crate) fn prim2_uncarbonated(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg1: Micheline<'a>,
        arg2: Micheline<'a>,
    ) -> Self {
        Micheline::App(prim, Self::alloc_seq(arena, [arg1, arg2]), NO_ANNS)
    }

    /// Construct a primitive application with three arguments, allocating the
    /// arguments in the [Arena].
    pub fn prim3(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg1: Micheline<'a>,
        arg2: Micheline<'a>,
        arg3: Micheline<'a>,
        gas: &mut Gas,
    ) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::prim3_uncarbonated(arena, prim, arg1, arg2, arg3))
    }

    /// Same as [prim3] but does not consume any gas. Use this only in
    /// places which cannot be called from the kernel such as the text
    /// parser, macro expansion, and tests
    pub(crate) fn prim3_uncarbonated(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg1: Micheline<'a>,
        arg2: Micheline<'a>,
        arg3: Micheline<'a>,
    ) -> Self {
        Micheline::App(prim, Self::alloc_seq(arena, [arg1, arg2, arg3]), NO_ANNS)
    }

    /// Construct a primitive application from already-allocated arguments.
    pub fn prim_n(prim: Prim, args: &'a [Micheline<'a>], gas: &mut Gas) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::App(prim, args, NO_ANNS))
    }

    /// Same as [prim_n] but [args] are allocated in the given arena.
    pub fn prim_n_arr<const N: usize>(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        args: [Micheline<'a>; N],
        gas: &mut Gas,
    ) -> Result<Self, OutOfGas> {
        Self::prim_n(prim, Self::alloc_seq(arena, args), gas)
    }

    /// Construct a Micheline sequence of already-allocated args
    pub fn seq(args: &'a [Micheline<'a>], gas: &mut Gas) -> Result<Self, OutOfGas> {
        gas.consume(unparsing_cost::NODE)?;
        Ok(Self::Seq(args))
    }

    /// Same as [seq] but [args] are allocated in the given arena.
    pub fn seq_arr<const N: usize>(
        arena: &'a Arena<Micheline<'a>>,
        args: [Micheline<'a>; N],
        gas: &mut Gas,
    ) -> Result<Self, OutOfGas> {
        Self::seq(Self::alloc_seq(arena, args), gas)
    }

    /// Same as [seq_arr] but does not consume any gas. Use this only in
    /// places which cannot be called from the kernel such as the text
    /// parser, macro expansion, and tests
    pub(crate) fn seq_arr_uncarbonated<const N: usize>(
        arena: &'a Arena<Micheline<'a>>,
        args: [Micheline<'a>; N],
    ) -> Self {
        Self::Seq(Self::alloc_seq(arena, args))
    }

    /// Add an annotation on a Micheline node.
    pub fn annotate(&mut self, annotation: Annotation<'a>, gas: &mut Gas) -> Result<(), OutOfGas> {
        match self {
            Self::App(_prim, _args, ref mut annots) => {
                gas.consume(unparsing_cost::annotation(&annotation).map_err(|_| OutOfGas)?)?;
                annots.push(annotation);
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl From<i128> for Micheline<'_> {
    fn from(x: i128) -> Self {
        Micheline::Int(x.into())
    }
}

impl From<BigInt> for Micheline<'_> {
    fn from(x: BigInt) -> Self {
        Micheline::Int(x)
    }
}

impl From<BigUint> for Micheline<'_> {
    fn from(x: BigUint) -> Self {
        Micheline::Int(x.into())
    }
}

impl From<String> for Micheline<'_> {
    fn from(x: String) -> Self {
        Micheline::String(x)
    }
}

impl From<Vec<u8>> for Micheline<'_> {
    fn from(x: Vec<u8>) -> Self {
        Micheline::Bytes(x)
    }
}

impl From<()> for Micheline<'_> {
    fn from(_: ()) -> Self {
        Micheline::App(Prim::Unit, &[], NO_ANNS)
    }
}

impl From<bool> for Micheline<'_> {
    fn from(x: bool) -> Self {
        Micheline::App(if x { Prim::True } else { Prim::False }, &[], NO_ANNS)
    }
}

impl From<&str> for Micheline<'_> {
    fn from(s: &str) -> Self {
        Micheline::from(s.to_owned())
    }
}

/// Trait for types that can be converted into [Micheline].
pub trait IntoMicheline<'a> {
    /// Untypes a value using optimized representation in legacy mode.
    ///
    /// This differs from plain optimized representation in that it always
    /// represents tuples as nested binary pairs (right combs). This is, for
    /// instance, what `PACK` uses.
    ///
    /// However, note that a right-comb `pair` type is represented as a
    /// sequence, for consistency with `PACK`.
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        gas: &mut Gas,
    ) -> Result<Micheline<'a>, OutOfGas>;
}

/// Pattern synonym matching all types which are not yet
/// supported. Useful for total match in the typechecker.
#[macro_export]
macro_rules! micheline_unsupported_types_common {
    () => {
        Prim::chest
            | Prim::chest_key
            | Prim::tx_rollup_l2_address
            | Prim::sapling_state
            | Prim::sapling_transaction
            | Prim::sapling_transaction_deprecated
    };
}

#[cfg(feature = "bls")]
macro_rules! micheline_unsupported_types {
    () => {
        $crate::micheline_unsupported_types_common!()
    };
}

#[cfg(not(feature = "bls"))]
macro_rules! micheline_unsupported_types {
    () => {
        $crate::micheline_unsupported_types_common!()
            | Prim::bls12_381_fr
            | Prim::bls12_381_g1
            | Prim::bls12_381_g2
    };
}

/// Pattern synonym matching all type primitive applications. Useful for total
/// matches.
#[macro_export]
macro_rules! micheline_types_common {
    () => {
        Prim::int
            | Prim::nat
            | Prim::bool
            | Prim::mutez
            | Prim::string
            | Prim::operation
            | Prim::unit
            | Prim::never
            | Prim::address
            | Prim::chain_id
            | Prim::pair
            | Prim::or
            | Prim::option
            | Prim::list
            | Prim::contract
            | Prim::map
            | Prim::bytes
            | Prim::ticket
            | Prim::sapling_state
            | Prim::sapling_transaction
            | Prim::sapling_transaction_deprecated
            | Prim::chest
            | Prim::chest_key
            | Prim::key
            | Prim::key_hash
            | Prim::signature
            | Prim::lambda
            | Prim::timestamp
            | Prim::tx_rollup_l2_address
            | Prim::set
            | Prim::big_map
            | Prim::bls12_381_fr
            | Prim::bls12_381_g1
            | Prim::bls12_381_g2
    };
}

macro_rules! micheline_types {
    () => {
        Micheline::App($crate::micheline_types_common!(), ..)
    };
}

/// Pattern synonym matching all Micheline literals. Useful for total
/// matches.
macro_rules! micheline_literals {
    () => {
        Micheline::Int(..) | Micheline::String(..) | Micheline::Bytes(..)
    };
}

/// Pattern synonym matching all field primitive applications. Useful for total
/// matches.
macro_rules! micheline_fields {
    () => {
        Micheline::App(
            Prim::parameter | Prim::storage | Prim::code | Prim::view | Prim::constant,
            ..,
        )
    };
}

/// Pattern synonym matching all instruction which are not yet
/// supported. Useful for total match in the typechecker.
#[macro_export]
macro_rules! micheline_unsupported_instructions_common {
    () => {
        Prim::SAPLING_EMPTY_STATE
            | Prim::SAPLING_VERIFY_UPDATE
            | Prim::OPEN_CHEST
            | Prim::CREATE_ACCOUNT
            | Prim::STEPS_TO_QUOTA
            | Prim::TICKET_DEPRECATED
            | Prim::INDEX_ADDRESS
            | Prim::GET_ADDRESS_INDEX
    };
}

#[cfg(feature = "bls")]
macro_rules! micheline_unsupported_instructions {
    () => {
        $crate::micheline_unsupported_instructions_common!()
    };
}

#[cfg(not(feature = "bls"))]
macro_rules! micheline_unsupported_instructions {
    () => {
        $crate::micheline_unsupported_instructions_common!() | Prim::PAIRING_CHECK
    };
}

/// Pattern synonym matching all instruction primitive applications. Useful for total
/// matches.
#[macro_export]
macro_rules! micheline_instructions {
    () => {
        Micheline::App(
            Prim::PUSH
                | Prim::INT
                | Prim::GT
                | Prim::GE
                | Prim::LE
                | Prim::LT
                | Prim::EQ
                | Prim::NEQ
                | Prim::LOOP
                | Prim::LOOP_LEFT
                | Prim::DIP
                | Prim::ADD
                | Prim::DROP
                | Prim::IF
                | Prim::IF_CONS
                | Prim::IF_LEFT
                | Prim::IF_NONE
                | Prim::FAILWITH
                | Prim::NEVER
                | Prim::DUP
                | Prim::UNIT
                | Prim::CAST
                | Prim::RENAME
                | Prim::ISNAT
                | Prim::NAT
                | Prim::BYTES
                | Prim::CAR
                | Prim::CDR
                | Prim::PAIR
                | Prim::SOME
                | Prim::COMPARE
                | Prim::ADDRESS
                | Prim::CONTRACT
                | Prim::AMOUNT
                | Prim::NIL
                | Prim::MEM
                | Prim::GET
                | Prim::UPDATE
                | Prim::GET_AND_UPDATE
                | Prim::SIZE
                | Prim::UNPAIR
                | Prim::NONE
                | Prim::CONS
                | Prim::ITER
                | Prim::CHAIN_ID
                | Prim::SWAP
                | Prim::SELF
                | Prim::PACK
                | Prim::UNPACK
                | Prim::BLAKE2B
                | Prim::KECCAK
                | Prim::SHA256
                | Prim::SHA512
                | Prim::SHA3
                | Prim::OPEN_CHEST
                | Prim::VIEW
                | Prim::BALANCE
                | Prim::NOW
                | Prim::SOURCE
                | Prim::SENDER
                | Prim::SLICE
                | Prim::TICKET_DEPRECATED
                | Prim::TICKET
                | Prim::READ_TICKET
                | Prim::SPLIT_TICKET
                | Prim::JOIN_TICKETS
                | Prim::DIG
                | Prim::DUG
                | Prim::LEVEL
                | Prim::SELF_ADDRESS
                | Prim::STEPS_TO_QUOTA
                | Prim::CHECK_SIGNATURE
                | Prim::CONCAT
                | Prim::CREATE_ACCOUNT
                | Prim::CREATE_CONTRACT
                | Prim::IMPLICIT_ACCOUNT
                | Prim::TRANSFER_TOKENS
                | Prim::SET_DELEGATE
                | Prim::EMIT
                | Prim::HASH_KEY
                | Prim::EMPTY_SET
                | Prim::EMPTY_MAP
                | Prim::EMPTY_BIG_MAP
                | Prim::MIN_BLOCK_TIME
                | Prim::VOTING_POWER
                | Prim::TOTAL_VOTING_POWER
                | Prim::SAPLING_EMPTY_STATE
                | Prim::SAPLING_VERIFY_UPDATE
                | Prim::ABS
                | Prim::NEG
                | Prim::SUB
                | Prim::SUB_MUTEZ
                | Prim::MUL
                | Prim::EDIV
                | Prim::LSL
                | Prim::LSR
                | Prim::EXEC
                | Prim::APPLY
                | Prim::LAMBDA
                | Prim::LAMBDA_REC
                | Prim::LEFT
                | Prim::RIGHT
                | Prim::MAP
                | Prim::NOT
                | Prim::AND
                | Prim::XOR
                | Prim::OR
                | Prim::PAIRING_CHECK
                | Prim::IS_IMPLICIT_ACCOUNT
                | Prim::INDEX_ADDRESS
                | Prim::GET_ADDRESS_INDEX,
            ..,
        )
    };
}

/// Pattern synonym matching all value constructor primitive applications.
/// Useful for total matches.
macro_rules! micheline_values {
    () => {
        Micheline::App(
            Prim::True
                | Prim::False
                | Prim::Unit
                | Prim::None
                | Prim::Pair
                | Prim::Some
                | Prim::Elt
                | Prim::Left
                | Prim::Right
                | Prim::Lambda_rec
                | Prim::Ticket
                | Prim::Transfer_tokens
                | Prim::Emit
                | Prim::Create_contract
                | Prim::Set_delegate,
            ..,
        )
    };
}

pub(crate) use {
    micheline_fields, micheline_instructions, micheline_literals, micheline_types,
    micheline_unsupported_instructions, micheline_unsupported_types, micheline_values,
};

#[cfg(test)]
#[allow(missing_docs)]
pub mod test_helpers {

    /// Helper to reduce syntactic noise when constructing Micheline applications in tests.
    ///
    /// See the test below for examples.
    macro_rules! app {
        ($prim:ident [$($args:expr),* $(,)*]) => {
            $crate::ast::micheline::Micheline::App(
                $crate::lexer::Prim::$prim, &[$($crate::ast::micheline::Micheline::from($args)),*],
                $crate::ast::annotations::NO_ANNS,
            )
        };
        ($prim:ident) => {
            $crate::ast::micheline::Micheline::App(
                $crate::lexer::Prim::$prim,
                &[],
                $crate::ast::annotations::NO_ANNS,
            )
        };
    }

    #[test]
    fn test_app() {
        use super::*;
        assert_eq!(app!(True), Micheline::App(Prim::True, &[], NO_ANNS));
        assert_eq!(
            app!(DUP[3]),
            Micheline::App(Prim::DUP, &[Micheline::Int(3.into())], NO_ANNS)
        );
        assert_eq!(
            app!(DIP[3, seq!{ app!(DROP) }]),
            Micheline::App(
                Prim::DIP,
                &[
                    Micheline::Int(3.into()),
                    Micheline::Seq(&[Micheline::App(Prim::DROP, &[], NO_ANNS)])
                ],
                NO_ANNS
            )
        );
    }

    /// Helper to reduce syntactic noise when constructing Micheline sequences in tests.
    ///
    /// See the test below for examples.
    macro_rules! seq {
        {$($elt:expr);* $(;)*} => {
            $crate::ast::micheline::Micheline::Seq(&[$($crate::ast::micheline::Micheline::from($elt)),*])
        }
    }

    #[test]
    fn test_seq() {
        use super::*;
        assert_eq!(seq! {}, Micheline::Seq(&[]));
        assert_eq!(
            seq! { app!(CAR) },
            Micheline::Seq(&[Micheline::App(Prim::CAR, &[], NO_ANNS)])
        );
        assert_eq!(
            seq! { app!(CAR); app!(DUP); },
            Micheline::Seq(&[
                Micheline::App(Prim::CAR, &[], NO_ANNS),
                Micheline::App(Prim::DUP, &[], NO_ANNS),
            ])
        );
    }

    pub(crate) use {app, seq};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(dead_code)]
    /// Static test to check that `micheline_*` pattern synonyms cover all
    /// constructors except Seq.
    fn pattern_synonym_coverage(micheline: Micheline) {
        match micheline {
            micheline_fields!()
            | micheline_instructions!()
            | micheline_literals!()
            | micheline_types!()
            | micheline_values!()
            | Micheline::Seq(..) => (),
        }
    }
}
