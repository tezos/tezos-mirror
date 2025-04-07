/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Definition of Micheline representation and utilities for working with it.

use num_bigint::{BigInt, BigUint};
use typed_arena::Arena;

use super::annotations::{Annotations, NO_ANNS};
use crate::lexer::Prim;

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

/* *** Note: alloc_extend ***

Arena has an unfortunate pothole related to alloc_extend: if the iterator
itself tries to allocate in the arena, it will panic.

To avoid triggering it accidentally, the mehtod has an error attached via
clippy. When it's known to be safe, `allow` directive is added on the call.
*/

impl<'a> Micheline<'a> {
    pub(crate) fn alloc_seq<const N: usize>(arena: &'a Arena<Self>, args: [Self; N]) -> &'a [Self] {
        // The call is safe, the iterable, being an array, doesn't allocate in
        // the arena during iteration. See Note: alloc_extend
        #[allow(clippy::disallowed_methods)]
        arena.alloc_extend(args)
    }

    pub(crate) fn alloc_iter(
        arena: &'a Arena<Self>,
        mut iter: impl ExactSizeIterator<Item = Self>,
    ) -> &'a [Micheline<'a>] {
        // preallocate the slice, filling it with Micheline::Seq(&[]), which is
        // the simplest Micheline variant. We control the iterator here, it
        // doesn't allocate in the arena, the call is safe.
        // See Note: alloc_extend
        #[allow(clippy::disallowed_methods)]
        let buf = arena.alloc_extend(std::iter::repeat(Micheline::Seq(&[])).take(iter.len()));
        let mut actual_len: usize = 0;
        for (dest, item) in buf.iter_mut().zip(&mut iter) {
            *dest = item;
            actual_len += 1;
        }
        assert!(iter.next().is_none());
        assert!(buf.len() == actual_len);
        buf
    }

    /// Construct a primitive application with zero arguments.
    pub fn prim0(prim: Prim) -> Self {
        Micheline::App(prim, &[], NO_ANNS)
    }

    /// Construct a primitive application with one argument, allocating the
    /// argument in the [Arena].
    pub fn prim1(arena: &'a Arena<Micheline<'a>>, prim: Prim, arg: Micheline<'a>) -> Self {
        Micheline::App(prim, Self::alloc_seq(arena, [arg]), NO_ANNS)
    }

    /// Construct a primitive application with two arguments, allocating the
    /// arguments in the [Arena].
    pub fn prim2(
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
    ) -> Self {
        Micheline::App(prim, Self::alloc_seq(arena, [arg1, arg2, arg3]), NO_ANNS)
    }

    /// Construct a Micheline sequence, allocating the elements in the [Arena].
    pub fn seq<const N: usize>(arena: &'a Arena<Micheline<'a>>, args: [Micheline<'a>; N]) -> Self {
        Micheline::Seq(Self::alloc_seq(arena, args))
    }
}

impl<'a> From<i128> for Micheline<'a> {
    fn from(x: i128) -> Self {
        Micheline::Int(x.into())
    }
}

impl<'a> From<BigInt> for Micheline<'a> {
    fn from(x: BigInt) -> Self {
        Micheline::Int(x)
    }
}

impl<'a> From<BigUint> for Micheline<'a> {
    fn from(x: BigUint) -> Self {
        Micheline::Int(x.into())
    }
}

impl<'a> From<String> for Micheline<'a> {
    fn from(x: String) -> Self {
        Micheline::String(x)
    }
}

impl<'a> From<Vec<u8>> for Micheline<'a> {
    fn from(x: Vec<u8>) -> Self {
        Micheline::Bytes(x)
    }
}

impl<'a> From<()> for Micheline<'a> {
    fn from(_: ()) -> Self {
        Micheline::App(Prim::Unit, &[], NO_ANNS)
    }
}

impl<'a> From<bool> for Micheline<'a> {
    fn from(x: bool) -> Self {
        Micheline::prim0(if x { Prim::True } else { Prim::False })
    }
}

impl<'a> From<&str> for Micheline<'a> {
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
    ) -> Micheline<'a>;
}

/// Pattern synonym matching all types which are not yet
/// supported. Useful for total match in the typechecker.
macro_rules! micheline_unsupported_types {
    () => {
        Prim::chest
            | Prim::chest_key
            | Prim::tx_rollup_l2_address
            | Prim::sapling_state
            | Prim::sapling_transaction
            | Prim::sapling_transaction_deprecated
    };
}

/// Pattern synonym matching all type primitive applications. Useful for total
/// matches.
macro_rules! micheline_types {
    () => {
        Micheline::App(
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
                | Prim::bls12_381_g1
                | Prim::bls12_381_g2
                | Prim::bls12_381_fr
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
                | Prim::big_map,
            ..,
        )
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
macro_rules! micheline_unsupported_instructions {
    () => {
        Prim::SAPLING_EMPTY_STATE
            | Prim::SAPLING_VERIFY_UPDATE
            | Prim::OPEN_CHEST
            | Prim::VIEW
            | Prim::CREATE_ACCOUNT
            | Prim::STEPS_TO_QUOTA
            | Prim::TICKET_DEPRECATED
            | Prim::CAST
            | Prim::RENAME
    };
}

/// Pattern synonym matching all instruction primitive applications. Useful for total
/// matches.
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
                | Prim::PAIRING_CHECK
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
                | Prim::OR,
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
