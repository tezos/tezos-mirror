/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use typed_arena::Arena;

use super::annotations::{Annotations, NO_ANNS};
use crate::lexer::Prim;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Micheline<'a> {
    Int(i128),
    String(String),
    Bytes(Vec<u8>),
    /// Application of a Micheline primitive to some arguments with optional
    /// annotations. The primitive is the first field, arguments are the second
    /// field, annotations are the last field.
    App(Prim, &'a [Micheline<'a>], Annotations<'a>),
    Seq(&'a [Micheline<'a>]),
}

impl<'a> Micheline<'a> {
    pub fn prim0(prim: Prim) -> Self {
        Micheline::App(prim, &[], NO_ANNS)
    }

    pub fn prim1(arena: &'a Arena<Micheline<'a>>, prim: Prim, arg: Micheline<'a>) -> Self {
        Micheline::App(prim, arena.alloc_extend([arg]), NO_ANNS)
    }

    pub fn prim2(
        arena: &'a Arena<Micheline<'a>>,
        prim: Prim,
        arg1: Micheline<'a>,
        arg2: Micheline<'a>,
    ) -> Self {
        Micheline::App(prim, arena.alloc_extend([arg1, arg2]), NO_ANNS)
    }
}

impl<'a> From<i128> for Micheline<'a> {
    fn from(x: i128) -> Self {
        Micheline::Int(x)
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
                | Prim::address
                | Prim::chain_id
                | Prim::pair
                | Prim::or
                | Prim::option
                | Prim::list
                | Prim::contract
                | Prim::map
                | Prim::bytes
                | Prim::key
                | Prim::signature,
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
        Micheline::App(Prim::parameter | Prim::storage | Prim::code, ..)
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
                | Prim::LOOP
                | Prim::DIP
                | Prim::ADD
                | Prim::DROP
                | Prim::IF
                | Prim::IF_CONS
                | Prim::IF_LEFT
                | Prim::IF_NONE
                | Prim::FAILWITH
                | Prim::DUP
                | Prim::UNIT
                | Prim::CAR
                | Prim::CDR
                | Prim::PAIR
                | Prim::SOME
                | Prim::COMPARE
                | Prim::AMOUNT
                | Prim::NIL
                | Prim::GET
                | Prim::UPDATE
                | Prim::UNPAIR
                | Prim::CONS
                | Prim::ITER
                | Prim::CHAIN_ID
                | Prim::SELF
                | Prim::SWAP
                | Prim::CHECK_SIGNATURE,
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
                | Prim::Right,
            ..,
        )
    };
}

pub(crate) use {
    micheline_fields, micheline_instructions, micheline_literals, micheline_types, micheline_values,
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
            Micheline::App(Prim::DUP, &[Micheline::Int(3)], NO_ANNS)
        );
        assert_eq!(
            app!(DIP[3, seq!{ app!(DROP) }]),
            Micheline::App(
                Prim::DIP,
                &[
                    Micheline::Int(3),
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
