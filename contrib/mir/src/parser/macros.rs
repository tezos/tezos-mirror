/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::*;

use crate::lexer::macros::*;
use crate::lexer::Prim;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum MacroError {
    #[error("unexpected number of arguments for macro: {0}")]
    UnexpectedArgumentCount(Macro),
}

pub fn expand_macro<'a>(
    arena: &'a Arena<Micheline<'a>>,
    m: &Macro,
    args: MacroArgs<'a>,
) -> Result<Micheline<'a>, ParserError> {
    use Macro::*;
    use MacroArgs::*;
    use MacroError::*;
    use Micheline as M;
    use Micheline::*;
    use Prim::*;
    let unex_arg_err: ParserError = UnexpectedArgumentCount(m.clone()).into();
    match (m, args) {
        (CMPEQ, NoArgs) => Ok(Seq(arena.alloc_extend([M::prim0(COMPARE), M::prim0(EQ)]))),
        (CMPEQ, _) => Err(unex_arg_err),

        (CMPLE, NoArgs) => Ok(Seq(arena.alloc_extend([M::prim0(COMPARE), M::prim0(LE)]))),
        (CMPLE, _) => Err(unex_arg_err),

        (IF_SOME, TwoArgs(ib1, ib2)) => {
            Ok(Seq(arena.alloc_extend([M::prim2(arena, IF_NONE, ib2, ib1)])))
        }
        (IF_SOME, _) => Err(unex_arg_err),

        (IFCMPEQ, TwoArgs(ib1, ib2)) => Ok(Seq(arena.alloc_extend([
            M::prim0(COMPARE),
            M::prim0(EQ),
            M::prim2(arena, IF, ib1, ib2),
        ]))),
        (IFCMPEQ, _) => Err(unex_arg_err),

        (IFCMPLE, TwoArgs(ib1, ib2)) => Ok(Seq(arena.alloc_extend([
            M::prim0(COMPARE),
            M::prim0(LE),
            M::prim2(arena, IF, ib1, ib2),
        ]))),
        (IFCMPLE, _) => Err(unex_arg_err),

        (ASSERT, NoArgs) => Ok(Seq(arena.alloc_extend([M::prim2(
            arena,
            IF,
            Seq(&[]),
            Seq(arena.alloc_extend([expand_macro(arena, &FAIL, NoArgs)?])),
        )]))),
        (ASSERT, _) => Err(unex_arg_err),

        // The following might seem a bit less straight forward than it could be. But the reference
        // implementation wraps the first two instructions in a seq, so we are doing the same.
        (ASSERT_CMPEQ, NoArgs) => Ok(Seq(arena.alloc_extend([
            expand_macro(arena, &CMPEQ, NoArgs)?,
            M::prim2(
                arena,
                IF,
                Seq(&[]),
                Seq(arena.alloc_extend([expand_macro(arena, &FAIL, NoArgs)?])),
            ),
        ]))),
        (ASSERT_CMPEQ, _) => Err(unex_arg_err),

        (ASSERT_CMPLE, NoArgs) => Ok(Seq(arena.alloc_extend([
            expand_macro(arena, &CMPLE, NoArgs)?,
            M::prim2(
                arena,
                IF,
                Seq(&[]),
                Seq(arena.alloc_extend([expand_macro(arena, &FAIL, NoArgs)?])),
            ),
        ]))),
        (ASSERT_CMPLE, _) => Err(unex_arg_err),

        (FAIL, NoArgs) => Ok(Seq(arena.alloc_extend([M::prim0(UNIT), M::prim0(FAILWITH)]))),
        (FAIL, _) => Err(unex_arg_err),

        // Do not wrap expansion of DII+P and DUU+P in a Seq to
        // match octez-client behavior.
        (DIIP(c), OneArg(ib)) => Ok(M::prim2(arena, DIP, M::Int(*c as i128), ib)),
        (DIIP(_), _) => Err(unex_arg_err),

        (DUUP(c), NoArgs) => Ok(M::prim1(arena, DUP, M::Int(*c as i128))),
        (DUUP(_), _) => Err(unex_arg_err),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_helpers::*;

    #[test]
    fn test_macros() {
        // The tests below checks that the macro expands to the same sequence
        // of instruction as produced by the octez-client. The latter is also
        // provided as the expectation.
        assert_eq!(
            parse("{ ASSERT }").unwrap(),
            parse("{ { IF {} { { UNIT ; FAILWITH } } } }").unwrap()
        );

        assert_eq!(
            parse("{ ASSERT_CMPEQ }").unwrap(),
            parse("{{ { COMPARE ; EQ } ; IF {} { { UNIT ; FAILWITH } } }}").unwrap()
        );

        assert_eq!(
            parse("{ ASSERT_CMPLE }").unwrap(),
            parse("{{ { COMPARE ; LE } ; IF {} { { UNIT ; FAILWITH } } }}").unwrap()
        );

        assert_eq!(
            parse("{ IF_SOME { UNIT } {} }").unwrap(),
            parse("{ { IF_NONE {} { UNIT } } }").unwrap()
        );

        assert_eq!(
            parse("{ IFCMPEQ { UNIT } {} }").unwrap(),
            parse("{ { COMPARE ; EQ ; IF { UNIT } {} } }").unwrap()
        );

        assert_eq!(
            parse("{ IFCMPLE { UNIT } {} }").unwrap(),
            parse("{ { COMPARE ; LE ; IF { UNIT } {} } }").unwrap()
        );

        assert_eq!(
            parse("{ FAIL }").unwrap(),
            parse("{ { UNIT ; FAILWITH } }").unwrap()
        );

        assert_eq!(
            parse("{ DIIIP { UNIT } }").unwrap(),
            parse("{ DIP 3 { UNIT } }").unwrap()
        );

        assert_eq!(parse("{ DUUP }").unwrap(), parse("{ DUP 2 }").unwrap());

        assert_eq!(parse("{ DUUUUP }").unwrap(), parse("{ DUP 4 }").unwrap());

        assert_eq!(
            parse("{ FAIL {} {} }").unwrap_err().to_string(),
            "unexpected number of arguments for macro: FAIL"
        );
    }
}
