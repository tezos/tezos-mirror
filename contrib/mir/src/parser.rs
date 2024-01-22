/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::lexer::{LexerError, Tok};
use crate::syntax;
use lalrpop_util::ParseError;
use logos::Logos;
use typed_arena::Arena;

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParserError {
    #[error("expected a natural from 0 to 1023 inclusive, but got {0}")]
    ExpectedU10(i128),
    #[error(transparent)]
    LexerError(#[from] LexerError),
}

pub struct Parser<'a> {
    pub arena: Arena<Micheline<'a>>,
}

impl Default for Parser<'_> {
    fn default() -> Self {
        Parser::new()
    }
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser {
            arena: Arena::new(),
        }
    }

    pub fn parse(&'a self, src: &str) -> Result<Micheline, ParseError<usize, Tok, ParserError>> {
        syntax::MichelineNakedParser::new().parse(&self.arena, spanned_lexer(src))
    }

    pub fn parse_top_level(
        &'a self,
        src: &str,
    ) -> Result<Micheline, ParseError<usize, Tok, ParserError>> {
        syntax::MichelineTopLevelParser::new().parse(&self.arena, spanned_lexer(src))
    }
}

pub fn spanned_lexer(
    src: &'_ str,
) -> impl Iterator<Item = Result<(usize, Tok, usize), ParserError>> + '_ {
    Tok::lexer(src)
        .spanned()
        .map(|(tok_or_err, span)| match tok_or_err {
            Ok(tok) => Ok((span.start, tok, span.end)),
            Err(err) => Err(err.into()),
        })
}

#[cfg(test)]
pub mod test_helpers {
    use super::*;

    pub fn parse(s: &str) -> Result<Micheline, ParseError<usize, Tok, ParserError>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse(s)
    }

    pub fn parse_contract_script(
        s: &str,
    ) -> Result<Micheline, ParseError<usize, Tok, ParserError>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse_top_level(s)
    }
}

#[cfg(test)]
mod tests {
    use super::test_helpers::*;
    use crate::ast::micheline::test_helpers::{app, seq};

    #[test]
    fn pair_type() {
        assert_eq!(
            parse("PUSH (pair int nat) Unit").unwrap(),
            app!(PUSH[app!(pair[app!(int), app!(nat)]), app!(Unit)])
        );
        assert_eq!(
            parse("PUSH (pair int nat unit) Unit").unwrap(),
            app!(PUSH[app!(pair[app!(int), app!(nat), app!(unit)]), app!(Unit)])
        );
        assert_eq!(
            parse("PUSH (pair (pair int nat) unit) Unit").unwrap(),
            app!(PUSH[
              app!(pair[app!(pair[app!(int), app!(nat)]), app!(unit)]), app!(Unit)
            ])
        );
    }

    #[test]
    fn or_type() {
        assert_eq!(
            parse("PUSH (or int nat) Unit").unwrap(),
            app!(PUSH[app!(or[app!(int), app!(nat)]), app!(Unit)])
        );
    }

    #[test]
    fn pair_value() {
        assert_eq!(
            parse("PUSH unit (Pair 3 4)").unwrap(),
            app!(PUSH[app!(unit), app!(Pair[3, 4])])
        );
        assert_eq!(
            parse("PUSH unit (Pair 3 4 5)").unwrap(),
            app!(PUSH[app!(unit), app!(Pair[3, 4, 5])])
        );
        assert_eq!(
            parse("PUSH unit (Pair (Pair 3 4) 5)").unwrap(),
            app!(PUSH[app!(unit), app!(Pair[app!(Pair[3, 4]), 5])])
        );
        assert_eq!(
            parse("PUSH pair unit unit Pair Unit Unit"),
            Ok(app!(
                PUSH[app!(pair), app!(unit), app!(unit), app!(Pair), app!(Unit), app!(Unit)]
            ))
        );
        assert_eq!(
            parse("PUSH (pair unit unit) Pair Unit Unit"),
            Ok(app!(
                PUSH[app!(pair[app!(unit), app!(unit)]), app!(Pair), app!(Unit), app!(Unit)]
            ))
        );
    }

    #[test]
    fn or_value() {
        assert_eq!(
            parse("PUSH (or int unit) (Left 3)").unwrap(),
            app!(PUSH[app!(or[app!(int), app!(unit)]), app!(Left[3])]),
        );
    }

    #[test]
    fn value_parens() {
        assert_eq!(
            parse("PUSH unit (Unit)").unwrap(),
            app!(PUSH[app!(unit), app!(Unit)])
        );
    }

    #[test]
    fn type_anns() {
        assert_eq!(parse("(int :p)"), Ok(app!(int)));
        assert_eq!(
            parse("(pair :point (int :x_pos) (int :y_pos))"),
            Ok(app!(pair[app!(int), app!(int)]))
        );
        assert_eq!(parse("(string %foo)"), Ok(app!(string)));
        assert_eq!(parse("(string %foo :bar @baz)"), Ok(app!(string)));
        assert_eq!(parse("(string @foo)"), Ok(app!(string)));
        assert_eq!(
            parse("(pair %a (int %b) (int %c))"),
            Ok(app!(pair[app!(int), app!(int)]))
        );
        assert_eq!(
            parse("(or %a (int %b) (int %c))"),
            Ok(app!(or[app!(int), app!(int)]))
        );
        assert_eq!(
            parse("(option %a int %b)")
                .unwrap_err()
                .to_string()
                .as_str()
                .lines()
                .next()
                .unwrap(),
            "Unrecognized token `<ann>` found at 15:17"
        );
    }

    #[test]
    fn instr_anns() {
        assert_eq!(
            parse("PUSH @var :ty %field int 1").unwrap(),
            app!(PUSH[app!(int), 1]),
        );
        assert_eq!(
            parse("CAR @var :ty %field :ty.2 @var.2 %field.2").unwrap(),
            app!(CAR),
        );
    }

    #[test]
    fn invalid_prim() {
        assert_eq!(
            parse("{UNNIT}").unwrap_err().to_string(),
            "unknown primitive: UNNIT"
        );
    }

    #[test]
    fn parse_contract_script_test() {
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code FAILWITH"),
            Ok(seq! {
              app!(parameter[app!(unit)]);
              app!(storage[app!(unit)]);
              app!(code[app!(FAILWITH)]);
            })
        );

        // non-atomic arguments to code must be wrapped in braces
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code NIL unit"),
            Ok(seq! {
                app!(parameter[app!(unit)]);
                app!(storage[app!(unit)]);
                app!(code[app!(NIL), app!(unit)]);
            })
        );
        // or parentheses
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code (NIL unit)"),
            Ok(seq! {
                app!(parameter[app!(unit)]);
                app!(storage[app!(unit)]);
                app!(code[app!(NIL[app!(unit)])]);
            })
        );
    }

    #[test]
    // Test parsing of annotated contract type.
    // Type contract is not pushable so this would be rejected
    // at type-checking but this test only exercises the parser.
    fn contract_ty_push() {
        assert_eq!(
            parse("PUSH (contract :ct %foo unit) Unit").unwrap(),
            app!(PUSH[app!(contract[app!(unit)]), app!(Unit)])
        );
    }

    #[test]
    fn bytes_push() {
        assert_eq!(
            parse("PUSH unit 0xdeadf00d").unwrap(),
            app!(PUSH[app!(unit), vec![0xde, 0xad, 0xf0, 0x0d]])
        );
    }

    #[test]
    fn address_ty_push() {
        assert_eq!(
            parse("PUSH address \"tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw\"").unwrap(),
            app!(PUSH[app!(address), "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw"])
        );
    }
}
