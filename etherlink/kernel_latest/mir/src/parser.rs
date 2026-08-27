// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Michelson parser.

#[cfg(feature = "text-parser")]
pub mod macros;

use crate::ast::*;
#[cfg(feature = "text-parser")]
use crate::lexer::{LexerError, Tok};
#[cfg(feature = "text-parser")]
use crate::syntax;
#[cfg(feature = "text-parser")]
use lalrpop_util::ParseError;
#[cfg(feature = "text-parser")]
use logos::Logos;
#[cfg(feature = "text-parser")]
use macros::MacroError;
use typed_arena::Arena;

/// Errors that can happen during parsing, aside from parser-specific ones.
#[cfg(feature = "text-parser")]
#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParserError {
    /// An error happened at the lexer stage.
    #[error(transparent)]
    LexerError(#[from] LexerError),
    /// An error happened during macro expansion.
    #[error(transparent)]
    MacroError(#[from] MacroError),
}

/// A parser for Michelson. Carries an [Arena] for placing [Micheline] nodes
/// into.
pub struct Parser<'a> {
    /// The [Arena] to place [Micheline] nodes into.
    pub arena: Arena<Micheline<'a>>,
}

impl Default for Parser<'_> {
    fn default() -> Self {
        Parser::new()
    }
}

impl<'a> Parser<'a> {
    /// Construct a new parser.
    pub fn new() -> Self {
        Parser {
            arena: Arena::new(),
        }
    }
}

#[cfg(feature = "text-parser")]
impl<'a> Parser<'a> {
    /// Parse Michelson code or value into [Micheline].
    pub fn parse(
        &'a self,
        src: &'a str,
    ) -> Result<Micheline<'a>, ParseError<usize, Tok<'a>, ParserError>> {
        syntax::MichelineNakedParser::new().parse(&self.arena, spanned_lexer(src))
    }

    /// Parse Michelson script into [Micheline]. Top-level refers to a full
    /// Michelson script, i.e. something that contains `parameter`, `storage`
    /// and `code` fields.
    pub fn parse_top_level(
        &'a self,
        src: &'a str,
    ) -> Result<Micheline<'a>, ParseError<usize, Tok<'a>, ParserError>> {
        syntax::MichelineTopLevelParser::new().parse(&self.arena, spanned_lexer(src))
    }
}

/// Given a Michelson string, create an iterator over lexemes in that string,
/// with location information attached.
#[cfg(feature = "text-parser")]
pub(crate) fn spanned_lexer(
    src: &str,
) -> impl Iterator<Item = Result<(usize, Tok<'_>, usize), ParserError>> + '_ {
    Tok::lexer(src)
        .spanned()
        .map(|(tok_or_err, span)| match tok_or_err {
            Ok(tok) => Ok((span.start, tok, span.end)),
            Err(err) => Err(err.into()),
        })
}

#[cfg(all(test, feature = "text-parser"))]
#[allow(missing_docs)]
pub mod test_helpers {
    use super::*;

    pub fn parse(
        s: &str,
    ) -> Result<Micheline<'_>, ParseError<usize, Tok<'_>, ParserError>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse(s)
    }

    pub fn parse_contract_script(
        s: &str,
    ) -> Result<Micheline<'_>, ParseError<usize, Tok<'_>, ParserError>> {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse_top_level(s)
    }
}

#[cfg(all(test, feature = "text-parser"))]
mod tests {
    use super::test_helpers::*;
    use crate::ast::micheline::test_helpers::{app, seq};
    use crate::ast::{Annotation, Micheline};
    use crate::lexer::Prim;

    #[test]
    fn instructions() {
        assert_eq!(parse("LE").unwrap(), app!(LE));
        assert_eq!(parse("EQ").unwrap(), app!(EQ));
        assert_eq!(
            parse("EQ @a").unwrap(),
            Micheline::App(Prim::EQ, &[], [Annotation::Variable("a".into())].into())
        );
    }

    #[test]
    fn types() {
        assert_eq!(parse("ticket").unwrap(), app!(ticket));
        assert_eq!(parse("timestamp").unwrap(), app!(timestamp));
    }

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
        assert_eq!(
            parse("(int :p)"),
            Ok(Micheline::App(
                Prim::int,
                &[],
                [Annotation::Type("p".into())].into()
            ))
        );
        assert_eq!(
            parse("(pair :point (int :x_pos) (int :y_pos))"),
            Ok(Micheline::App(
                Prim::pair,
                &[
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Type("x_pos".into())].into()
                    ),
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Type("y_pos".into())].into()
                    ),
                ],
                [Annotation::Type("point".into())].into()
            ))
        );
        assert_eq!(
            parse("(string %foo)"),
            Ok(Micheline::App(
                Prim::string,
                &[],
                [Annotation::Field("foo".into())].into()
            ))
        );
        assert_eq!(
            parse("(string %foo :bar @baz)"),
            Ok(Micheline::App(
                Prim::string,
                &[],
                [
                    Annotation::Field("foo".into()),
                    Annotation::Type("bar".into()),
                    Annotation::Variable("baz".into())
                ]
                .into()
            ))
        );
        assert_eq!(
            parse("(string @foo)"),
            Ok(Micheline::App(
                Prim::string,
                &[],
                [Annotation::Variable("foo".into())].into()
            ))
        );
        assert_eq!(
            parse("(pair %a (int %b) (int %c))"),
            Ok(Micheline::App(
                Prim::pair,
                &[
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Field("b".into())].into()
                    ),
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Field("c".into())].into()
                    ),
                ],
                [Annotation::Field("a".into())].into()
            ))
        );
        assert_eq!(
            parse("(or %a (int %b) (int %c))"),
            Ok(Micheline::App(
                Prim::or,
                &[
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Field("b".into())].into()
                    ),
                    Micheline::App(
                        Prim::int,
                        &[],
                        [Annotation::Field("c".into())].into()
                    ),
                ],
                [Annotation::Field("a".into())].into()
            ))
        );
        assert_eq!(
            parse("(option %a int %b)")
                .unwrap_err()
                .to_string()
                .as_str()
                .lines()
                .next()
                .unwrap(),
            "Unrecognized token `%b` found at 15:17"
        );
    }

    #[test]
    fn instr_anns() {
        assert_eq!(
            parse("PUSH @var :ty %field int 1").unwrap(),
            Micheline::App(
                Prim::PUSH,
                &[app!(int), 1.into()],
                [
                    Annotation::Variable("var".into()),
                    Annotation::Type("ty".into()),
                    Annotation::Field("field".into())
                ]
                .into()
            ),
        );
        assert_eq!(
            parse("CAR @var :ty %field :ty.2 @var.2 %field.2").unwrap(),
            Micheline::App(
                Prim::CAR,
                &[],
                [
                    Annotation::Variable("var".into()),
                    Annotation::Type("ty".into()),
                    Annotation::Field("field".into()),
                    Annotation::Type("ty.2".into()),
                    Annotation::Variable("var.2".into()),
                    Annotation::Field("field.2".into()),
                ]
                .into()
            ),
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
            app!(PUSH[
                Micheline::App(
                    Prim::contract,
                    &[app!(unit)],
                    [Annotation::Type("ct".into()), Annotation::Field("foo".into())].into(),
                ),
                app!(Unit)
            ])
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
