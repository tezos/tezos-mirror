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

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParserError {
    #[error("expected a natural from 0 to 1023 inclusive, but got {0}")]
    ExpectedU10(i128),
    #[error(transparent)]
    LexerError(#[from] LexerError),
}

#[allow(dead_code)]
pub fn parse(src: &str) -> Result<ParsedInstructionBlock, ParseError<usize, Tok, ParserError>> {
    syntax::instructionBlockParser::new().parse(spanned_lexer(src))
}

fn spanned_lexer(
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
mod tests {
    use super::*;

    #[test]
    fn pair_type() {
        assert_eq!(
            parse("{ PUSH (pair int nat) Unit }").unwrap(),
            vec![Instruction::Push((
                Type::new_pair(Type::Int, Type::Nat),
                Value::UnitValue
            ))]
        );
        assert_eq!(
            parse("{ PUSH (pair int nat unit) Unit }").unwrap(),
            vec![Instruction::Push((
                Type::new_pair(Type::Int, Type::new_pair(Type::Nat, Type::Unit)),
                Value::UnitValue
            ))]
        );
        assert_eq!(
            parse("{ PUSH (pair (pair int nat) unit) Unit }").unwrap(),
            vec![Instruction::Push((
                Type::new_pair(Type::new_pair(Type::Int, Type::Nat), Type::Unit),
                Value::UnitValue
            ))]
        );
    }

    #[test]
    fn pair_value() {
        assert_eq!(
            parse("{ PUSH unit (Pair 3 4) }").unwrap(),
            vec![Instruction::Push((
                Type::Unit,
                Value::new_pair(Value::NumberValue(3), Value::NumberValue(4)),
            ))]
        );
        assert_eq!(
            parse("{ PUSH unit (Pair 3 4 5) }").unwrap(),
            vec![Instruction::Push((
                Type::Unit,
                Value::new_pair(
                    Value::NumberValue(3),
                    Value::new_pair(Value::NumberValue(4), Value::NumberValue(5)),
                ),
            ))]
        );
        assert_eq!(
            parse("{ PUSH unit (Pair (Pair 3 4) 5) }").unwrap(),
            vec![Instruction::Push((
                Type::Unit,
                Value::new_pair(
                    Value::new_pair(Value::NumberValue(3), Value::NumberValue(4)),
                    Value::NumberValue(5),
                ),
            ))]
        );
        assert!(parse("{ PUSH pair unit unit Pair Unit Unit }")
            .unwrap_err()
            .to_string()
            .starts_with("Unrecognized token `pair` found at 7:11"));
        assert!(parse("{ PUSH (pair unit unit) Pair Unit Unit }")
            .unwrap_err()
            .to_string()
            .starts_with("Unrecognized token `Pair` found at 24:28\n"));
    }

    #[test]
    fn value_parens() {
        assert_eq!(
            parse("{ PUSH unit (Unit) }").unwrap(),
            vec![Instruction::Push((Type::Unit, Value::UnitValue))]
        );
    }

    #[test]
    fn type_anns() {
        use Type as T;
        use Type::*;
        macro_rules! parse_type {
            ($s:expr) => {
                crate::syntax::TypeParser::new().parse(spanned_lexer($s))
            };
        }
        assert_eq!(parse_type!("(int :p)"), Ok(Int));
        assert_eq!(
            parse_type!("(pair :point (int :x_pos) (int :y_pos))"),
            Ok(T::new_pair(Int, Int))
        );
        assert_eq!(parse_type!("(string %foo)"), Ok(String));
        assert_eq!(parse_type!("(string %foo :bar @baz)"), Ok(String));
        assert_eq!(parse_type!("(string @foo)"), Ok(String));
        assert_eq!(
            parse_type!("(pair %a (int %b) (int %c))"),
            Ok(T::new_pair(Int, Int))
        );
        assert_eq!(
            parse_type!("(option %a int %b)")
                .unwrap_err()
                .to_string()
                .as_str(),
            "Unrecognized token `<ann>` found at 15:17\nExpected one of \")\""
        );
    }

    #[test]
    fn instr_anns() {
        use Instruction::*;
        use Type as T;
        use Value as V;
        assert_eq!(
            parse("{PUSH @var :ty %field int 1}").unwrap(),
            vec![Push((T::Int, V::NumberValue(1)))],
        );
        assert_eq!(
            parse("{CAR @var :ty %field :ty.2 @var.2 %field.2}").unwrap(),
            vec![Car],
        );
    }
}
