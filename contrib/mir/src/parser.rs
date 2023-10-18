/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::syntax;
use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParserError {
    #[error("parsing of numeric literal {0} failed")]
    NumericLiteral(String),
    #[error("expected a natural from 0 to 1023 inclusive, but got {0}")]
    ExpectedU10(String),
    #[error("forbidden character found in string literal \"{0}\"")]
    ForbiddenCharacterIn(String),
    #[error("undefined escape sequence: \"\\{0}\"")]
    UndefinedEscape(char),
}

pub fn parse(
    src: &str,
) -> Result<ParsedInstructionBlock, ParseError<usize, Token<'_>, ParserError>> {
    syntax::instructionBlockParser::new().parse(src)
}

/// Takes a string _with_ the sourrounding quotes, strips the quotes, checks the
/// string is valid (i.e. contains only printable ASCII characters) and replaces
/// escapes with corresponding characters.
pub fn validate_unescape_string(
    s: &str,
) -> Result<String, ParseError<usize, Token<'_>, ParserError>> {
    // strip the quotes
    let s = &s[1..s.len() - 1];

    // check if all characters are printable ASCII
    if !s.chars().all(|c| matches!(c, ' '..='~')) {
        return Err(ParserError::ForbiddenCharacterIn(s.to_owned()).into());
    }

    let mut res = String::new();
    // this may overreserve, but no more than 2x
    res.reserve(s.len());

    let unescape_char = |c| match c {
        'n' => Ok('\n'),
        '"' => Ok('"'),
        '\\' => Ok('\\'),
        _ => Err(ParserError::UndefinedEscape(c)),
    };

    let mut in_escape: bool = false;
    for c in s.chars() {
        if in_escape {
            res.push(unescape_char(c)?);
            in_escape = false;
        } else if matches!(c, '\\') {
            in_escape = true;
        } else {
            res.push(c);
        }
    }
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn unescape_string() {
        macro_rules! assert_parse {
            ($s:expr, $e:expr) => {
                assert_eq!(
                    syntax::stringParser::new()
                        .parse($s)
                        .as_deref()
                        .map_err(|e| e.to_string()),
                    $e
                )
            };
        }
        assert_parse!(r#""bar""#, Ok("bar"));
        assert_parse!(r#""foo\nbar""#, Ok("foo\nbar"));
        assert_parse!(r#""foo\\nbar""#, Ok("foo\\nbar"));
        assert_parse!(r#""foo\"bar\"""#, Ok("foo\"bar\""));
        assert_parse!(r#""foo\\nbar""#, Ok("foo\\nbar"));
        assert_parse!(r#""foo\\\\bar""#, Ok("foo\\\\bar"));
        // unicode is not accepted
        assert_parse!(
            r#""हिन्दी""#,
            Err("forbidden character found in string literal \"हिन्दी\"".to_owned())
        );
        // unknown escapes are not accepted
        assert_parse!(
            r#""\a""#,
            Err("undefined escape sequence: \"\\a\"".to_owned())
        );
        // unterminated strings are not accepted
        assert_parse!(r#"""#, Err("Invalid token at 0".to_owned()));
        assert_parse!(r#""\""#, Err("Invalid token at 0".to_owned()));
    }

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
}
