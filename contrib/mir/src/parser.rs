/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::*;
use crate::lexer::{LexerError, Prim, Tok};
use crate::syntax;
use lalrpop_util::ParseError;
use logos::Logos;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParserError {
    #[error("expected a natural from 0 to 1023 inclusive, but got {0}")]
    ExpectedU10(i128),
    #[error(transparent)]
    LexerError(#[from] LexerError),
    #[error("no {0} field")]
    NoField(Prim),
    #[error("duplicate {0} field")]
    DuplicateField(Prim),
}

pub fn parse(src: &str) -> Result<ParsedInstruction, ParseError<usize, Tok, ParserError>> {
    syntax::InstructionParser::new().parse(spanned_lexer(src))
}

pub fn parse_contract_script(
    src: &str,
) -> Result<ContractScript<ParsedStage>, ParseError<usize, Tok, ParserError>> {
    syntax::ContractScriptParser::new().parse(spanned_lexer(src))
}

/// Helper type to parse contract fields
pub enum ContractScriptEntity {
    Parameter(Type),
    Storage(Type),
    Code(ParsedInstruction),
}

impl TryFrom<Vec<ContractScriptEntity>> for ContractScript<ParsedStage> {
    type Error = crate::parser::ParserError;
    fn try_from(value: Vec<ContractScriptEntity>) -> Result<Self, Self::Error> {
        use crate::lexer::Prim as P;
        use crate::parser::ParserError as Err;
        use ContractScriptEntity as CE;
        let mut param: Option<Type> = None;
        let mut storage: Option<Type> = None;
        let mut code: Option<ParsedInstruction> = None;
        fn set_if_none<T>(x: &mut Option<T>, y: T, e: P) -> Result<(), Err> {
            if x.is_none() {
                *x = Some(y);
                Ok(())
            } else {
                Err(Err::DuplicateField(e))
            }
        }
        for i in value {
            match i {
                CE::Parameter(p) => set_if_none(&mut param, p, P::parameter),
                CE::Storage(p) => set_if_none(&mut storage, p, P::storage),
                CE::Code(p) => set_if_none(&mut code, p, P::code),
            }?;
        }
        Ok(ContractScript {
            parameter: param.ok_or(Err::NoField(P::parameter))?,
            storage: storage.ok_or(Err::NoField(P::storage))?,
            code: code.ok_or(Err::NoField(P::code))?,
        })
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

/// Validate a number is a 10-bit unsigned integer.
pub fn validate_u10(n: i128) -> Result<u16, ParserError> {
    let res = u16::try_from(n).map_err(|_| ParserError::ExpectedU10(n))?;
    if res >= 1024 {
        return Err(ParserError::ExpectedU10(n));
    }
    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pair_type() {
        assert_eq!(
            parse("PUSH (pair int nat) Unit").unwrap(),
            Instruction::Push((Type::new_pair(Type::Int, Type::Nat), Value::Unit))
        );
        assert_eq!(
            parse("PUSH (pair int nat unit) Unit").unwrap(),
            Instruction::Push((
                Type::new_pair(Type::Int, Type::new_pair(Type::Nat, Type::Unit)),
                Value::Unit
            ))
        );
        assert_eq!(
            parse("PUSH (pair (pair int nat) unit) Unit").unwrap(),
            Instruction::Push((
                Type::new_pair(Type::new_pair(Type::Int, Type::Nat), Type::Unit),
                Value::Unit
            ))
        );
    }

    #[test]
    fn or_type() {
        assert_eq!(
            parse("PUSH (or int nat) Unit").unwrap(),
            Instruction::Push((Type::new_or(Type::Int, Type::Nat), Value::Unit))
        );
        // unlike for pairs, there's no linearized syntax for `or`
        assert_eq!(
            parse("{ PUSH (or int nat unit) Unit }")
                .unwrap_err()
                .to_string(),
            "Unrecognized token `unit` found at 19:23\nExpected one of \")\""
        );
    }

    #[test]
    fn pair_value() {
        assert_eq!(
            parse("PUSH unit (Pair 3 4)").unwrap(),
            Instruction::Push((
                Type::Unit,
                Value::new_pair(Value::Number(3), Value::Number(4)),
            ))
        );
        assert_eq!(
            parse("PUSH unit (Pair 3 4 5)").unwrap(),
            Instruction::Push((
                Type::Unit,
                Value::new_pair(
                    Value::Number(3),
                    Value::new_pair(Value::Number(4), Value::Number(5)),
                ),
            ))
        );
        assert_eq!(
            parse("PUSH unit (Pair (Pair 3 4) 5)").unwrap(),
            Instruction::Push((
                Type::Unit,
                Value::new_pair(
                    Value::new_pair(Value::Number(3), Value::Number(4)),
                    Value::Number(5),
                ),
            ))
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
    fn or_value() {
        assert_eq!(
            parse("PUSH (or int unit) (Left 3)").unwrap(),
            Instruction::Push((
                Type::new_or(Type::Int, Type::Unit),
                Value::new_or(Or::Left(Value::Number(3))),
            ))
        );
    }

    #[test]
    fn value_parens() {
        assert_eq!(
            parse("PUSH unit (Unit)").unwrap(),
            Instruction::Push((Type::Unit, Value::Unit))
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
            parse_type!("(or %a (int %b) (int %c))"),
            Ok(T::new_or(Int, Int))
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
            parse("PUSH @var :ty %field int 1").unwrap(),
            Push((T::Int, V::Number(1))),
        );
        assert_eq!(
            parse("CAR @var :ty %field :ty.2 @var.2 %field.2").unwrap(),
            Car,
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
        use crate::lexer::Prim::{code, parameter, storage};
        use Instruction::*;
        use ParserError as Err;
        use Type as T;

        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code FAILWITH"),
            Ok(ContractScript {
                parameter: T::Unit,
                storage: T::Unit,
                code: Failwith(())
            })
        );

        // non-atomic arguments to code must be wrapped in braces
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code NIL unit")
                .unwrap_err()
                .to_string()
                .lines()
                .next(),
            Some("Unrecognized token `NIL` found at 35:38")
        );
        // or parentheses
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; code (NIL unit)"),
            Ok(ContractScript {
                parameter: T::Unit,
                storage: T::Unit,
                code: Nil(T::Unit),
            })
        );
        // duplicate
        assert_eq!(
            parse_contract_script("parameter unit; parameter int; storage unit; code FAILWITH"),
            Err(Err::DuplicateField(parameter).into())
        );
        assert_eq!(
            parse_contract_script("parameter unit; storage unit; storage int; code FAILWITH"),
            Err(Err::DuplicateField(storage).into())
        );
        assert_eq!(
            parse_contract_script("code INT; parameter unit; storage unit; code FAILWITH"),
            Err(Err::DuplicateField(code).into())
        );
        // missing
        assert_eq!(
            parse_contract_script("storage unit; code FAILWITH"),
            Err(Err::NoField(parameter).into())
        );
        assert_eq!(
            parse_contract_script("parameter unit; code FAILWITH"),
            Err(Err::NoField(storage).into())
        );
        assert_eq!(
            parse_contract_script("parameter unit; storage unit"),
            Err(Err::NoField(code).into())
        );
    }

    #[test]
    // Test parsing of annotated contract type.
    // Type contract is not pushable so this would be rejected
    // at type-checking but this test only exercises the parser.
    fn contract_ty_push() {
        assert_eq!(
            parse("PUSH (contract :ct %foo unit) Unit").unwrap(),
            Instruction::Push((Type::new_contract(Type::Unit), Value::Unit))
        );
    }
}
