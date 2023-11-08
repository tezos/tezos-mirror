/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use logos::*;

use super::super::ast::Micheline;
use super::errors::*;

#[derive(Debug)]
pub enum MacroArgs<'a> {
    NoArgs,
    OneArg(Micheline<'a>),
    TwoArgs(Micheline<'a>, Micheline<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
#[logos(error = LexerError)]
pub enum Macro {
    #[token("CMPEQ")]
    CMPEQ,
    #[token("CMPLE")]
    CMPLE,
    #[token("IF_SOME")]
    IF_SOME,
    #[token("IFCMPEQ")]
    IFCMPEQ,
    #[token("IFCMPLE")]
    IFCMPLE,
    #[token("ASSERT")]
    ASSERT,
    #[token("ASSERT_CMPEQ")]
    ASSERT_CMPEQ,
    #[token("ASSERT_CMPLE")]
    ASSERT_CMPLE,
    #[token("FAIL")]
    FAIL,
    #[regex("DII+P", lex_diip)]
    DIIP(u16),
    #[regex("DUU+P", lex_duup)]
    DUUP(u16),
}

pub fn lex_diip<'a>(lex: &mut Lexer<'a, Macro>) -> Result<u16, LexerError> {
    (lex.slice().len() - 2)
        .try_into()
        .map_err(|_| LexerError::UnknownToken)
}

pub fn lex_duup<'a>(lex: &mut Lexer<'a, Macro>) -> Result<u16, LexerError> {
    (lex.slice().len() - 2)
        .try_into()
        .map_err(|_| LexerError::UnknownToken)
}

impl std::fmt::Display for Macro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Macro::DIIP(c) => write!(f, "D{}P", "I".repeat(usize::try_from(*c).unwrap())),
            Macro::DUUP(c) => write!(f, "D{}P", "U".repeat(usize::try_from(*c).unwrap())),
            _ => write!(f, "{:?}", &self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diip_display() {
        assert_eq!(format!("{}", Macro::DIIP(5)), "DIIIIIP");
    }

    #[test]
    fn test_duup_display() {
        assert_eq!(format!("{}", Macro::DUUP(5)), "DUUUUUP");
    }
}
