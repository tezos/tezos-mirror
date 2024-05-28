/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Utilities for lexing macros.

use logos::*;

use super::super::ast::Micheline;
use super::errors::*;

/// Arguments, to which a macro is applied.
#[derive(Debug)]
pub enum MacroArgs<'a> {
    /// A macro was applied to no arguments.
    NoArgs,
    /// A macro is applied to one argument.
    OneArg(Micheline<'a>),
    /// A macro is applied to two arguments.
    TwoArgs(Micheline<'a>, Micheline<'a>),
}

/// Enum representing macro names.
#[derive(Debug, Clone, PartialEq, Eq, Logos)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms, missing_docs)]
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
    /// Corresponds to `DI..IP` macro. The value carried by the variant
    /// corresponds to the number of `I`s.
    #[regex("DII+P", lex_diip)]
    DIIP(u16),
    /// Corresponds to `DU..UP` macro. The value carried by the variant
    /// corresponds to the number of `U`s.
    #[regex("DUU+P", lex_duup)]
    DUUP(u16),
}

fn lex_diip(lex: &mut Lexer<Macro>) -> Result<u16, LexerError> {
    (lex.slice().len() - 2)
        .try_into()
        .map_err(|_| LexerError::UnknownToken)
}

fn lex_duup(lex: &mut Lexer<Macro>) -> Result<u16, LexerError> {
    (lex.slice().len() - 2)
        .try_into()
        .map_err(|_| LexerError::UnknownToken)
}

impl std::fmt::Display for Macro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Macro::DIIP(c) => write!(f, "D{}P", "I".repeat(usize::from(*c))),
            Macro::DUUP(c) => write!(f, "D{}P", "U".repeat(usize::from(*c))),
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
