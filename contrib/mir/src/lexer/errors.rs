/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Errors possible during the lexing stage.

/// Unknown primitive error
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("unknown primitive: {0}")]
pub struct PrimError(pub String);

/// Errors possible during the lexing stage.
#[derive(Debug, PartialEq, Clone, thiserror::Error, Default)]
pub enum LexerError {
    /// Unrecognized token.
    #[error("unknown token")]
    #[default]
    UnknownToken,
    /// Parsing of a numeric literal failed. This shouldn't generally happen
    /// except when parsing tzt.
    #[error("parsing of numeric literal {0} failed")]
    NumericLiteral(String),
    /// Found a forbidden character in a string literal.
    #[error("forbidden character found in string literal \"{0}\"")]
    ForbiddenCharacterIn(String),
    /// Found an undefined escape sequence in a string literal.
    #[error("undefined escape sequence: \"\\{0}\"")]
    UndefinedEscape(char),
    /// Unknown primitive.
    #[error(transparent)]
    PrimError(#[from] PrimError),
    /// Invalid hexadecimal sequence in a byte literal.
    #[error("invalid hex sequence: {0}")]
    InvalidHex(#[from] hex::FromHexError),
}
