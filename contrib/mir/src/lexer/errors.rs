/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("unknown primitive: {0}")]
pub struct PrimError(pub String);

#[derive(Debug, PartialEq, Clone, thiserror::Error, Default)]
pub enum LexerError {
    #[error("unknown token")]
    #[default]
    UnknownToken,
    #[error("parsing of numeric literal {0} failed")]
    NumericLiteral(String),
    #[error("forbidden character found in string literal \"{0}\"")]
    ForbiddenCharacterIn(String),
    #[error("undefined escape sequence: \"\\{0}\"")]
    UndefinedEscape(char),
    #[error(transparent)]
    PrimError(#[from] PrimError),
    #[error("invalid hex sequence: {0}")]
    InvalidHex(#[from] hex::FromHexError),
}
