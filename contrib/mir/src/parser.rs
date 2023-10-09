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

pub fn parse(
    src: &str,
) -> Result<ParsedInstructionBlock, ParseError<usize, Token<'_>, &'static str>> {
    syntax::instructionBlockParser::new().parse(src)
}
