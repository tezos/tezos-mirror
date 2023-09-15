/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::syntax;

pub fn parse(src: &str) -> bool {
    Result::is_ok(&syntax::instructionBlockParser::new().parse(src))
}
