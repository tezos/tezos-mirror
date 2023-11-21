/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::Instruction;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lambda {
    Lambda { code: Vec<Instruction> },
    LambdaRec { code: Vec<Instruction> },
}
