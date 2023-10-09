/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::{Instruction, Stage};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParsedStage {}

impl Stage for ParsedStage {
    type AddMeta = ();
}

pub type ParsedInstruction = Instruction<ParsedStage>;
