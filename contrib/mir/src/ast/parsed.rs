/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::{Instruction, Stage, Type, Value};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParsedStage {}

impl Stage for ParsedStage {
    type AddMeta = ();
    type PushValue = (Type, Value);
    type NilType = Type;
    type GetOverload = ();
}

pub type ParsedInstruction = Instruction<ParsedStage>;
