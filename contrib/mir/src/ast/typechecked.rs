/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::{Instruction, Stage};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypecheckedStage {}

impl Stage for TypecheckedStage {
    type AddMeta = ();
}

pub type TypecheckedInstruction = Instruction<TypecheckedStage>;
