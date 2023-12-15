/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use super::{Instruction, Micheline};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lambda<'a> {
    Lambda {
        micheline_code: Micheline<'a>,
        code: Vec<Instruction<'a>>,
    },
    LambdaRec {
        micheline_code: Micheline<'a>,
        code: Vec<Instruction<'a>>,
    },
}
