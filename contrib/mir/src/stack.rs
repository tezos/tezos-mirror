/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::VecDeque;

use crate::ast::*;

pub type TypeStack = VecDeque<Type>;
pub type IStack = VecDeque<Value>;
