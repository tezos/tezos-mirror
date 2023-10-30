/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::context::*;
use crate::tzt::*;

pub fn construct_context(test: &TztTest) -> Ctx {
    Ctx {
        gas: crate::gas::Gas::default(),
        amount: test.amount.unwrap_or_default(),
        chain_id: Ctx::default().chain_id,
    }
}
