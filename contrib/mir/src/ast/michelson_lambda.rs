/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::rc::Rc;

use super::{Instruction, Micheline};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lambda<'a> {
    Lambda {
        micheline_code: Micheline<'a>,
        code: Rc<[Instruction<'a>]>, // see Note: Rc in lambdas
    },
    LambdaRec {
        micheline_code: Micheline<'a>,
        code: Rc<[Instruction<'a>]>, // see Note: Rc in lambdas
    },
}

/* *** Note: Rc in lambdas ***

When interpreting recursive lambdas, the lambda itself is used as the argument
of its body, which means we'd need two copies of the body if we used `Vec`. To
avoid potentially costly cloning, we use an `Rc` instead.

For non-recursive Lambda the same approach is used to make `LAMBDA` instruction
independent of the length of the code (there's a Lambda::clone call in the
implementation)
*/
