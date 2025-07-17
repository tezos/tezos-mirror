// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::ContextTr,
    interpreter::{interpreter_types::StackTr, InterpreterTypes},
    Inspector,
};

pub struct NoInspector;

impl<CTX, INTR> Inspector<CTX, INTR> for NoInspector
where
    CTX: ContextTr,
    INTR: InterpreterTypes<Stack: StackTr>,
{
}
