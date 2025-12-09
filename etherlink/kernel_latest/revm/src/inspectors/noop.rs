// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::ContextTr,
    interpreter::{interpreter_types::StackTr, InterpreterTypes},
    primitives::B256,
    Inspector,
};
use tezos_evm_runtime::runtime::Runtime;

use crate::inspectors::EtherlinkInspector;

pub struct NoInspector;

impl<CTX, INTR> Inspector<CTX, INTR> for NoInspector
where
    CTX: ContextTr,
    INTR: InterpreterTypes<Stack: StackTr>,
{
}

impl<'a, Host: Runtime + 'a> EtherlinkInspector<'a, Host> for NoInspector {
    fn is_struct_logger(&self) -> bool {
        false
    }

    fn get_transaction_hash(&self) -> Option<B256> {
        None
    }
}
