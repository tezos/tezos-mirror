// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

use crate::types::EvmTree;

#[ocaml::func]
#[ocaml::sig("Irmin_context.tree -> Irmin_context.tree")]
pub fn wasm_runtime_id(evm_tree: EvmTree) -> EvmTree {
    evm_tree
}
