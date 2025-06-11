// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum code storage

use revm::{
    primitives::{Bytes, B256},
    state::Bytecode,
};
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, RefPath},
    runtime::Runtime,
};

/// Path where EVM codes' are stored.
const EVM_CODES_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/eth_codes");

/// Path to the EVM bytecode.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

fn code_hash_path(code_hash: &B256) -> OwnedPath {
    let code_hash_path_string = format!("/{}", code_hash);
    OwnedPath::try_from(code_hash_path_string).unwrap()
}

#[derive(Debug, PartialEq)]
pub struct CodeStorage {
    path: OwnedPath,
}

impl From<OwnedPath> for CodeStorage {
    fn from(path: OwnedPath) -> Self {
        Self { path }
    }
}

impl CodeStorage {
    pub fn new(code_hash: &B256) -> Self {
        let code_hash_path = code_hash_path(code_hash);
        let path = concat(&EVM_CODES_PATH, &code_hash_path).unwrap();
        Self { path }
    }

    fn exists(&self, host: &impl Runtime) -> bool {
        let store_has_code = host.store_has(&self.path).unwrap();
        store_has_code.is_some()
    }

    pub fn get_code(&self, host: &impl Runtime) -> Bytecode {
        if self.exists(host) {
            let code1_path = concat(&self.path, &CODE_PATH).unwrap();
            Bytecode::new_raw_checked(Bytes::from(
                host.store_read_all(&code1_path).unwrap(),
            ))
            .unwrap()
        } else {
            // TODO: Double check that legacy code with one STOP instruction
            // is ok here.
            Bytecode::new()
        }
    }
}
