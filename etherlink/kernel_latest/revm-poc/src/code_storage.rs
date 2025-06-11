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

use crate::storage_helpers::{bytes_hash, read_u64_le, write_u64_le};

/// Path where EVM codes' are stored.
const EVM_CODES_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/eth_codes");

/// Path to the EVM bytecode.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// Path to the number of accounts that use specific bytecodes.
const REFERENCE_PATH: RefPath = RefPath::assert_from(b"/ref_count");

fn code_hash_path(code_hash: &B256) -> OwnedPath {
    let code_hash_path_string = format!("/{:x}", code_hash);
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

    fn get_ref_count(&self, host: &mut impl Runtime) -> u64 {
        let reference_path = concat(&self.path, &REFERENCE_PATH).unwrap();
        read_u64_le(host, &reference_path)
    }

    fn set_ref_count(&self, host: &mut impl Runtime, number_ref: u64) {
        let reference_path = concat(&self.path, &REFERENCE_PATH).unwrap();
        write_u64_le(host, &reference_path, number_ref)
    }

    fn increment_code_usage(&self, host: &mut impl Runtime) {
        let number_reference = self.get_ref_count(host);
        let number_reference = number_reference.saturating_add(1u64);
        self.set_ref_count(host, number_reference)
    }

    pub fn add(host: &mut impl Runtime, bytecode: &[u8]) -> B256 {
        let code_hash = bytes_hash(bytecode);
        let code = Self::new(&code_hash);
        if code.exists(host) {
            let code_path = concat(&code.path, &CODE_PATH).unwrap();
            host.store_write_all(&code_path, bytecode).unwrap();
        };
        code.increment_code_usage(host);
        code_hash
    }

    pub fn get_code(&self, host: &impl Runtime) -> Bytecode {
        if self.exists(host) {
            let code_path = concat(&self.path, &CODE_PATH).unwrap();
            Bytecode::new_raw_checked(Bytes::from(
                host.store_read_all(&code_path).unwrap_or_default(),
            ))
            .unwrap()
        } else {
            // TODO: Double check that legacy code with one STOP instruction
            // is ok here.
            Bytecode::new()
        }
    }
}
