// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum code storage

use revm::{
    primitives::{Bytes, B256},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::{OwnedPath, RefPath};

use crate::{
    storage_helpers::{bytes_hash, concat, read_u64_le_default, write_u64_le},
    Error,
};

/// Path where EVM codes' are stored.
const EVM_CODES_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/eth_codes");

/// Path to the EVM bytecode.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// Path to the number of accounts that use specific bytecodes.
const REFERENCE_PATH: RefPath = RefPath::assert_from(b"/ref_count");

fn code_hash_path(code_hash: &B256) -> Result<OwnedPath, Error> {
    let code_hash_path_string = format!("/{:x}", code_hash);
    OwnedPath::try_from(code_hash_path_string)
        .map_err(|err| Error::Custom(err.to_string()))
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
    pub fn new(code_hash: &B256) -> Result<Self, Error> {
        let code_hash_path = code_hash_path(code_hash)?;
        let path = concat(&EVM_CODES_PATH, &code_hash_path)?;
        Ok(Self { path })
    }

    fn exists(&self, host: &impl Runtime) -> Result<bool, Error> {
        let store_has_code = host.store_has(&self.path)?;
        Ok(store_has_code.is_some())
    }

    fn get_ref_count(&self, host: &mut impl Runtime) -> Result<u64, Error> {
        let reference_path = concat(&self.path, &REFERENCE_PATH)?;
        read_u64_le_default(host, &reference_path, 0)
    }

    fn set_ref_count(
        &self,
        host: &mut impl Runtime,
        number_ref: u64,
    ) -> Result<(), Error> {
        let reference_path = concat(&self.path, &REFERENCE_PATH)?;
        Ok(write_u64_le(host, &reference_path, number_ref)?)
    }

    fn increment_code_usage(&self, host: &mut impl Runtime) -> Result<(), Error> {
        let number_reference = self.get_ref_count(host)?;
        let number_reference = number_reference.saturating_add(1u64);
        self.set_ref_count(host, number_reference)
    }

    pub fn add(host: &mut impl Runtime, bytecode: &[u8]) -> Result<B256, Error> {
        let code_hash = bytes_hash(bytecode);
        let code = Self::new(&code_hash)?;
        if !code.exists(host)? {
            let code_path = concat(&code.path, &CODE_PATH)?;
            host.store_write_all(&code_path, bytecode)?;
        };
        code.increment_code_usage(host)?;
        Ok(code_hash)
    }

    pub fn get_code(&self, host: &impl Runtime) -> Result<Bytecode, Error> {
        if self.exists(host)? {
            let code_path = concat(&self.path, &CODE_PATH)?;
            Bytecode::new_raw_checked(Bytes::from(host.store_read_all(&code_path)?))
                .map_err(|err| Error::Custom(err.to_string()))
        } else {
            // TODO: Double check that legacy code with one STOP instruction
            // is ok here.
            Ok(Bytecode::new())
        }
    }
}
