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
            // `Bytecode::new()` creates a new legacy analyzed bytecode with exactly
            // one STOP (`0x00`) opcode.
            // This is a bit counter-intuitive as if the code doesn't exist we would
            // expect the code to be empty to match the code hash (KECCAK_EMPTY).
            // This is an internal mechanism of REVM needed for `LegacyAnalyzedBytecode`.
            Ok(Bytecode::new())
        }
    }

    #[cfg(test)]
    pub fn decrement_code_usage(&self, host: &mut impl Runtime) -> Result<u64, Error> {
        let number_reference = self.get_ref_count(host)?;
        let number_reference = number_reference.saturating_sub(1u64);
        self.set_ref_count(host, number_reference)?;
        Ok(number_reference)
    }

    #[cfg(test)]
    pub fn delete(host: &mut impl Runtime, code_hash: &B256) -> Result<(), Error> {
        let code = Self::new(code_hash)?;
        if code.exists(host)? {
            let number_reference = code.decrement_code_usage(host)?;
            // This was the last smart contract using this code
            if number_reference == 0 {
                host.store_delete(&code.path)?;
            };
        };
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{CodeStorage, REFERENCE_PATH};
    use crate::storage_helpers::{concat, read_u64_le};

    use revm::{
        primitives::{Bytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut host = MockKernelHost::default();
        let empty_code: Vec<u8> = vec![];

        let found_code_hash = CodeStorage::add(&mut host, &empty_code)
            .expect("Could not create code storage");

        assert_eq!(found_code_hash, KECCAK_EMPTY);
    }

    #[test]
    fn test_get_code_matches_given() {
        let mut host = MockKernelHost::default();
        let code: Vec<u8> = (0..100).collect();
        let code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not create code storage");
        let found_code = code_storage
            .get_code(&host)
            .expect("Could not retrieve code");
        assert_eq!(
            found_code,
            Bytecode::new_raw_checked(Bytes::from(code))
                .expect("Bytecode should be decodable")
        );
    }

    #[test]
    fn test_code_ref_is_incremented() {
        let mut host = MockKernelHost::default();
        let code: Vec<u8> = (0..100).collect();
        let code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not find code storage");
        let ref_path = concat(&code_storage.path, &REFERENCE_PATH).unwrap();

        let ref_count = read_u64_le(&host, &ref_path).expect("Reference count not found");
        assert_eq!(ref_count, 1u64);

        let second_code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        assert_eq!(second_code_hash, code_hash);

        let ref_count = read_u64_le(&host, &ref_path).expect("Reference count not found");
        assert_eq!(ref_count, 2u64);

        let () = CodeStorage::delete(&mut host, &code_hash)
            .expect("Could not delete code storage");

        let ref_count = read_u64_le(&host, &ref_path).expect("Reference count not found");
        assert_eq!(ref_count, 1u64);
    }

    #[test]
    fn test_code_is_deleted() {
        let mut host = MockKernelHost::default();

        let code_storage =
            CodeStorage::new(&KECCAK_EMPTY).expect("Could not find code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exists");

        assert!(!exists, "code storage should not exist");

        let code: Vec<u8> = vec![];
        let _code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let _code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let () = CodeStorage::delete(&mut host, &KECCAK_EMPTY)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let () = CodeStorage::delete(&mut host, &KECCAK_EMPTY)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exist");
        assert!(!exists, "code storage should not exists");
    }

    #[test]
    fn test_get_code_from_non_existing_code() {
        let mut host = MockKernelHost::default();

        let code: Vec<u8> = (0..100).collect();
        let code_hash = CodeStorage::add(&mut host, &code).unwrap();
        let code_storage = CodeStorage::new(&code_hash).unwrap();
        CodeStorage::delete(&mut host, &code_hash).unwrap();
        let empty_code = code_storage.get_code(&host).unwrap();
        let empty_hash = empty_code.hash_slow();

        assert_eq!(empty_code, Bytecode::new());
        assert_eq!(empty_hash, KECCAK_EMPTY);
    }
}
