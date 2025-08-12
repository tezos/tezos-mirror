// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum code storage

use host::path::{concat, OwnedPath, RefPath};
use primitive_types::{H256, U256};
use tezos_evm_runtime::runtime::Runtime;
use tezos_storage::helpers::bytes_hash;
use tezos_storage::{error::Error as GenStorageError, read_u64_le, write_u64_le};

/// Path where Ethereum account's code are stored
const EVM_CODES_PATH: RefPath = RefPath::assert_from(b"/evm/world_state/eth_codes");

/// Path to the number of accounts to use a particular code
const REFERENCE_PATH: RefPath = RefPath::assert_from(b"/ref_count");

/// Path to the code
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

fn code_hash_path(code_hash: &H256) -> Result<OwnedPath, GenStorageError> {
    let code_hash_hex = hex::encode(code_hash);
    let code_hash_path_string = format!("/{code_hash_hex}");
    OwnedPath::try_from(code_hash_path_string).map_err(Into::into)
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
    fn new(code_hash: &H256) -> Result<Self, GenStorageError> {
        let code_hash_path = code_hash_path(code_hash)?;
        let path = concat(&EVM_CODES_PATH, &code_hash_path)?;
        Ok(Self { path })
    }

    fn exists(&self, host: &impl Runtime) -> Result<bool, GenStorageError> {
        let store_has_code = host.store_has(&self.path)?;
        Ok(store_has_code.is_some())
    }

    fn get_ref_count(&self, host: &mut impl Runtime) -> Result<u64, GenStorageError> {
        let reference_path = concat(&self.path, &REFERENCE_PATH)?;
        read_u64_le(host, &reference_path).or(Ok(0u64))
    }

    fn set_ref_count(
        &self,
        host: &mut impl Runtime,
        number_ref: u64,
    ) -> Result<(), GenStorageError> {
        let reference_path = concat(&self.path, &REFERENCE_PATH)?;
        write_u64_le(host, &reference_path, number_ref)?;
        Ok(())
    }

    fn increment_code_usage(
        &self,
        host: &mut impl Runtime,
    ) -> Result<(), GenStorageError> {
        let number_reference = self.get_ref_count(host)?;
        let number_reference = number_reference.saturating_add(1u64);
        self.set_ref_count(host, number_reference)
    }

    fn decrement_code_usage(
        &self,
        host: &mut impl Runtime,
    ) -> Result<u64, GenStorageError> {
        let number_reference = self.get_ref_count(host)?;
        let number_reference = number_reference.saturating_sub(1u64);
        self.set_ref_count(host, number_reference)?;
        Ok(number_reference)
    }

    pub fn add(
        host: &mut impl Runtime,
        bytecode: &[u8],
    ) -> Result<H256, GenStorageError> {
        let code_hash: H256 = bytes_hash(bytecode);
        let code = Self::new(&code_hash)?;
        if !code.exists(host)? {
            let code_path = concat(&code.path, &CODE_PATH)?;
            host.store_write_all(&code_path, bytecode)?;
        };
        code.increment_code_usage(host)?;
        Ok(code_hash)
    }

    pub fn delete(
        host: &mut impl Runtime,
        code_hash: &H256,
    ) -> Result<(), GenStorageError> {
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

    pub fn get_code(
        host: &impl Runtime,
        code_hash: &H256,
    ) -> Result<Vec<u8>, GenStorageError> {
        let code = Self::new(code_hash)?;
        if code.exists(host)? {
            let code1_path = concat(&code.path, &CODE_PATH)?;
            host.store_read_all(&code1_path).map_err(Into::into)
        } else {
            Ok(vec![])
        }
    }

    pub fn code_size(
        host: &impl Runtime,
        code_hash: &H256,
    ) -> Result<U256, GenStorageError> {
        let code = Self::new(code_hash)?;
        if code.exists(host)? {
            let code_path = concat(&code.path, &CODE_PATH)?;
            host.store_value_size(&code_path)
                .map(U256::from)
                .map_err(Into::into)
        } else {
            Ok(U256::zero())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::account_storage;
    use tezos_evm_runtime::runtime::MockKernelHost;

    use super::*;

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut host = MockKernelHost::default();
        let empty_code: Vec<u8> = vec![];
        let empty_code_hash: H256 = account_storage::CODE_HASH_DEFAULT;

        let found_code_hash = CodeStorage::add(&mut host, &empty_code)
            .expect("Could not create code storage");

        assert_eq!(found_code_hash, empty_code_hash);
    }

    #[test]
    fn test_get_code_matches_given() {
        let mut host = MockKernelHost::default();
        let code: Vec<u8> = (0..100).collect();
        let code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");
        let found_code = CodeStorage::get_code(&host, &code_hash).expect("");
        assert_eq!(found_code, code);
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

        let ref_count = read_u64_le(&host, &ref_path).expect("reference count not found");
        assert_eq!(ref_count, 1u64);

        let second_code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        assert_eq!(second_code_hash, code_hash);

        let ref_count = read_u64_le(&host, &ref_path).expect("reference count not found");
        assert_eq!(ref_count, 2u64);

        let () = CodeStorage::delete(&mut host, &code_hash)
            .expect("Could not delete code storage");

        let ref_count = read_u64_le(&host, &ref_path).expect("reference count not found");
        assert_eq!(ref_count, 1u64);
    }

    #[test]
    fn test_code_is_deleted() {
        let mut host = MockKernelHost::default();
        let code_hash: H256 = account_storage::CODE_HASH_DEFAULT;

        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not find code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check contract exists");

        assert!(!exists, "code storage should not exists");

        let code: Vec<u8> = vec![];
        let _code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check contract exists");
        assert!(exists, "code storage should exists");

        let _code_hash =
            CodeStorage::add(&mut host, &code).expect("Could not create code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check contract exists");
        assert!(exists, "code storage should exists");

        let () = CodeStorage::delete(&mut host, &code_hash)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check contract exists");
        assert!(exists, "code storage should exists");

        let () = CodeStorage::delete(&mut host, &code_hash)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check contract exists");
        assert!(!exists, "code storage should not exists");
    }
}
