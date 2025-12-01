// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum code storage

use revm::{
    primitives::{hex::FromHex, Bytes, B256},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::{
    path::{OwnedPath, RefPath},
    runtime::RuntimeError,
};

use crate::{
    helpers::storage::{bytes_hash, concat, read_u64_le_default, write_u64_le},
    precompiles::constants::{
        FA_BRIDGE_SOL_CODE_HASH, FA_BRIDGE_SOL_CONTRACT,
        INTERNAL_FORWARDER_SOL_CODE_HASH, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_CODE_HASH, WITHDRAWAL_SOL_CONTRACT,
    },
    Error,
};

/// Path where EVM codes' are stored.
pub(crate) const EVM_CODES_PATH: RefPath =
    RefPath::assert_from(b"/evm/world_state/eth_codes");

/// Path to the EVM bytecode.
const CODE_PATH: RefPath = RefPath::assert_from(b"/code");

/// Path to the number of accounts that use specific bytecodes.
const REFERENCE_PATH: RefPath = RefPath::assert_from(b"/ref_count");

fn code_hash_path(code_hash: &B256) -> Result<OwnedPath, Error> {
    let code_hash_path_string = format!("/{code_hash:x}");
    OwnedPath::try_from(code_hash_path_string)
        .map_err(|err| Error::Custom(err.to_string()))
}

#[derive(Debug)]
pub struct CodeStorage {
    path: OwnedPath,
    hash: B256,
}

impl CodeStorage {
    pub fn new(code_hash: &B256) -> Result<Self, Error> {
        let code_hash_path = code_hash_path(code_hash)?;
        let path = concat(&EVM_CODES_PATH, &code_hash_path)?;
        Ok(Self {
            path,
            hash: *code_hash,
        })
    }

    fn exists(&self, host: &impl Runtime) -> Result<bool, Error> {
        let store_has_code = host.store_has(&concat(&self.path, &CODE_PATH)?)?;
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

    pub fn add(
        host: &mut impl Runtime,
        bytecode: &[u8],
        code_hash: Option<B256>,
    ) -> Result<B256, Error> {
        let code_hash = code_hash.unwrap_or_else(|| bytes_hash(bytecode));
        let code = Self::new(&code_hash)?;
        if !code.exists(host)? {
            let code_path = concat(&code.path, &CODE_PATH)?;
            host.store_write(&code_path, bytecode, 0)?;
        };
        code.increment_code_usage(host)?;
        Ok(code_hash)
    }

    pub fn get_code(&self, host: &impl Runtime) -> Result<Option<Bytecode>, Error> {
        if let Some(code) = get_precompile_bytecode(&self.hash)? {
            return Ok(Some(code));
        }
        let code_path = concat(&self.path, &CODE_PATH)?;
        match host.store_read_all(&code_path) {
            Ok(code_bytes) => Bytecode::new_raw_checked(Bytes::from(code_bytes))
                .map(Some)
                .map_err(|err| Error::Custom(err.to_string())),
            Err(RuntimeError::PathNotFound) => Ok(None),
            Err(err) => Err(Error::Runtime(err)),
        }
    }

    fn decrement_code_usage(&self, host: &mut impl Runtime) -> Result<u64, Error> {
        let mut number_reference = self.get_ref_count(host)?;
        if number_reference != 0 {
            // Condition avoids an unnecessary write access
            number_reference = number_reference.saturating_sub(1u64);
            self.set_ref_count(host, number_reference)?;
        }
        Ok(number_reference)
    }

    #[allow(dead_code)]
    pub fn delete(host: &mut impl Runtime, code_hash: &B256) -> Result<(), Error> {
        let code = Self::new(code_hash)?;
        if code.exists(host)? {
            let number_reference = code.decrement_code_usage(host)?;
            // This was the last smart contract using this code
            if number_reference == 0 {
                host.store_delete(&concat(&code.path, &CODE_PATH)?)?;
                host.store_delete(&concat(&code.path, &REFERENCE_PATH)?)?;
            };
        };
        Ok(())
    }
}

pub fn get_precompile_bytecode(code_hash: &B256) -> Result<Option<Bytecode>, Error> {
    if code_hash == &WITHDRAWAL_SOL_CODE_HASH {
        Ok(Some(Bytecode::new_legacy(
            Bytes::from_hex(WITHDRAWAL_SOL_CONTRACT)
                .map_err(|err| Error::Custom(err.to_string()))?,
        )))
    } else if code_hash == &FA_BRIDGE_SOL_CODE_HASH {
        Ok(Some(Bytecode::new_legacy(
            Bytes::from_hex(FA_BRIDGE_SOL_CONTRACT)
                .map_err(|err| Error::Custom(err.to_string()))?,
        )))
    } else if code_hash == &INTERNAL_FORWARDER_SOL_CODE_HASH {
        Ok(Some(Bytecode::new_legacy(
            Bytes::from_hex(INTERNAL_FORWARDER_SOL_CONTRACT)
                .map_err(|err| Error::Custom(err.to_string()))?,
        )))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use super::{CodeStorage, REFERENCE_PATH};
    use crate::helpers::storage::{concat, read_u64_le};

    use revm::{
        primitives::{Bytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use tezos_evm_runtime::runtime::MockKernelHost;

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut host = MockKernelHost::default();
        let empty_code: Vec<u8> = vec![];

        let found_code_hash = CodeStorage::add(&mut host, &empty_code, None)
            .expect("Could not create code storage");

        assert_eq!(found_code_hash, KECCAK_EMPTY);
    }

    #[test]
    fn test_get_code_matches_given() {
        let mut host = MockKernelHost::default();
        let code: Vec<u8> = (0..100).collect();
        let code_hash = CodeStorage::add(&mut host, &code, None)
            .expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not create code storage");
        let found_code = code_storage
            .get_code(&host)
            .expect("Could not retrieve code")
            .expect("Code should exist");
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
        let code_hash = CodeStorage::add(&mut host, &code, None)
            .expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not find code storage");
        let ref_path = concat(&code_storage.path, &REFERENCE_PATH).unwrap();

        let ref_count = read_u64_le(&host, &ref_path).expect("Reference count not found");
        assert_eq!(ref_count, 1u64);

        let second_code_hash = CodeStorage::add(&mut host, &code, None)
            .expect("Could not create code storage");

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
        let _code_hash = CodeStorage::add(&mut host, &code, None)
            .expect("Could not create code storage");

        let exists = code_storage
            .exists(&host)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let _code_hash = CodeStorage::add(&mut host, &code, None)
            .expect("Could not create code storage");

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
        let code_hash = CodeStorage::add(&mut host, &code, None).unwrap();
        let code_storage = CodeStorage::new(&code_hash).unwrap();
        CodeStorage::delete(&mut host, &code_hash).unwrap();
        let empty_code = code_storage.get_code(&host).unwrap();

        assert_eq!(empty_code, None);
    }
}
