// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023, 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Ethereum code storage

use revm::{
    primitives::{Bytes, B256},
    state::Bytecode,
};
use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;
use tezos_smart_rollup_keyspace::{Key, KeyError, KeySpace};

use crate::error::EvmDbError;

use crate::{
    helpers::storage::bytes_hash,
    precompiles::constants::{
        FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT, XTZ_BRIDGE_SOL_CONTRACT,
    },
};

/// Key where EVM codes' are stored, relative to the accounts keyspace.
pub(crate) const EVM_CODES: Key = Key::from_static(b"/eth_codes");

/// Key of the EVM bytecode.
const CODE: Key = Key::from_static(b"/code");

/// Key of the number of accounts that use specific bytecodes.
const REFERENCE: Key = Key::from_static(b"/ref_count");

fn code_hash_key(code_hash: &B256) -> Result<Key, KeyError> {
    EVM_CODES.concat(format!("/{code_hash:x}"))
}

#[derive(Debug)]
pub struct CodeStorage {
    /// This bytecode's subtree, relative to the accounts keyspace.
    key: Key,
    hash: B256,
}

impl CodeStorage {
    pub fn new(code_hash: &B256) -> Result<Self, EvmDbError> {
        Ok(Self {
            key: code_hash_key(code_hash)?,
            hash: *code_hash,
        })
    }

    /// Key of `segment` under this bytecode.
    fn segment_key(&self, segment: &Key) -> Result<Key, KeyError> {
        self.key.concat(segment)
    }

    fn exists(&self, eth_accounts: &impl KeySpace) -> Result<bool, EvmDbError> {
        Ok(eth_accounts.contains(&self.segment_key(&CODE)?))
    }

    fn get_ref_count(&self, eth_accounts: &impl KeySpace) -> Result<u64, EvmDbError> {
        Ok(eth_accounts.get_le_or(&self.segment_key(&REFERENCE)?, 0))
    }

    fn set_ref_count(
        &self,
        eth_accounts: &mut impl KeySpace,
        number_ref: u64,
    ) -> Result<(), EvmDbError> {
        Ok(eth_accounts.store_le(&self.segment_key(&REFERENCE)?, number_ref)?)
    }

    fn increment_code_usage(
        &self,
        eth_accounts: &mut impl KeySpace,
    ) -> Result<(), EvmDbError> {
        let number_reference = self.get_ref_count(eth_accounts)?;
        let number_reference = number_reference.saturating_add(1u64);
        self.set_ref_count(eth_accounts, number_reference)
    }

    pub fn add(
        eth_accounts: &mut impl KeySpace,
        bytecode: &[u8],
        code_hash: Option<B256>,
    ) -> Result<B256, EvmDbError> {
        let code_hash = code_hash.unwrap_or_else(|| bytes_hash(bytecode));
        let code = Self::new(&code_hash)?;
        if !code.exists(eth_accounts)? {
            eth_accounts.write(&code.segment_key(&CODE)?, 0, bytecode)?;
        };
        code.increment_code_usage(eth_accounts)?;
        Ok(code_hash)
    }

    pub fn get_code(
        &self,
        eth_accounts: &impl KeySpace,
    ) -> Result<Option<Bytecode>, EvmDbError> {
        if let Some(code) = get_precompile_bytecode(&self.hash)? {
            return Ok(Some(code));
        }
        match eth_accounts.get(&self.segment_key(&CODE)?) {
            Some(code_bytes) => Bytecode::new_raw_checked(Bytes::from(code_bytes))
                .map(Some)
                .map_err(|source| EvmDbError::InvalidBytecode {
                    hash: self.hash,
                    source,
                }),
            None => Ok(None),
        }
    }

    fn decrement_code_usage(
        &self,
        eth_accounts: &mut impl KeySpace,
    ) -> Result<u64, EvmDbError> {
        let mut number_reference = self.get_ref_count(eth_accounts)?;
        if number_reference != 0 {
            // Condition avoids an unnecessary write access
            number_reference = number_reference.saturating_sub(1u64);
            self.set_ref_count(eth_accounts, number_reference)?;
        }
        Ok(number_reference)
    }

    pub fn delete(
        eth_accounts: &mut impl KeySpace,
        code_hash: &B256,
    ) -> Result<(), EvmDbError> {
        let code = Self::new(code_hash)?;
        if code.exists(eth_accounts)? {
            let number_reference = code.decrement_code_usage(eth_accounts)?;
            // This was the last smart contract using this code
            if number_reference == 0 {
                eth_accounts.delete(&code.segment_key(&CODE)?);
                eth_accounts.delete(&code.segment_key(&REFERENCE)?);
            };
        };
        Ok(())
    }
}

pub fn get_precompile_bytecode(code_hash: &B256) -> Result<Option<Bytecode>, EvmDbError> {
    if code_hash == &XTZ_BRIDGE_SOL_CONTRACT.code_hash {
        Ok(Some(Bytecode::new_legacy(Bytes::from_static(
            XTZ_BRIDGE_SOL_CONTRACT.code,
        ))))
    } else if code_hash == &FA_BRIDGE_SOL_CONTRACT.code_hash {
        Ok(Some(Bytecode::new_legacy(Bytes::from_static(
            FA_BRIDGE_SOL_CONTRACT.code,
        ))))
    } else if code_hash == &INTERNAL_FORWARDER_SOL_CONTRACT.code_hash {
        Ok(Some(Bytecode::new_legacy(Bytes::from_static(
            INTERNAL_FORWARDER_SOL_CONTRACT.code,
        ))))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod test {
    use super::{CodeStorage, CODE, REFERENCE};

    use revm::{
        primitives::{Bytes, KECCAK_EMPTY},
        state::Bytecode,
    };
    use tezos_evm_runtime::runtime_keyspaces::{
        MockRuntimeKeyspaces, ETH_ACCOUNTS_KEYSPACE_NAME,
    };
    use tezos_smart_rollup_keyspace::extensions::KeySpaceExtNum;
    use tezos_smart_rollup_keyspace::Key;

    #[test]
    fn test_empty_contract_hash_matches_default() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let empty_code: Vec<u8> = vec![];

        let found_code_hash = CodeStorage::add(eth_accounts, &empty_code, None)
            .expect("Could not create code storage");

        assert_eq!(found_code_hash, KECCAK_EMPTY);
    }

    #[test]
    fn test_get_code_matches_given() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code: Vec<u8> = (0..100).collect();
        let code_hash = CodeStorage::add(eth_accounts, &code, None)
            .expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not create code storage");
        let found_code = code_storage
            .get_code(eth_accounts)
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
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();
        let code: Vec<u8> = (0..100).collect();
        let code_hash = CodeStorage::add(eth_accounts, &code, None)
            .expect("Could not create code storage");
        let code_storage =
            CodeStorage::new(&code_hash).expect("Could not find code storage");
        let ref_key = code_storage.segment_key(&REFERENCE).unwrap();

        let ref_count: u64 = eth_accounts
            .get_le(&ref_key)
            .expect("Reference count not found");
        assert_eq!(ref_count, 1u64);

        let second_code_hash = CodeStorage::add(eth_accounts, &code, None)
            .expect("Could not create code storage");

        assert_eq!(second_code_hash, code_hash);

        let ref_count: u64 = eth_accounts
            .get_le(&ref_key)
            .expect("Reference count not found");
        assert_eq!(ref_count, 2u64);

        let () = CodeStorage::delete(eth_accounts, &code_hash)
            .expect("Could not delete code storage");

        let ref_count: u64 = eth_accounts
            .get_le(&ref_key)
            .expect("Reference count not found");
        assert_eq!(ref_count, 1u64);
    }

    #[test]
    fn test_code_is_deleted() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();

        let code_storage =
            CodeStorage::new(&KECCAK_EMPTY).expect("Could not find code storage");

        let exists = code_storage
            .exists(eth_accounts)
            .expect("Could not check if contract exists");

        assert!(!exists, "code storage should not exist");

        let code: Vec<u8> = vec![];
        let _code_hash = CodeStorage::add(eth_accounts, &code, None)
            .expect("Could not create code storage");

        let exists = code_storage
            .exists(eth_accounts)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let _code_hash = CodeStorage::add(eth_accounts, &code, None)
            .expect("Could not create code storage");

        let exists = code_storage
            .exists(eth_accounts)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let () = CodeStorage::delete(eth_accounts, &KECCAK_EMPTY)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(eth_accounts)
            .expect("Could not check if contract exists");
        assert!(exists, "code storage should exist");

        let () = CodeStorage::delete(eth_accounts, &KECCAK_EMPTY)
            .expect("Could not delete code storage");

        let exists = code_storage
            .exists(eth_accounts)
            .expect("Could not check if contract exist");
        assert!(!exists, "code storage should not exists");
    }

    #[test]
    fn test_get_code_from_non_existing_code() {
        let mut rk = MockRuntimeKeyspaces::default();
        let eth_accounts = rk.eth_accounts_mut();

        let code: Vec<u8> = (0..100).collect();
        let code_hash = CodeStorage::add(eth_accounts, &code, None).unwrap();
        let code_storage = CodeStorage::new(&code_hash).unwrap();
        CodeStorage::delete(eth_accounts, &code_hash).unwrap();
        let empty_code = code_storage.get_code(eth_accounts).unwrap();

        assert_eq!(empty_code, None);
    }

    /// The durable path a key of the accounts keyspace resolves to: the
    /// keyspace prepends its own name to every key it is handed.
    fn durable(key: &Key) -> Vec<u8> {
        let name = ETH_ACCOUNTS_KEYSPACE_NAME.to_string();
        [name.as_bytes(), key.as_bytes()].concat()
    }

    #[test]
    fn code_keys_keep_their_durable_paths() {
        let code_hash = KECCAK_EMPTY;
        let code = CodeStorage::new(&code_hash).unwrap();

        assert_eq!(
            durable(&code.segment_key(&CODE).unwrap()),
            format!("/evm/eth_accounts/eth_codes/{code_hash:x}/code").into_bytes()
        );
        assert_eq!(
            durable(&code.segment_key(&REFERENCE).unwrap()),
            format!("/evm/eth_accounts/eth_codes/{code_hash:x}/ref_count").into_bytes()
        );
    }
}
