// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::B256;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_host::path::OwnedPath;

use crate::Error;

// Ref. https://www.evm.codes/?fork=cancun#40
// (opcode 0x40: BLOCKHASH)
pub const BLOCKS_STORED: u64 = 256;

const MALFORMED_HASH_ERR: &str = "Malformed block hash";

fn to_block_hash_path(block_number: u64) -> Result<OwnedPath, Error> {
    let path: Vec<u8> = format!("/evm/world_state/indexes/blocks/{block_number}").into();
    OwnedPath::try_from(path).map_err(|err| Error::Custom(err.to_string()))
}

/// Get block hash by block number.
pub fn get_block_hash(host: &impl Runtime, block_number: u64) -> Result<B256, Error> {
    let block_path = to_block_hash_path(block_number)?;
    let block_hash = host.store_read(&block_path, 0, 32)?;

    if block_hash.len() == 32 {
        Ok(B256::from_slice(&block_hash))
    } else {
        Err(Error::Custom(MALFORMED_HASH_ERR.to_owned()))
    }
}

#[cfg(test)]
mod test {
    use super::{get_block_hash, to_block_hash_path, MALFORMED_HASH_ERR};
    use crate::Error;
    use revm::primitives::B256;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::runtime::Runtime;

    #[test]
    fn test_malformed_hash() {
        let mut host = MockKernelHost::default();

        let block_number = 1u64;
        let path = to_block_hash_path(block_number).unwrap();
        host.store_write(&path, &[1; 31], 0).unwrap();

        if let Err(Error::Custom(error)) = get_block_hash(&host, block_number) {
            if error != MALFORMED_HASH_ERR {
                panic!(
                    "Wrong error, got \"{error}\" \
                     while \"{MALFORMED_HASH_ERR}\" was expected"
                )
            }
        } else {
            panic!("Expected a malformed hash")
        }
    }

    #[test]
    fn test_valid_hash() {
        let mut host = MockKernelHost::default();

        let block_number = 1u64;
        let block_hash = &[42; 32];
        let path = to_block_hash_path(block_number).unwrap();
        host.store_write(&path, block_hash, 0).unwrap();

        assert_eq!(
            get_block_hash(&host, block_number).unwrap(),
            B256::from(block_hash)
        )
    }
}
