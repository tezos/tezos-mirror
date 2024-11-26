// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::internal_runtime::InternalRuntime;
use sha3::{Digest, Keccak256};
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_mock::MockHost;
impl InternalRuntime for MockHost {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        let hash: [u8; 32] = Keccak256::digest(path.as_bytes()).into();
        Ok(hash.into())
    }
}
