// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

// The [__internal_store_get_hash] host function is not made available by the
// SDK. We expose it through an [InternalRuntime] trait.

use tezos_smart_rollup_host::{path::Path, runtime::RuntimeError};

pub trait InternalRuntime {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError>;
}

// Wrapper for InternalRuntime, this will be added
// to the Runtime for the Kernel to use.
// The path is optional to be able to get the hash
// of the root directory.
pub trait ExtendedRuntime {
    fn store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError>;
}
