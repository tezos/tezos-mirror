// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RawHost`]: a host whose durable accesses are put under a prefix.
//!
//! A [`SafeStorage`](crate::safe_storage::SafeStorage) wraps one with prefix
//! `/tmp`. Without a prefix, it forwards the accesses unchanged.

use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path},
    runtime::{RuntimeError, ValueType},
    storage::StorageV1,
};

use crate::extensions::WithGas;

/// A host whose durable accesses are under `prefix`, or unchanged if it is
/// `None`.
pub struct RawHost<H> {
    pub(crate) host: H,
    pub(crate) prefix: Option<OwnedPath>,
}

/// `path` under `prefix`.
///
/// Errors with [`RuntimeError::PathNotFound`] if it exceeds the path size
/// limit.
fn prefixed(prefix: &OwnedPath, path: &impl Path) -> Result<OwnedPath, RuntimeError> {
    concat(prefix, path).map_err(|_| RuntimeError::PathNotFound)
}

/// Calls `$host.$method` with each path argument under the prefix, if any.
macro_rules! under_prefix {
    ($self:ident, $method:ident, [$($path:ident),*] $(, $arg:expr)*) => {
        match &$self.prefix {
            None => $self.host.$method($($path,)* $($arg),*),
            Some(prefix) => {
                $(let $path = &prefixed(prefix, $path)?;)*
                $self.host.$method($($path,)* $($arg),*)
            }
        }
    };
}

impl<Host: StorageV1> StorageV1 for RawHost<&mut Host> {
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        under_prefix!(self, store_has, [path])
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        under_prefix!(self, store_read, [path], from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        under_prefix!(self, store_read_slice, [path], from_offset, buffer)
    }

    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        under_prefix!(self, store_read_all, [path])
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        under_prefix!(self, store_write, [path], src, at_offset)
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        under_prefix!(self, store_write_all, [path], src)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        under_prefix!(self, store_delete, [path])
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        under_prefix!(self, store_delete_value, [path])
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        under_prefix!(self, store_count_subkeys, [prefix])
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        under_prefix!(self, store_move, [from_path, to_path])
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        under_prefix!(self, store_copy, [from_path, to_path])
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        under_prefix!(self, store_value_size, [path])
    }

    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; tezos_smart_rollup_core::STORE_HASH_SIZE], RuntimeError> {
        under_prefix!(self, store_get_hash, [path])
    }
}

impl<Host: WithGas> WithGas for RawHost<&mut Host> {
    fn add_execution_gas(&mut self, gas: u64) {
        self.host.add_execution_gas(gas)
    }

    fn executed_gas(&self) -> u64 {
        self.host.executed_gas()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::MockKernelHost;
    use crate::safe_storage::{SafeStorage, TMP};
    use tezos_smart_rollup_host::path::PATH_MAX_SIZE;

    /// A path of `len` bytes.
    fn path_of_len(len: usize) -> OwnedPath {
        OwnedPath::try_from(format!("/{}", "a".repeat(len - 1)).into_bytes()).unwrap()
    }

    #[test]
    fn a_path_the_prefix_overflows_is_not_found() {
        let mut host = MockKernelHost::default();
        // Fits with one `/tmp`, not with two.
        let path = path_of_len(PATH_MAX_SIZE - TMP.len());
        let mut raw = RawHost {
            host: &mut host,
            prefix: Some(OwnedPath::try_from(TMP.repeat(2).into_bytes()).unwrap()),
        };
        assert_eq!(raw.store_has(&path), Err(RuntimeError::PathNotFound));
        assert_eq!(
            raw.store_write_all(&path, b"v"),
            Err(RuntimeError::PathNotFound)
        );

        let mut safe = SafeStorage::new(&mut host, vec![]);
        safe.store_write_all(&path, b"v").unwrap();
        let path = path_of_len(PATH_MAX_SIZE);
        assert_eq!(safe.store_read_all(&path), Err(RuntimeError::PathNotFound));
    }
}
