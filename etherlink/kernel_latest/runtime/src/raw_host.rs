// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! [`RawHost`]: a host whose durable accesses are put under a prefix.
//!
//! A [`SafeStorage`](crate::safe_storage::SafeStorage) wraps one with prefix
//! `/tmp`, and a
//! [`RuntimeKeyspaces`](crate::runtime_keyspaces::RuntimeKeyspaces) holds one
//! with its own prefix. Without a prefix, it forwards the accesses unchanged.

use std::fmt::Display;

use tezos_smart_rollup_host::{
    input::Message,
    path::{concat, OwnedPath, Path},
    runtime::{RuntimeError, ValueType},
    storage::StorageV1,
    wasm::WasmHost,
};
use tezos_smart_rollup_keyspace::{KeySpaceLoaderError, Name};

use crate::extensions::WithGas;

/// A host whose durable accesses are under `prefix`, or unchanged if it is
/// `None`.
pub struct RawHost<H> {
    pub(crate) host: H,
    pub(crate) prefix: Option<OwnedPath>,
}

impl<Host> RawHost<&mut Host> {
    /// The wrapped host, if the prefix is `None`: accesses through it are
    /// then the same as through `self`.
    pub fn unprefixed_mut(&mut self) -> Option<&mut Host> {
        match self.prefix {
            None => Some(&mut *self.host),
            Some(_) => None,
        }
    }
}

/// `path` under `prefix`.
///
/// Errors with [`RuntimeError::PathNotFound`] if it exceeds the path size
/// limit.
fn prefixed(prefix: &OwnedPath, path: &impl Path) -> Result<OwnedPath, RuntimeError> {
    concat(prefix, path).map_err(|_| RuntimeError::PathNotFound)
}

/// The keyspace name `name` under `prefix`.
///
/// Errors with [`KeySpaceLoaderError::InvalidName`] if it exceeds the name
/// size limit.
pub(crate) fn prefixed_name(
    prefix: &impl Display,
    name: &Name,
) -> Result<Name, KeySpaceLoaderError> {
    Ok(Name::try_from(format!("{prefix}{name}"))?)
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

/// Forwards to the wrapped host: none of these accesses durable storage.
///
/// Needed by the simulation, which reads its inbox through
/// [`RuntimeKeyspaces::host_mut`](crate::runtime_keyspaces::RuntimeKeyspaces::host_mut).
impl<Host: WasmHost> WasmHost for RawHost<&mut Host> {
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.read_input()
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    fn clear_reboot_mark(&mut self) -> Result<(), RuntimeError> {
        self.host.clear_reboot_mark()
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        self.host.last_run_aborted()
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        self.host.upgrade_failed()
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        self.host.restart_forced()
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.host.reboot_left()
    }

    fn runtime_version(&self) -> Result<String, RuntimeError> {
        self.host.runtime_version()
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
