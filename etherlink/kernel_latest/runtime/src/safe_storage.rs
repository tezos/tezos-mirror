// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use crate::extensions::WithGas;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::{
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{RuntimeError, ValueType},
};
use tezos_smart_rollup_keyspace::{KeySpaceLoader, KeySpaceLoaderError, Name};

/// The prefix the mirror copies its roots under.
const TMP: &str = "/tmp";
pub const TMP_PATH: RefPath = RefPath::assert_from(TMP.as_bytes());

pub fn safe_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&TMP_PATH, path).map_err(|_| RuntimeError::PathNotFound)
}

pub struct SafeStorage<Runtime> {
    pub host: Runtime,
    /// Invariant: paths must not overlap (no path is a prefix of another)
    pub world_states: Vec<OwnedPath>,
}

impl<Host: StorageV1> StorageV1 for SafeStorage<&mut Host> {
    #[inline(always)]
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_has(&path)
    }

    #[inline(always)]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read(&path, from_offset, max_bytes)
    }

    #[inline(always)]
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_slice(&path, from_offset, buffer)
    }

    #[inline(always)]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_all(&path)
    }

    #[inline(always)]
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write(&path, src, at_offset)
    }

    #[inline(always)]
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write_all(&path, src)
    }

    #[inline(always)]
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete(&path)
    }

    #[inline(always)]
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete_value(&path)
    }

    #[inline(always)]
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        let prefix = safe_path(prefix)?;
        self.host.store_count_subkeys(&prefix)
    }

    #[inline(always)]
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_move(&from_path, &to_path)
    }

    #[inline(always)]
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_copy(&from_path, &to_path)
    }

    #[inline(always)]
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_value_size(&path)
    }

    #[inline(always)]
    fn store_get_hash(
        &self,
        path: &impl Path,
    ) -> Result<[u8; tezos_smart_rollup_core::STORE_HASH_SIZE], RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_get_hash(&path)
    }
}

impl<Host: StorageV1> SafeStorage<&mut Host> {
    pub fn start(&mut self) -> Result<(), RuntimeError> {
        // Clean up any leftover data in /tmp from a previous run.
        // This is safe because start() is only called when no
        // BlockInProgress exists in storage.
        let _ = self.host.store_delete(&TMP_PATH);

        for world_state in self.world_states.iter() {
            let tmp_path = safe_path(world_state)?;
            self.host.store_copy(world_state, &tmp_path)?;
        }
        Ok(())
    }

    pub fn promote(&mut self) -> Result<(), RuntimeError> {
        for world_state in self.world_states.iter() {
            let tmp_path = safe_path(world_state)?;
            self.host.store_move(&tmp_path, world_state)?;
        }
        Ok(())
    }

    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        self.host.store_delete(&TMP_PATH)
    }
}

impl<Host: KeySpaceLoader> KeySpaceLoader for SafeStorage<&mut Host> {
    type KeySpace = Host::KeySpace;

    /// Loads `/tmp<name>`, so the keyspace writes into the mirror:
    /// [`Self::revert`] drops its writes, and [`Self::promote`] carries them
    /// back when `name` is one of `world_states`.
    ///
    /// Errors with [`KeySpaceLoaderError::InvalidName`] when the prefixed
    /// name exceeds the name size limit, and otherwise with whatever the
    /// wrapped loader returns, notably [`KeySpaceLoaderError::AlreadyLoaded`]
    /// when a handle on the same prefixed name is still alive.
    fn load_or_create(
        &mut self,
        name: Name,
    ) -> Result<Self::KeySpace, KeySpaceLoaderError> {
        let tmp_name = Name::try_from(format!("{TMP}{name}"))?;
        self.host.load_or_create(tmp_name)
    }
}

impl<Host: WithGas> WithGas for SafeStorage<&mut Host> {
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
    use crate::runtime_keyspaces::{
        ETH_ACCOUNTS_KEYSPACE_NAME, ETH_ACCOUNTS_ROOT_PATH,
        EVM_WORLD_STATE_KEYSPACE_NAME, EVM_WORLD_STATE_ROOT_PATH,
    };
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_keyspace::{Key, KeySpace, NameError, MAX_KEYSPACE_NAME_SIZE};

    #[test]
    fn keyspace_writes_follow_the_mirror() {
        let mut host = MockKernelHost::default();
        // `start` copies the root, so it has to exist.
        host.store_write_all(&ETH_ACCOUNTS_ROOT_PATH, b"seed")
            .unwrap();
        let probe = Key::from_static(b"/probe");
        let live_probe = RefPath::assert_from(b"/evm/eth_accounts/probe");
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![OwnedPath::from(ETH_ACCOUNTS_ROOT_PATH)],
        };

        safe.start().unwrap();
        {
            let mut keyspace = safe.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME).unwrap();
            assert_eq!(keyspace.name().to_string(), "/tmp/evm/eth_accounts");
            keyspace.set(&probe, b"inside").unwrap();
        }
        assert!(safe.host.store_has(&live_probe).unwrap().is_none());
        assert_eq!(
            safe.host
                .store_read_all(&safe_path(&live_probe).unwrap())
                .unwrap(),
            b"inside"
        );
        safe.revert().unwrap();
        assert!(safe.host.store_has(&live_probe).unwrap().is_none());

        safe.start().unwrap();
        {
            let mut keyspace = safe.load_or_create(ETH_ACCOUNTS_KEYSPACE_NAME).unwrap();
            keyspace.set(&probe, b"kept").unwrap();
        }
        safe.promote().unwrap();
        assert_eq!(safe.host.store_read_all(&live_probe).unwrap(), b"kept");
    }

    #[test]
    fn load_or_create_rejects_a_name_the_prefix_overflows() {
        let mut host = MockKernelHost::default();
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![],
        };
        // The longest name the loader takes on its own: `/tmp` on top of it
        // does not fit any more.
        let name = Name::try_from(format!("/{}", "a".repeat(MAX_KEYSPACE_NAME_SIZE - 1)))
            .unwrap();
        assert!(matches!(
            safe.load_or_create(name),
            Err(KeySpaceLoaderError::InvalidName(NameError::NameTooLong))
        ));
    }

    #[test]
    fn start_clears_stale_mirror_data() {
        let mut host = MockKernelHost::default();
        // `start` copies the root, so it has to exist.
        let world_state_data = RefPath::assert_from(b"/evm/world_state/accounts");
        host.store_write_all(&world_state_data, b"some_account_data")
            .unwrap();
        let trace = Key::from_static(b"/trace/call_trace/0");
        let tmp_trace = RefPath::assert_from(b"/tmp/evm/world_state/trace/call_trace/0");
        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![OwnedPath::from(EVM_WORLD_STATE_ROOT_PATH)],
        };

        // An interrupted block leaves a trace in the mirror.
        safe.start().unwrap();
        {
            let mut keyspace =
                safe.load_or_create(EVM_WORLD_STATE_KEYSPACE_NAME).unwrap();
            keyspace.set(&trace, [0xAA, 0xBB]).unwrap();
        }
        assert_eq!(safe.host.store_read_all(&tmp_trace).unwrap(), [0xAA, 0xBB]);

        // The next `start` wipes it and copies the root again.
        safe.start().unwrap();
        assert_eq!(safe.host.store_has(&tmp_trace).unwrap(), None);
        assert_eq!(
            safe.host
                .store_read_all(&safe_path(&world_state_data).unwrap())
                .unwrap(),
            b"some_account_data"
        );
    }

    #[test]
    fn start_preserves_world_state_when_tmp_is_clean() {
        let mut host = MockKernelHost::default();

        let world_state_data = RefPath::assert_from(b"/evm/world_state/accounts");
        host.store_write_all(&world_state_data, b"account_data")
            .unwrap();

        let mut safe = SafeStorage {
            host: &mut host,
            world_states: vec![OwnedPath::from(EVM_WORLD_STATE_ROOT_PATH)],
        };

        // First start on a clean host (no stale /tmp)
        safe.start().unwrap();

        let tmp_accounts = safe_path(&world_state_data).unwrap();
        assert_eq!(
            safe.host.store_read_all(&tmp_accounts).unwrap(),
            b"account_data"
        );
    }
}
