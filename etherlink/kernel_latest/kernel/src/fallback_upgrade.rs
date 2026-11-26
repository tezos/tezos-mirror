// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_core::{MAX_FILE_CHUNK_SIZE, PREIMAGE_HASH_SIZE};
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceError, ReadKind};
use tezosx_types::StorageReadErrorKind;

use crate::upgrade::{KERNEL_BOOT_KEY, KERNEL_ROOT_HASH_KEY};

const BACKUP_KERNEL_BOOT_PATH_KEY: Key = Key::from_static(b"/__backup_kernel/boot.wasm");

const BACKUP_KERNEL_ROOT_HASH_KEY: Key = Key::from_static(b"/__backup_kernel/root_hash");

/// Copies the kernel of the `/kernel` keyspace and the kernel root hash of
/// `/base` to their backups in `/base`.
///
/// Backs up a zero root hash when `/base` holds none. Logs an error and
/// leaves the kernel backup as it is when `/kernel` holds no kernel. Errors
/// when a write fails.
pub fn backup_current_kernel(
    base: &mut impl KeySpace,
    kernel: &impl KeySpace,
) -> Result<(), KeySpaceError> {
    // Fallback preparation detected
    // Storing the current kernel boot path under a temporary path in
    // order to fallback on it if something goes wrong in the upcoming
    // upgraded kernel.
    log!(
        Info,
        "Preparing potential fallback by backing up the current kernel."
    );

    // If there is a kernel root hash (which is not mandatory after origination,
    // we copy it to the backup, otherwise we just copy empty bytes to have
    // something to fallback on.
    match base.get_prefix_exact::<PREIMAGE_HASH_SIZE>(&KERNEL_ROOT_HASH_KEY) {
        Some(root_hash) => {
            base.set(&BACKUP_KERNEL_ROOT_HASH_KEY, root_hash)?;
        }
        None => {
            base.set(&BACKUP_KERNEL_ROOT_HASH_KEY, [0; PREIMAGE_HASH_SIZE])?;
        }
    };

    let copied =
        copy_value(kernel, &KERNEL_BOOT_KEY, base, &BACKUP_KERNEL_BOOT_PATH_KEY)?;
    if !copied {
        log!(
            Error,
            "Kernel backup skipped: no kernel at key {} of key space {}.",
            KERNEL_BOOT_KEY,
            kernel.name()
        );
    }
    Ok(())
}

/// Restores the kernel of the `/kernel` keyspace and the kernel root hash of
/// `/base` from their backups in `/base`.
///
/// Errors with [`StorageReadErrorKind::NotFound`] when a backup is absent,
/// and with a write error when a write fails. The root hash is restored
/// first: a missing kernel backup leaves it restored.
pub fn fallback_backup_kernel(
    base: &mut impl KeySpace,
    kernel: &mut impl KeySpace,
) -> Result<(), KeySpaceError<StorageReadErrorKind>> {
    log!(
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );

    let backup_kernel_root_hash =
        base.get(&BACKUP_KERNEL_ROOT_HASH_KEY).ok_or_else(|| {
            KeySpaceError::read(
                base.name(),
                &BACKUP_KERNEL_ROOT_HASH_KEY,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            )
        })?;
    base.set(&KERNEL_ROOT_HASH_KEY, &backup_kernel_root_hash)?;

    restore_kernel_backup(base, kernel)
}

/// Restores the kernel of the `/kernel` keyspace from its backup in `/base`.
///
/// Errors with [`StorageReadErrorKind::NotFound`] when the backup is absent,
/// in which case the kernel is unchanged, and with a write error when a write
/// fails.
pub fn restore_kernel_backup(
    base: &impl KeySpace,
    kernel: &mut impl KeySpace,
) -> Result<(), KeySpaceError<StorageReadErrorKind>> {
    let copied =
        copy_value(base, &BACKUP_KERNEL_BOOT_PATH_KEY, kernel, &KERNEL_BOOT_KEY)?;
    if !copied {
        return Err(KeySpaceError::read(
            base.name(),
            &BACKUP_KERNEL_BOOT_PATH_KEY,
            ReadKind::Ext(StorageReadErrorKind::NotFound),
        ));
    }
    Ok(())
}

/// Copies the value at `from_key` of `from` to `to_key` of `to`, in place of
/// the value there, one chunk at a time: the value never sits whole in memory.
///
/// Returns `false`, and writes nothing, when there is no value at `from_key`.
/// Errors when a write fails.
fn copy_value(
    from: &impl KeySpace,
    from_key: &Key,
    to: &mut impl KeySpace,
    to_key: &Key,
) -> Result<bool, KeySpaceError> {
    let mut chunk = [0; MAX_FILE_CHUNK_SIZE];
    let Some(size) = from.read(from_key, 0, &mut chunk) else {
        return Ok(false);
    };
    to.set(to_key, &chunk[..size])?;
    let mut offset = size;
    while let Some(size) = from.read(from_key, offset, &mut chunk) {
        offset += to.write(to_key, offset, &chunk[..size])?;
    }
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::load_keyspaces;
    use tezos_evm_runtime::runtime::MockKernelHost;
    use tezos_smart_rollup_host::path::RefPath;
    use tezos_smart_rollup_host::storage::StorageV1;

    const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");

    #[test]
    fn fallback_restores_the_kernel_at_its_absolute_path() {
        let mut host = MockKernelHost::default();
        let mut bk = load_keyspaces(&mut host).unwrap();
        // Several chunks, and an upgraded kernel longer than the backup.
        let current_kernel = vec![1; 2 * MAX_FILE_CHUNK_SIZE + 10];
        host.store_write_all(&KERNEL_BOOT_PATH, &current_kernel)
            .unwrap();
        bk.base_mut()
            .set(&KERNEL_ROOT_HASH_KEY, [1; PREIMAGE_HASH_SIZE])
            .unwrap();
        let (base, kernel) = bk.base_and_kernel_mut();
        backup_current_kernel(base, kernel).unwrap();

        host.store_write_all(&KERNEL_BOOT_PATH, &[2; 3 * MAX_FILE_CHUNK_SIZE])
            .unwrap();
        bk.base_mut()
            .set(&KERNEL_ROOT_HASH_KEY, [2; PREIMAGE_HASH_SIZE])
            .unwrap();
        let (base, kernel) = bk.base_and_kernel_mut();
        fallback_backup_kernel(base, kernel).unwrap();

        assert_eq!(
            host.store_read_all(&KERNEL_BOOT_PATH).unwrap(),
            current_kernel
        );
        assert_eq!(
            bk.base().get(&KERNEL_ROOT_HASH_KEY),
            Some(vec![1; PREIMAGE_HASH_SIZE])
        );
    }

    #[test]
    fn fallback_without_a_kernel_backup_restores_only_the_root_hash() {
        let mut host = MockKernelHost::default();
        let mut bk = load_keyspaces(&mut host).unwrap();
        // A root hash backup, but no kernel backup.
        bk.base_mut()
            .set(&BACKUP_KERNEL_ROOT_HASH_KEY, [0; PREIMAGE_HASH_SIZE])
            .unwrap();

        host.store_write_all(&KERNEL_BOOT_PATH, b"upgraded kernel")
            .unwrap();
        bk.base_mut()
            .set(&KERNEL_ROOT_HASH_KEY, [2; PREIMAGE_HASH_SIZE])
            .unwrap();
        let (base, kernel) = bk.base_and_kernel_mut();
        assert_eq!(
            fallback_backup_kernel(base, kernel),
            Err(KeySpaceError::read(
                bk.base().name(),
                &BACKUP_KERNEL_BOOT_PATH_KEY,
                ReadKind::Ext(StorageReadErrorKind::NotFound),
            ))
        );

        assert_eq!(
            host.store_read_all(&KERNEL_BOOT_PATH).unwrap(),
            b"upgraded kernel"
        );
        assert_eq!(
            bk.base().get(&KERNEL_ROOT_HASH_KEY),
            Some(vec![0; PREIMAGE_HASH_SIZE])
        );
    }

    #[test]
    fn backup_without_a_kernel_leaves_the_kernel_backup() {
        let mut host = MockKernelHost::default();
        let mut bk = load_keyspaces(&mut host).unwrap();
        bk.base_mut()
            .set(&BACKUP_KERNEL_BOOT_PATH_KEY, b"older kernel")
            .unwrap();

        let (base, kernel) = bk.base_and_kernel_mut();
        backup_current_kernel(base, kernel).unwrap();

        assert_eq!(
            bk.base().get(&BACKUP_KERNEL_BOOT_PATH_KEY),
            Some(b"older kernel".to_vec())
        );
        assert_eq!(
            bk.base().get(&BACKUP_KERNEL_ROOT_HASH_KEY),
            Some(vec![0; PREIMAGE_HASH_SIZE])
        );
    }
}
