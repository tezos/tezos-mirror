// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::{runtime::RuntimeError, KERNEL_BOOT_PATH};
use tezos_smart_rollup_keyspace::{Key, KeySpace};

use crate::upgrade::KERNEL_ROOT_HASH_KEY;

const BACKUP_KERNEL_BOOT_PATH_KEY: Key = Key::from_static(b"/__backup_kernel/boot.wasm");

const BACKUP_KERNEL_ROOT_HASH_KEY: Key = Key::from_static(b"/__backup_kernel/root_hash");

pub fn backup_current_kernel<Host>(
    host: &mut Host,
    base: &mut impl KeySpace,
) -> anyhow::Result<()>
where
    Host: StorageV1,
{
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

    match host.store_read_all(&KERNEL_BOOT_PATH) {
        Ok(kernel) => base.set(&BACKUP_KERNEL_BOOT_PATH_KEY, &kernel)?,
        Err(RuntimeError::PathNotFound) => {}
        Err(e) => return Err(e.into()),
    }
    Ok(())
}

pub fn fallback_backup_kernel<Host>(
    host: &mut Host,
    base: &mut impl KeySpace,
) -> anyhow::Result<()>
where
    Host: StorageV1,
{
    log!(
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );

    let backup_kernel_root_hash = base
        .get(&BACKUP_KERNEL_ROOT_HASH_KEY)
        .ok_or(RuntimeError::PathNotFound)?;
    base.set(&KERNEL_ROOT_HASH_KEY, &backup_kernel_root_hash)?;

    let backup_kernel_boot = base
        .get(&BACKUP_KERNEL_BOOT_PATH_KEY)
        .ok_or(RuntimeError::PathNotFound)?;
    host.store_write_all(&KERNEL_BOOT_PATH, &backup_kernel_boot)?;
    Ok(())
}
