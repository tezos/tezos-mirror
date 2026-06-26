// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::{path::RefPath, runtime::RuntimeError, KERNEL_BOOT_PATH};

use crate::upgrade::KERNEL_ROOT_HASH;

const BACKUP_KERNEL_BOOT_PATH: RefPath =
    RefPath::assert_from(b"/__backup_kernel/boot.wasm");

const BACKUP_KERNEL_ROOT_HASH: RefPath =
    RefPath::assert_from(b"/__backup_kernel/root_hash");

pub fn backup_current_kernel(host: &mut impl Runtime) -> Result<(), RuntimeError> {
    // Fallback preparation detected
    // Storing the current kernel boot path under a temporary path in
    // order to fallback on it if something goes wrong in the upcoming
    // upgraded kernel.
    log!(
        host,
        Info,
        "Preparing potential fallback by backing up the current kernel."
    );

    // If there is a kernel root hash (which is not mandatory after origination,
    // we copy it to the backup, otherwise we just copy empty bytes to have
    // something to fallback on.
    match host.store_read(&KERNEL_ROOT_HASH, 0, PREIMAGE_HASH_SIZE) {
        Ok(root_hash) => {
            host.store_write(&BACKUP_KERNEL_ROOT_HASH, &root_hash, 0)?;
        }
        Err(RuntimeError::PathNotFound) => {
            host.store_write(&BACKUP_KERNEL_ROOT_HASH, &[0; PREIMAGE_HASH_SIZE], 0)?;
        }
        Err(e) => return Err(e),
    };

    match host.store_read_all(&KERNEL_BOOT_PATH) {
        Ok(kernel) => host.store_write_all(&BACKUP_KERNEL_BOOT_PATH, &kernel),
        Err(RuntimeError::PathNotFound) => Ok(()),
        Err(e) => Err(e),
    }
}

pub fn fallback_backup_kernel(host: &mut impl Runtime) -> Result<(), RuntimeError> {
    log!(
        host,
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );

    let backup_kernel_root_hash = host.store_read_all(&BACKUP_KERNEL_ROOT_HASH)?;
    host.store_write_all(&KERNEL_ROOT_HASH, &backup_kernel_root_hash)?;

    let backup_kernel_boot = host.store_read_all(&BACKUP_KERNEL_BOOT_PATH)?;
    host.store_write_all(&KERNEL_BOOT_PATH, &backup_kernel_boot)
}
