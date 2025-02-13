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
    if host.store_has(&KERNEL_ROOT_HASH)?.is_some() {
        host.store_copy(&KERNEL_ROOT_HASH, &BACKUP_KERNEL_ROOT_HASH)?;
    } else {
        host.store_write_all(&BACKUP_KERNEL_ROOT_HASH, &[0; PREIMAGE_HASH_SIZE])?;
    }

    host.store_copy(&KERNEL_BOOT_PATH, &BACKUP_KERNEL_BOOT_PATH)
}

pub fn fallback_backup_kernel(host: &mut impl Runtime) -> Result<(), RuntimeError> {
    log!(
        host,
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );

    host.store_copy(&BACKUP_KERNEL_ROOT_HASH, &KERNEL_ROOT_HASH)?;
    host.store_copy(&BACKUP_KERNEL_BOOT_PATH, &KERNEL_BOOT_PATH)
}
