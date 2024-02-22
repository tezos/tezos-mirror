// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::safe_storage::{safe_path, InternalStorage, SafeStorage};
use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_host::{path::RefPath, runtime::RuntimeError, KERNEL_BOOT_PATH};

const BACKUP_KERNEL_BOOT_PATH: RefPath =
    RefPath::assert_from(b"/__backup_kernel/boot.wasm");

fn backup_current_kernel<Host: Runtime>(
    host: &mut SafeStorage<&mut Host, &mut InternalStorage>,
) -> Result<(), RuntimeError> {
    // Fallback preparation detected
    // Storing the current kernel boot path under a temporary path in
    // order to fallback on it if something goes wrong in the upcoming
    // upgraded kernel.
    log!(
        host,
        Info,
        "Preparing potential fallback by backing up the current kernel."
    );
    host.store_copy(&KERNEL_BOOT_PATH, &BACKUP_KERNEL_BOOT_PATH)
}

pub fn fallback_backup_kernel<Host: Runtime>(
    host: &mut SafeStorage<&mut Host, &mut InternalStorage>,
) -> Result<(), RuntimeError> {
    log!(
        host,
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );
    host.store_move(&BACKUP_KERNEL_BOOT_PATH, &KERNEL_BOOT_PATH)
}

fn clean_backup_kernel<Host: Runtime>(
    host: &mut SafeStorage<&mut Host, &mut InternalStorage>,
) -> Result<(), RuntimeError> {
    log!(host, Info, "Cleaning the backup kernel.");
    host.store_delete(&BACKUP_KERNEL_BOOT_PATH)
}

pub fn promote_upgrade<Host: Runtime>(
    host: &mut SafeStorage<&mut Host, &mut InternalStorage>,
) -> Result<(), RuntimeError> {
    let safe_kernel_boot_path = safe_path(&KERNEL_BOOT_PATH)?;
    match host.store_read(&safe_kernel_boot_path, 0, 0) {
        Ok(_) => {
            // Upgrade detected
            log!(host, Info, "Upgrade activated.");
            backup_current_kernel(host)?;
            host.store_move(&safe_kernel_boot_path, &KERNEL_BOOT_PATH)
        }
        Err(_) => {
            // No on-going upgrade detected
            if host.store_read(&BACKUP_KERNEL_BOOT_PATH, 0, 0).is_ok() {
                clean_backup_kernel(host)?
            };
            Ok(())
        }
    }
}
