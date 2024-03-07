// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_evm_logging::{log, Level::*};
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_host::{path::RefPath, runtime::RuntimeError, KERNEL_BOOT_PATH};

const BACKUP_KERNEL_BOOT_PATH: RefPath =
    RefPath::assert_from(b"/__backup_kernel/boot.wasm");

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
    host.store_copy(&KERNEL_BOOT_PATH, &BACKUP_KERNEL_BOOT_PATH)
}

pub fn fallback_backup_kernel(host: &mut impl Runtime) -> Result<(), RuntimeError> {
    log!(
        host,
        Error,
        "Something went wrong, fallback mechanism is triggered."
    );
    host.store_copy(&BACKUP_KERNEL_BOOT_PATH, &KERNEL_BOOT_PATH)
}
