// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_evm_logging::{log, Level};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
#[cfg(feature = "proto-alpha")]
use tezos_smart_rollup_host::dal_parameters::RollupDalParameters;
use tezos_smart_rollup_host::{
    input::Message,
    metadata::RollupMetadata,
    path::{concat, OwnedPath, Path, RefPath},
    runtime::{Runtime, RuntimeError, ValueType},
    Error, KERNEL_BOOT_PATH,
};

const BACKUP_KERNEL_BOOT_PATH: RefPath =
    RefPath::assert_from(b"/__backup_kernel/boot.wasm");

pub const TMP_PATH: RefPath = RefPath::assert_from(b"/tmp");

const STORE_HASH_SIZE: usize = 32;

// The [__internal_store_get_hash] host function is not made available by the
// SDK. We expose it through an [InternalRuntime] trait.

#[link(wasm_import_module = "smart_rollup_core")]
extern "C" {
    pub fn __internal_store_get_hash(
        path: *const u8,
        path_len: usize,
        dst: *mut u8,
        max_size: usize,
    ) -> i32;
}

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
    fn store_get_hash<T: Path>(
        &mut self,
        path: Option<T>,
    ) -> Result<Vec<u8>, RuntimeError>;
}

pub struct InternalStorage();

impl InternalRuntime for InternalStorage {
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        let mut buffer = [0u8; STORE_HASH_SIZE];
        let result = unsafe {
            __internal_store_get_hash(
                path.as_ptr(),
                path.size(),
                buffer.as_mut_ptr(),
                STORE_HASH_SIZE,
            )
        };
        match Error::wrap(result) {
            Ok(_i) => Ok(buffer.to_vec()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }
}

// The kernel runtime requires both the standard Runtime and the
// new Extended one.
pub trait KernelRuntime: Runtime + ExtendedRuntime {}

// Every type implementing an InternalRuntime will implement
// the ExtendedRuntime.
impl<T: InternalRuntime> ExtendedRuntime for T {
    fn store_get_hash<P: Path>(
        &mut self,
        path: Option<P>,
    ) -> Result<Vec<u8>, RuntimeError> {
        match path {
            Some(p) => {
                let path = safe_path(&p)?;
                self.__internal_store_get_hash(&path)
            }
            None => self.__internal_store_get_hash(&TMP_PATH),
        }
    }
}

// If a type implements the Runtime and InternalRuntime traits,
// it also implements the KernelRuntime.
impl<T: Runtime + InternalRuntime> KernelRuntime for T {}

pub struct SafeStorage<Host, Internal> {
    pub host: Host,
    pub internal: Internal,
}

fn safe_path<T: Path>(path: &T) -> Result<OwnedPath, RuntimeError> {
    concat(&TMP_PATH, path).map_err(|_| RuntimeError::PathNotFound)
}

impl<Host, InternalHost: InternalRuntime> InternalRuntime
    for SafeStorage<&mut Host, &mut InternalHost>
{
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.internal.__internal_store_get_hash(path)
    }
}

impl<Host: Runtime, InternalHost> Runtime for SafeStorage<&mut Host, &mut InternalHost> {
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.write_output(from)
    }

    fn write_debug(&self, msg: &str) {
        self.host.write_debug(msg)
    }

    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.read_input()
    }

    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_has(&path)
    }

    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read(&path, from_offset, max_bytes)
    }

    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_slice(&path, from_offset, buffer)
    }

    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_read_all(&path)
    }

    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write(&path, src, at_offset)
    }

    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_write_all(&path, src)
    }

    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete(&path)
    }

    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_delete_value(&path)
    }

    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        let prefix = safe_path(prefix)?;
        self.host.store_count_subkeys(&prefix)
    }

    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_move(&from_path, &to_path)
    }

    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        let from_path = safe_path(from_path)?;
        let to_path = safe_path(to_path)?;
        self.host.store_copy(&from_path, &to_path)
    }

    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.reveal_preimage(hash, destination)
    }

    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        let path = safe_path(path)?;
        self.host.store_value_size(&path)
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.mark_for_reboot()
    }

    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.reveal_metadata()
    }

    #[cfg(all(feature = "alloc", feature = "proto-alpha"))]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host
            .reveal_dal_page(published_level, slot_index, page_index, destination)
    }

    #[cfg(feature = "proto-alpha")]
    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.host.reveal_dal_parameters()
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

impl<Host: Runtime, Internal> SafeStorage<&mut Host, &mut Internal> {
    fn backup_current_kernel(&mut self) -> Result<(), RuntimeError> {
        // Fallback preparation detected
        // Storing the current kernel boot path under a temporary path in
        // order to fallback on it if something goes wrong in the upcoming
        // upgraded kernel.
        log!(
            self.host,
            Level::Info,
            "Preparing potential fallback by backing up the current kernel."
        );
        self.host
            .store_copy(&KERNEL_BOOT_PATH, &BACKUP_KERNEL_BOOT_PATH)
    }

    pub fn fallback_backup_kernel(&mut self) -> Result<(), RuntimeError> {
        log!(
            self.host,
            Level::Info,
            "Something went wrong, fallback mechanism is triggered."
        );
        self.host
            .store_move(&BACKUP_KERNEL_BOOT_PATH, &KERNEL_BOOT_PATH)
    }

    fn clean_backup_kernel(&mut self) -> Result<(), RuntimeError> {
        log!(self.host, Level::Info, "Cleaning the backup kernel.");
        self.host.store_delete(&BACKUP_KERNEL_BOOT_PATH)
    }

    pub fn promote_upgrade(&mut self) -> Result<(), RuntimeError> {
        let safe_kernel_boot_path = safe_path(&KERNEL_BOOT_PATH)?;
        match self.host.store_read(&safe_kernel_boot_path, 0, 0) {
            Ok(_) => {
                // Upgrade detected
                log!(self, Level::Info, "Upgrade activated.");
                self.backup_current_kernel()?;
                self.host
                    .store_move(&safe_kernel_boot_path, &KERNEL_BOOT_PATH)
            }
            Err(_) => {
                // No on-going upgrade detected
                if self.host.store_read(&BACKUP_KERNEL_BOOT_PATH, 0, 0).is_ok() {
                    self.clean_backup_kernel()?
                };
                Ok(())
            }
        }
    }

    pub fn promote(&mut self, path: &impl Path) -> Result<(), RuntimeError> {
        self.host.store_move(&TMP_PATH, path)
    }

    pub fn revert(&mut self) -> Result<(), RuntimeError> {
        self.host.store_delete(&TMP_PATH)
    }
}
