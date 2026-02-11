// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! The WASM pvm provides certain capabilities specific to its runtime.
//! See [WasmHost] for more details.
//!
//! # RISC-V compatability
//!
//! While it is currently possible to target RISC-V using the WasmHost,
//! most of the functionalities exposed here will panic if an attempt is made to use them on
//! RISC-V directly.
//!
//! Namely, RISC-V treats `read_input` as non-blocking, and the notion of reboots in completely absent
//! in RISC-V.
//!
//! In future, a RISC-V specific host will be available to be used directly.

#[cfg(feature = "alloc")]
use alloc::string::String;
#[cfg(feature = "alloc")]
use alloc::vec::Vec;
use tezos_smart_rollup_core::SmartRollupCore;

#[cfg(feature = "alloc")]
use crate::input::Message;
use crate::storage::StorageV1;
use crate::{path::RefPath, runtime::RuntimeError, Error};

const REBOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/env/reboot");

/// Certain capabilities of the WASM pvm runtime are specific to this platform, or
/// have certain characteristics that may differ to RISC-V.
pub trait WasmHost {
    /// Write contents of the given slice to output.
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError>;

    /// Read the next input from the global inbox.
    ///
    /// Returns `None` if no message was available. This happens when the kernel has
    /// finished reading the inbox at the current level.
    ///
    /// The kernel will need to yield to the next level to recieve more input.
    #[cfg(feature = "alloc")]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError>;

    /// Mark the kernel for reboot.
    ///
    /// If the kernel is marked for reboot, it will continue
    /// reading inbox messages for the current level next time `kernel_run` runs.
    /// If the inbox contains no more messages, the kernel will still continue at
    /// the current inbox level until it is no longer marked for reboot.
    ///
    /// If the kernel is _not_ marked for reboot, it will skip the rest of the inbox
    /// for the current level and _yield_. It will then continue at the next inbox
    /// level.
    ///
    /// The kernel is given a maximum number of reboots per level. The number of reboots remaining
    /// is written to `/readonly/kernel/env/reboot_counter` (little endian i32).
    ///
    /// If the kernel exceeds this, it is forced to yield to the next level (and a flag is set at
    /// `/readonly/kernel/env/too_many_reboot` to indicate this happened.
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError>;

    /// True if the last kernel run was aborted.
    fn last_run_aborted(&self) -> Result<bool, RuntimeError>;

    /// True if the kernel failed to upgrade.
    fn upgrade_failed(&self) -> Result<bool, RuntimeError>;

    /// True if the kernel rebooted too many times.
    fn restart_forced(&self) -> Result<bool, RuntimeError>;

    /// The number of reboot left for the kernel.
    fn reboot_left(&self) -> Result<u32, RuntimeError>;

    /// The runtime_version the kernel is using.
    #[cfg(feature = "alloc")]
    fn runtime_version(&self) -> Result<String, RuntimeError>;
}

impl<Host: SmartRollupCore> WasmHost for Host {
    fn write_output(&mut self, output: &[u8]) -> Result<(), RuntimeError> {
        let result_code =
            unsafe { SmartRollupCore::write_output(self, output.as_ptr(), output.len()) };

        match Error::wrap(result_code) {
            Ok(_) => Ok(()),
            Err(e) => Err(RuntimeError::HostErr(e)),
        }
    }

    #[cfg(feature = "alloc")]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        use core::mem::MaybeUninit;
        use tezos_smart_rollup_core::{
            smart_rollup_core::ReadInputMessageInfo, MAX_INPUT_MESSAGE_SIZE,
        };

        use crate::input::Message;

        let mut buffer = Vec::with_capacity(MAX_INPUT_MESSAGE_SIZE);

        let mut message_info = MaybeUninit::<ReadInputMessageInfo>::uninit();

        let bytes_read = unsafe {
            SmartRollupCore::read_input(
                self,
                message_info.as_mut_ptr(),
                buffer.as_mut_ptr(),
                MAX_INPUT_MESSAGE_SIZE,
            )
        };

        let bytes_read = match Error::wrap(bytes_read) {
            Ok(0) => return Ok(None),
            Ok(size) => size,
            Err(e) => return Err(RuntimeError::HostErr(e)),
        };

        let ReadInputMessageInfo { level, id } = unsafe {
            buffer.set_len(bytes_read);
            message_info.assume_init()
        };

        // level & id are guaranteed to be positive
        let input = Message::new(level as u32, id as u32, buffer);

        Ok(Some(input))
    }

    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.store_write(&REBOOT_PATH, &[0_u8], 0)
    }

    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        const PATH_STUCK_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/stuck");
        let last_run_aborted = StorageV1::store_has(self, &PATH_STUCK_FLAG)?.is_some();
        Ok(last_run_aborted)
    }

    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        const PATH_UPGRADE_ERROR_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/upgrade_error");
        let upgrade_failed =
            StorageV1::store_has(self, &PATH_UPGRADE_ERROR_FLAG)?.is_some();
        Ok(upgrade_failed)
    }

    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        const PATH_TOO_MANY_REBOOT_FLAG: RefPath =
            RefPath::assert_from_readonly(b"/readonly/kernel/env/too_many_reboot");
        let restart_forced =
            StorageV1::store_has(self, &PATH_TOO_MANY_REBOOT_FLAG)?.is_some();
        Ok(restart_forced)
    }

    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        #[cfg(not(pvm_kind = "riscv"))]
        {
            const PATH_REBOOT_COUNTER: RefPath =
                RefPath::assert_from_readonly(b"/readonly/kernel/env/reboot_counter");
            const SIZE: usize = core::mem::size_of::<i32>();

            let mut bytes: [u8; SIZE] = [0; SIZE];
            self.store_read_slice(&PATH_REBOOT_COUNTER, 0, &mut bytes)?;

            let counter = u32::from_le_bytes(bytes);
            Ok(counter)
        }

        // Kernels for the RISC-V PVM do not need to reboot. This function is only
        // implemented for compatibility with existing kernels.
        #[cfg(pvm_kind = "riscv")]
        {
            Ok(u32::MAX)
        }
    }

    #[cfg(feature = "alloc")]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        const PATH_VERSION: RefPath =
            RefPath::assert_from_readonly(b"/readonly/wasm_version");
        let bytes = StorageV1::store_read(self, &PATH_VERSION, 0, 9)?;
        // SAFETY: This storage can only contains valid version string which are utf8 safe.
        let version = unsafe { alloc::string::String::from_utf8_unchecked(bytes) };
        Ok(version)
    }
}
