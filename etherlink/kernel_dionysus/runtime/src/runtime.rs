// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// The kernel runtime requires both the standard Runtime and the
// new Extended one.

use std::{
    borrow::{Borrow, BorrowMut},
    marker::PhantomData,
};

use crate::{
    extensions::WithGas,
    internal_runtime::{ExtendedRuntime, InternalRuntime},
};
use tezos_evm_logging::{Level, Verbosity};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
use tezos_smart_rollup_host::{
    dal_parameters::RollupDalParameters,
    input::Message,
    metadata::RollupMetadata,
    path::{Path, RefPath},
    runtime::{Runtime as SdkRuntime, RuntimeError, ValueType},
};
use tezos_smart_rollup_mock::MockHost;

// Set by the node, contains the verbosity for the logs
pub const VERBOSITY_PATH: RefPath = RefPath::assert_from(b"/evm/logging_verbosity");

pub trait Runtime:
    SdkRuntime + InternalRuntime + ExtendedRuntime + Verbosity + WithGas
{
}

// If a type implements the Runtime, InternalRuntime and ExtendedRuntime traits,
// it also implements the kernel Runtime.
impl<T: SdkRuntime + InternalRuntime + ExtendedRuntime + Verbosity + WithGas> Runtime
    for T
{
}

// This type has two interesting parts:
// 1. Host: BorrowMut<R> + Borrow<R>
//
//    This allows building a KernelHost that can own its host (see
//    KernelHost::default()) as long as this host can be borrowed. It makes it
//    compatible with type `KernelHost<&mut Host, _>`, which is the type built
//    in the kernel as the entrypoint only gives a mutable reference to the
//    host. As such, the implementation of the *Runtime traits work for both.
//
// 2. _pd: PhantomData<R>
//
//    SdkRuntime cannot be used directly as parameter of Borrow and BorrowMut as
//    it is a trait, it needs to be a parameter itself of the type.
//    However it is never used in the type itself, which will be rejected by the
//    compiler. PhantomData associates `R` to the struct with no cost at
//    runtime.
pub struct KernelHost<R, Host: BorrowMut<R> + Borrow<R>> {
    pub host: Host,
    pub logs_verbosity: Level,
    pub execution_gas_used: u64,
    pub _pd: PhantomData<R>,
}

impl<R: SdkRuntime + InternalRuntime, Host: BorrowMut<R> + Borrow<R>> SdkRuntime
    for KernelHost<R, Host>
{
    #[inline(always)]
    fn write_output(&mut self, from: &[u8]) -> Result<(), RuntimeError> {
        self.host.borrow_mut().write_output(from)
    }

    #[inline(always)]
    fn write_debug(&self, msg: &str) {
        self.host.borrow().write_debug(msg)
    }

    #[inline(always)]
    fn read_input(&mut self) -> Result<Option<Message>, RuntimeError> {
        self.host.borrow_mut().read_input()
    }

    #[inline(always)]
    fn store_has<T: Path>(&self, path: &T) -> Result<Option<ValueType>, RuntimeError> {
        self.host.borrow().store_has(path)
    }

    #[inline(always)]
    fn store_read<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        max_bytes: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.borrow().store_read(path, from_offset, max_bytes)
    }

    #[inline(always)]
    fn store_read_slice<T: Path>(
        &self,
        path: &T,
        from_offset: usize,
        buffer: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host
            .borrow()
            .store_read_slice(path, from_offset, buffer)
    }

    #[inline(always)]
    fn store_read_all(&self, path: &impl Path) -> Result<Vec<u8>, RuntimeError> {
        self.host.borrow().store_read_all(path)
    }

    #[inline(always)]
    fn store_write<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
        at_offset: usize,
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_write(path, src, at_offset)
    }

    #[inline(always)]
    fn store_write_all<T: Path>(
        &mut self,
        path: &T,
        src: &[u8],
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_write_all(path, src)
    }

    #[inline(always)]
    fn store_delete<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_delete(path)
    }

    #[inline(always)]
    fn store_delete_value<T: Path>(&mut self, path: &T) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_delete_value(path)
    }

    #[inline(always)]
    fn store_count_subkeys<T: Path>(&self, prefix: &T) -> Result<u64, RuntimeError> {
        self.host.borrow().store_count_subkeys(prefix)
    }

    #[inline(always)]
    fn store_move(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_move(from_path, to_path)
    }

    #[inline(always)]
    fn store_copy(
        &mut self,
        from_path: &impl Path,
        to_path: &impl Path,
    ) -> Result<(), RuntimeError> {
        self.host.borrow_mut().store_copy(from_path, to_path)
    }

    #[inline(always)]
    fn reveal_preimage(
        &self,
        hash: &[u8; PREIMAGE_HASH_SIZE],
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.borrow().reveal_preimage(hash, destination)
    }

    #[inline(always)]
    fn store_value_size(&self, path: &impl Path) -> Result<usize, RuntimeError> {
        self.host.borrow().store_value_size(path)
    }

    #[inline(always)]
    fn mark_for_reboot(&mut self) -> Result<(), RuntimeError> {
        self.host.borrow_mut().mark_for_reboot()
    }

    #[inline(always)]
    fn reveal_metadata(&self) -> RollupMetadata {
        self.host.borrow().reveal_metadata()
    }

    #[inline(always)]
    fn reveal_dal_page(
        &self,
        published_level: i32,
        slot_index: u8,
        page_index: i16,
        destination: &mut [u8],
    ) -> Result<usize, RuntimeError> {
        self.host.borrow().reveal_dal_page(
            published_level,
            slot_index,
            page_index,
            destination,
        )
    }

    #[inline(always)]
    fn reveal_dal_parameters(&self) -> RollupDalParameters {
        self.host.borrow().reveal_dal_parameters()
    }

    #[inline(always)]
    fn last_run_aborted(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn upgrade_failed(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn restart_forced(&self) -> Result<bool, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }

    #[inline(always)]
    fn reboot_left(&self) -> Result<u32, RuntimeError> {
        self.host.borrow().reboot_left()
    }

    #[inline(always)]
    fn runtime_version(&self) -> Result<String, RuntimeError> {
        // This function is never used by the kernel. Be aware that if you need to use it, you also
        // need to modify the WASM Runtime.
        unimplemented!()
    }
}

impl<R: SdkRuntime + InternalRuntime, Host: Borrow<R> + BorrowMut<R>> InternalRuntime
    for KernelHost<R, Host>
{
    #[inline(always)]
    fn __internal_store_get_hash<T: Path>(
        &mut self,
        path: &T,
    ) -> Result<Vec<u8>, RuntimeError> {
        self.host.borrow_mut().__internal_store_get_hash(path)
    }
}

impl<R: SdkRuntime + InternalRuntime, Host: BorrowMut<R> + Borrow<R>> ExtendedRuntime
    for KernelHost<R, Host>
{
    #[inline(always)]
    fn store_get_hash<T: Path>(&mut self, path: &T) -> Result<Vec<u8>, RuntimeError> {
        self.__internal_store_get_hash(path)
    }
}

impl<R: SdkRuntime, Host: Borrow<R> + BorrowMut<R>> KernelHost<R, Host>
where
    for<'a> &'a mut Host: BorrowMut<R>,
{
    pub fn to_ref_host(&mut self) -> KernelHost<R, &mut Host> {
        KernelHost {
            host: &mut self.host,
            logs_verbosity: self.logs_verbosity,
            execution_gas_used: self.execution_gas_used,
            _pd: PhantomData,
        }
    }
}

impl<R: SdkRuntime + InternalRuntime, Host: BorrowMut<R> + Borrow<R>> Verbosity
    for KernelHost<R, Host>
{
    fn verbosity(&self) -> Level {
        self.logs_verbosity
    }
}

pub fn read_logs_verbosity<Host: tezos_smart_rollup_host::runtime::Runtime>(
    host: &Host,
) -> Level {
    match host.store_read_all(&VERBOSITY_PATH) {
        Ok(value) if value.len() == 1 => {
            Level::try_from(value[0]).unwrap_or(Level::default())
        }
        _ => Level::default(),
    }
}

impl<R: SdkRuntime + InternalRuntime, Host: BorrowMut<R> + Borrow<R>> WithGas
    for KernelHost<R, Host>
{
    fn add_execution_gas(&mut self, gas: u64) {
        self.execution_gas_used += gas;
    }

    fn executed_gas(&self) -> u64 {
        self.execution_gas_used
    }
}

impl<R: SdkRuntime, Host: BorrowMut<R> + Borrow<R>> KernelHost<R, Host> {
    pub fn init(host: Host) -> Self {
        let logs_verbosity = read_logs_verbosity(host.borrow());
        Self {
            host,
            logs_verbosity,
            execution_gas_used: 0,
            _pd: PhantomData,
        }
    }
}
pub type MockKernelHost = KernelHost<MockHost, MockHost>;

impl Default for MockKernelHost {
    fn default() -> Self {
        Self {
            host: MockHost::default(),
            logs_verbosity: Level::default(),
            execution_gas_used: 0,
            _pd: PhantomData,
        }
    }
}

impl MockKernelHost {
    pub fn with_address(address: SmartRollupAddress) -> Self {
        let host = MockHost::with_address(&address);
        KernelHost {
            host,
            logs_verbosity: Level::default(),
            execution_gas_used: 0,
            _pd: PhantomData,
        }
    }
}
