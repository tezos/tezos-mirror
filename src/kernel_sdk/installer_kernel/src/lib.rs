// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Installer kernel for tezos smart rollups.
//!
//! # About
//!
//! When originating a smart rollup, you must supply a kernel - the program to be executed
//! by the rollup. This origination kernel must fit within the size of a Layer-1 operation
//! (about 32KB).
//!
//! Almost all useful kernels are larger than this, however. As a result, it is recommended
//! to use this installer kernel. When originating a rollup, you may use a configured
//! installer kernel - which will then proceed to upgrade to your desired kernel.

#![cfg_attr(all(target_arch = "wasm32", not(feature = "std")), no_std)]
#![forbid(unsafe_code)]

use core::panic::PanicInfo;
use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_encoding::dac::reveal_loop;
use tezos_smart_rollup_encoding::dac::V0SliceContentPage;
use tezos_smart_rollup_encoding::dac::MAX_PAGE_SIZE;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::runtime::Runtime;

// Path of currently running kernel.
const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");

// Path that we write the kernel to, before upgrading.
const PREPARE_KERNEL_PATH: RefPath = RefPath::assert_from(b"/installer/kernel/boot.wasm");

// Support 3 levels of hashes pages, and then bottom layer of content.
const MAX_DAC_LEVELS: usize = 4;

#[cfg(all(feature = "entrypoint", target_arch = "wasm32"))]
tezos_smart_rollup_entrypoint::kernel_entry!(installer);

/// Installer.
pub fn installer<Host: Runtime>(host: &mut Host) {
    if let Some(preimage_hash) = read_reveal_hash(host) {
        if let Err(e) = install_kernel(host, &preimage_hash) {
            Runtime::write_debug(host, e)
        }
    } else {
        host.write_debug("Failed to read reveal hash")
    }
}

/// Panic handler used when targetting wasm.
///
/// Any error when installing a kernel is fatal, therefore we
/// handle panics by panicking again, which aborts execution.
#[cfg_attr(all(target_arch = "wasm32", not(feature = "std")), panic_handler)]
#[allow(dead_code)]
fn panic(_info: &PanicInfo) -> ! {
    panic!()
}

/// Reads the final [PREIMAGE_HASH_SIZE] bytes of the installer binary.
///
/// The binary must have been instrumented with a custom module, containing the
/// preimage hash of the target kernel, and this hash must be placed at the end
/// of the binary.
fn read_reveal_hash(host: &mut impl Runtime) -> Option<[u8; PREIMAGE_HASH_SIZE]> {
    let kernel_size = host.store_value_size(&KERNEL_BOOT_PATH).ok()?;
    let reveal_hash_start = kernel_size.checked_sub(PREIMAGE_HASH_SIZE)?;

    let mut preimage_hash = [0; PREIMAGE_HASH_SIZE];

    host.store_read_slice(&KERNEL_BOOT_PATH, reveal_hash_start, &mut preimage_hash)
        .ok()?;

    Some(preimage_hash)
}

fn install_kernel<Host: Runtime>(
    host: &mut Host,
    root_hash: &[u8; PREIMAGE_HASH_SIZE],
) -> Result<(), &'static str> {
    let mut buffer = [0; MAX_PAGE_SIZE * MAX_DAC_LEVELS];

    let mut write_kernel_page = write_kernel_page();

    reveal_loop(
        host,
        0,
        root_hash,
        buffer.as_mut_slice(),
        MAX_DAC_LEVELS,
        &mut write_kernel_page,
    )?;

    Runtime::store_move(host, &PREPARE_KERNEL_PATH, &KERNEL_BOOT_PATH)
        .map_err(|_| "FAILED to install kernel in KERNEL_PATH")?;

    Ok(())
}

fn write_kernel_page<Host: Runtime>(
) -> impl FnMut(&mut Host, V0SliceContentPage) -> Result<(), &'static str> {
    let mut kernel_size = 0;
    move |host, page| {
        let written = append_content(host, kernel_size, page)?;
        kernel_size += written;
        Ok(())
    }
}

/// Appends the content of the page path given.
fn append_content<Host: Runtime>(
    host: &mut Host,
    kernel_size: usize,
    content: V0SliceContentPage,
) -> Result<usize, &'static str> {
    let content = content.as_ref();

    let mut size_written = 0;
    while size_written < content.len() {
        let num_to_write = usize::min(MAX_FILE_CHUNK_SIZE, content.len() - size_written);
        let bytes_to_write = &content[size_written..(size_written + num_to_write)];

        Runtime::store_write(
            host,
            &PREPARE_KERNEL_PATH,
            bytes_to_write,
            kernel_size + size_written,
        )
        .map_err(|_| "Failed to write kernel content page")?;

        size_written += num_to_write;
    }

    Ok(size_written)
}
