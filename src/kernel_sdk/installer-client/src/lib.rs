// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

pub mod installer;
pub mod preimages;

use tezos_smart_rollup_host::path::RefPath;

// Path that we write the kernel to, before upgrading.
pub const PREPARE_KERNEL_PATH: RefPath =
    RefPath::assert_from(b"/installer/kernel/boot.wasm");

// Path of currently running kernel.
pub const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");
