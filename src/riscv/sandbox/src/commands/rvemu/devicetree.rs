// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Additional resources on device trees:
//!   - https://elinux.org/Device_Tree_Usage
//!   - https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4

use rvemu::{bus::DRAM_BASE, dram::DRAM_SIZE};
use std::error::Error;

/// Information about the initial ramdisk.
pub use octez_riscv::devicetree::InitialRamDisk;

/// Generate a Flattened Device Tree for the current hardware configuration.
pub fn generate(initrd: Option<InitialRamDisk>) -> Result<Vec<u8>, Box<dyn Error>> {
    Ok(octez_riscv::devicetree::generate_custom(
        DRAM_BASE, DRAM_SIZE, initrd,
    )?)
}
