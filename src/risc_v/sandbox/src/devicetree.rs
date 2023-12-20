// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Additional resources on device trees:
//!   - https://elinux.org/Device_Tree_Usage
//!   - https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4

use rvemu::{bus::DRAM_BASE, dram::DRAM_SIZE};
use std::error::Error;
use vm_fdt::FdtWriter;

/// Information about the initial ramdisk.
pub struct InitialRamDisk {
    /// Start address of the initrd
    pub start: u64,

    /// Number of bytes in the initrd
    pub length: u64,
}

/// Create a new node scope in the device tree.
macro_rules! node {
    ( $i:ident, $name:expr, $inner:block ) => {
        let __current_node = $i.begin_node($name)?;

        {
            $inner
        }

        $i.end_node(__current_node)?;
    };

    ( $i:ident, $inner:block ) => {
        node!($i, "", $inner)
    };

    ( $i:ident, $name:expr, $addr:expr, $inner:block ) => {
        node!($i, format!("{}@{:x}", $name, $addr).as_str(), $inner)
    };
}

/// Generate a Flattened Device Tree for the current hardware configuration.
pub fn generate(initrd: Option<InitialRamDisk>) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut fdt = FdtWriter::new()?;

    // /
    node!(fdt, {
        // Cells are made up of multiple 32-bit unsigned integers.
        // The following specifies that address and size cells should be made up
        // of two elements. This makes them 64-bit wide.
        //
        // The `vm_fdt` crate takes care of translating 64-bit integers into
        // multiple 32-bit integers.
        //
        // Note, some cells combine address and size. With this configuration
        // below, a combined cell would be a 2-element array of 64-bit unsigned
        // integers that will be translated into a cell of 4 32-bit unsigned
        // integers.
        fdt.property_u32("#address-cells", 2)?;
        fdt.property_u32("#size-cells", 2)?;

        // Technically we're not emulating `virtio` in QEMU. However, this
        // seems to work with HermitOS so far.
        fdt.property_string("compatible", "riscv-virtio")?;
        fdt.property_string("model", "riscv-virtio,qemu")?;

        // /chosen
        node!(fdt, "chosen", {
            // HermitOS loader wants an initial ramdisk.
            if let Some(initrd) = initrd {
                // End pointer is exclusive (i.e. after the initrd).
                fdt.property_u64("linux,initrd-end", initrd.start + initrd.length)?;
                fdt.property_u64("linux,initrd-start", initrd.start)?;
            }
        });

        // /memory@?
        node!(fdt, "memory", DRAM_BASE, {
            fdt.property_string("device_type", "memory")?;
            fdt.property_array_u64("reg", &[DRAM_BASE, DRAM_SIZE])?;
        });

        // /cpus
        node!(fdt, "cpus", {
            // Addresses are 32-bit unsigned integers, and there are no sizes in
            // use in this section.
            fdt.property_u32("#address-cells", 1)?;
            fdt.property_u32("#size-cells", 0)?;

            // Supervisors will use this to figure out passing of time.
            fdt.property_u32("timebase-frequency", 10000000)?;

            // /cpus/cpu@0
            node!(fdt, "cpu", 0, {
                fdt.property_phandle(0x1)?;
                fdt.property_string("device_type", "cpu")?;
                fdt.property_u32("reg", 0x0)?;
                fdt.property_string("status", "okay")?;
                fdt.property_string("compatible", "riscv")?;
            });
        });
    });

    Ok(fdt.finish()?)
}
