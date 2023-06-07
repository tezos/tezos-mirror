// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_host::runtime::Runtime;

/// Trait that represents a kernel
pub trait Kernel {
    /// Represents the `kernel_entry` functions defined by kernels
    fn entry<Host: Runtime>(host: &mut Host);
}
