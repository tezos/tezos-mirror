// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use host::rollup_core::RawRollupCore;
use host::runtime::Runtime;

use kernel::kernel_entry;
mod account;
mod error;
mod wei;

pub fn main<Host: Runtime + RawRollupCore>(host: &mut Host) {
}

kernel_entry!(main);

