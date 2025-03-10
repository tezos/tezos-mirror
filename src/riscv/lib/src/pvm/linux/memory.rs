// SPDX-FileCopyrightText: 2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::machine_state::memory::PAGE_SIZE;

/// Number of pages that make up the stack
const STACK_PAGES: u64 = 0x2000;

/// Maximum stack size in bytes
pub const STACK_SIZE: u64 = PAGE_SIZE.get() * STACK_PAGES;
