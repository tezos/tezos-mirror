// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

mod physical_address;
mod pte;
mod virtual_address;

/// Offset of the `page offset` field in virtual and physical addresses.
const PAGE_OFFSET_WIDTH: usize = 12;
