// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![cfg_attr(not(feature = "std"), no_std)]

pub mod binary;
#[cfg(feature = "alloc")]
pub mod yaml;
