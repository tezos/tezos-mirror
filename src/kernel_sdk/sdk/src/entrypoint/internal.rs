// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#[cfg(feature = "extra")]
mod static_input_host;

#[cfg(feature = "extra")]
pub use static_input_host::StaticInbox;
