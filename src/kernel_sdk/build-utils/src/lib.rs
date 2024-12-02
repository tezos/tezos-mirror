// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Utilities relating to compilation targets
pub mod target;

/// Generate configuration aliases as part of a Cargo build script.
pub fn generate_cfg_aliases() {
    target::generate_cfg_aliases();
}
