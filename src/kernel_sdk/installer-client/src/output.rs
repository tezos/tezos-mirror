// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ffi::OsStr;
use std::fs::write;
use std::path::Path;

/// Write kernel to `kernel_path`.
///
/// Attempts to encode kernel as either hex or wasm, depending on extension.
pub fn save_kernel(kernel_path: &Path, kernel: &[u8]) -> std::io::Result<()> {
    if kernel_path.extension() == Some(OsStr::new("wasm")) {
        return write(kernel_path, kernel);
    }

    if kernel_path.extension() != Some(OsStr::new("hex")) {
        eprintln!("Expected either 'hex' or 'wasm' for extension. Defaulting to hex.");
    }

    let kernel = hex::encode(kernel);
    write(kernel_path, kernel)
}
