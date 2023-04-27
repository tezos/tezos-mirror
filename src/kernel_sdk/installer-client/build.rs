// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=./installer.wasm");

    Command::new("sh")
        .arg("-c")
        .arg("cd .. && make installer.wasm")
        .output()
        .expect("Failed to build installer kernel.");
}
