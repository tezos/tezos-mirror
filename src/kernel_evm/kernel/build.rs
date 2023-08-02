// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/*");
    let git_hash = match Command::new("git").args(["rev-parse", "HEAD"]).output() {
        Ok(output) => String::from_utf8(output.stdout).unwrap(),
        Err(_) => env!("CI_COMMIT_SHA").to_string(),
    };
    println!("cargo:rustc-env=GIT_HASH={}", git_hash)
}
