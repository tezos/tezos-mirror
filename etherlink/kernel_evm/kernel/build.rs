// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/*");
    let git_hash = match (
        option_env!("CI_COMMIT_SHA"),
        Command::new("git").args(["rev-parse", "HEAD"]).output(),
    ) {
        (Some(commit), _) => commit.to_string(),
        (_, Ok(output)) => String::from_utf8(output.stdout).unwrap(),
        (None, Err(_)) => "unknown version".to_string(),
    };
    println!("cargo:rustc-env=GIT_HASH={}", git_hash)
}
