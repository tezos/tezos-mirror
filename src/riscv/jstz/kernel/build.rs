// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

fn main() {
    println!("cargo:rerun-if-env-changed=INBOX_FILE");
}
