// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

fn main() {
    // Generate the parser into OUT_DIR (rather than the source tree) so the
    // generated, unformatted `syntax.rs` never sits under `src/` where
    // `cargo fmt` would try to format it. It is pulled in with `lalrpop_mod!`.
    lalrpop::process_root().unwrap()
}
