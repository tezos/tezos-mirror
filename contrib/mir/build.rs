// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

fn main() {
    lalrpop::Configuration::new()
        .generate_in_source_tree()
        .process()
        .unwrap()
}
