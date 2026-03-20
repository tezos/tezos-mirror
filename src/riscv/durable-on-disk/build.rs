// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::env;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("octez_riscv_durable_storage_on_disk_api.ml")
        .generate()
        .unwrap();
}

pub fn main() {
    if env::var("INSIDE_DUNE").is_err() {
        // Generate the Ocaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
