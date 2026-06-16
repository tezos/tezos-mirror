// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::env;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("src/octez_libcrux_ml_dsa.ml")
        .generate()
        .unwrap();
}

pub fn main() {
    if env::var("INSIDE_DUNE").is_err() {
        // Generate the OCaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
