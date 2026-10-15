// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::env;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("octez_riscv_durable_storage_in_memory_api.ml")
        .generate()
        .unwrap();
}

pub fn main() {
    // The generated OCaml signature files live in this package, so without an
    // explicit list cargo fingerprints the whole package by mtime and this
    // script invalidates itself on every build.
    println!("cargo:rerun-if-changed=src");
    // `INSIDE_DUNE` decides whether the signatures are generated.
    println!("cargo:rerun-if-env-changed=INSIDE_DUNE");

    if env::var("INSIDE_DUNE").is_err() {
        // Generate the Ocaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
