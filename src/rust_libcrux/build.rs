// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::env;
use std::fs;
use std::path::Path;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("src/octez_libcrux_ml_dsa.ml")
        .generate()
        .unwrap();
}

// The generated OCaml signature files live in `src` next to the Rust sources,
// so tracking the directory would make this script invalidate itself. Track
// the Rust sources one by one instead: a new module also edits its parent.
fn rerun_if_rust_sources_changed(dir: &Path) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            rerun_if_rust_sources_changed(&path);
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }
}

pub fn main() {
    rerun_if_rust_sources_changed(Path::new("src"));
    // `INSIDE_DUNE` decides whether the signatures are generated.
    println!("cargo:rerun-if-env-changed=INSIDE_DUNE");

    if env::var("INSIDE_DUNE").is_err() {
        // Generate the OCaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
