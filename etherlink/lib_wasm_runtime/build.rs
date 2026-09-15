// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

fn main() {
    // The generated OCaml signature files live in this package, so without an
    // explicit list cargo fingerprints the whole package by mtime and this
    // script invalidates itself on every build.
    println!("cargo:rerun-if-changed=src");
    // `LINT` and `INSIDE_DUNE` select which file is generated.
    println!("cargo:rerun-if-env-changed=LINT");
    println!("cargo:rerun-if-env-changed=INSIDE_DUNE");

    let dune = std::env::var("INSIDE_DUNE").is_err();
    let lint = std::env::var("LINT").is_ok();

    if lint || dune {
        ocaml_build::Sigs::new(format!(
            "ocaml-api/{}wasm_runtime_gen.ml",
            if lint { ".expected-" } else { "" }
        ))
        .generate()
        .unwrap();
    }
}
