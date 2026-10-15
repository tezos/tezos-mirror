// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>

pub fn main() {
    // The generated OCaml signature files live in this package, so without an
    // explicit list cargo fingerprints the whole package by mtime and this
    // script invalidates itself on every build.
    println!("cargo:rerun-if-changed=src");
    // `INSIDE_DUNE` decides whether the signatures are generated.
    println!("cargo:rerun-if-env-changed=INSIDE_DUNE");

    // does not generate the bindings when building with dune, they are
    // generated only manually.
    let not_in_dune = std::env::var("INSIDE_DUNE").is_err();

    if not_in_dune {
        ocaml_build::Sigs::new("ocaml-api/igd_next_gen.ml")
            .generate()
            .unwrap();
    }
}
