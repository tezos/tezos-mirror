// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>

pub fn main() {
    // does not generate the bindings when building with dune, they are
    // generated only manually.
    let not_in_dune = std::env::var("INSIDE_DUNE").is_err();

    if not_in_dune {
        ocaml_build::Sigs::new("ocaml-api/rust_tezedge_gen.ml")
            .generate()
            .unwrap();
    }
}
