// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

fn main() {
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
