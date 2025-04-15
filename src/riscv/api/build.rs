use std::env;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("octez_riscv_api.ml")
        .generate()
        .unwrap();
}

pub fn main() {
    if env::var("CARGO_FEATURE_DEFAULT").is_ok() && env::var("CARGO_FEATURE_CI").is_ok() {
        panic!("Feature 'default' and 'ci' are mutually exclusive");
    }

    if env::var("INSIDE_DUNE").is_err() {
        // Generate the Ocaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
