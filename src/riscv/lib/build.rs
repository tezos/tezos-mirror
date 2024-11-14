use std::env;

fn generate_ocaml_sigs() {
    ocaml_build::Sigs::new("octez_riscv_api.ml")
        .generate()
        .unwrap();
}

pub fn main() {
    if env::var("INSIDE_DUNE").is_err() {
        // Generate the Ocaml signatures only when building outside the Dune rule.
        generate_ocaml_sigs();
    }
}
