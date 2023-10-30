extern crate cbindgen;

use std::env;

fn generate_c_headers() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("octez_risc_v_pvm.h");
}

fn main() {
    if env::var("INSIDE_DUNE").is_ok() {
        // Generate the C headers only when building through the Dune rule.
        generate_c_headers();
    }
}
