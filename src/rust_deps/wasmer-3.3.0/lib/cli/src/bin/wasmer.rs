use wasmer_cli::cli::wasmer_main;

#[cfg(not(feature = "backend"))]
compile_error!(
    "Either enable at least one backend, or compile the wasmer-headless binary instead.\nWith cargo, you can provide a compiler option with the --features flag.\n\nExample values:\n\n\t\t--features cranelift,singlepass\n\t\t--features jsc\n\n\n"
);

fn main() {
    wasmer_main();
}
