#[test]
pub fn test_macro_expansion() {
    // By default, this test fails if at least one expanded file is
    // missing or wrong. To generate or regenerate all expanded files,
    // call `MACROTEST=overwrite cargo test`.

    let macrotest_env_variable = "MACROTEST";
    let overwrite = "overwrite";

    match std::env::var_os(macrotest_env_variable) {
        None => macrotest::expand_without_refresh("tests/expand/*.rs"),
        Some(ref v) if v == overwrite => macrotest::expand("tests/expand/*.rs"),
        Some(v) => panic!(
            "Unrecognized value for environment variable {:?}, expected {:?} got {:?}",
            macrotest_env_variable, overwrite, v
        ),
    }
}
