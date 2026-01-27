// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-License-Identifier: MIT

use tezos_data_encoding::{enc::BinWriter, nom::NomReader};

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
            "Unrecognized value for environment variable {macrotest_env_variable:?}, expected {overwrite:?} got {v:?}"
        ),
    }
}

pub fn check_roundtrip<T: std::fmt::Debug + PartialEq + BinWriter + for<'a> NomReader<'a>>(t: T) {
    let mut serialized = vec![];
    t.bin_write(&mut serialized)
        .expect("serialization should succeed");
    let (remaining_input, deserialized) =
        T::nom_read(&serialized).expect("deserialization should succeed");
    assert!(remaining_input.is_empty());
    assert_eq!(t, deserialized)
}

pub fn check_serialization<T: BinWriter>(t: T, expected: &[u8]) {
    let mut serialized = vec![];
    t.bin_write(&mut serialized)
        .expect("serialization should succeed");
    assert_eq!(serialized, expected)
}

mod doc_example {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    struct S {
        a: i32,
        b: Option<Vec<bool>>,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(S {
            a: -42,
            b: Some(vec![true, false, false]),
        })
    }
}

mod enum_implicit_tags {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    enum E {
        A,
        B,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(E::A);
        check_roundtrip(E::B);
        check_serialization(E::A, &[0]);
        check_serialization(E::B, &[1])
    }
}

mod enum_explicit_tags {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    enum E {
        #[encoding(tag = 4)]
        A,
        #[encoding(tag = 12)]
        B,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(E::A);
        check_roundtrip(E::B);
        check_serialization(E::A, &[4]);
        check_serialization(E::B, &[12])
    }
}

mod enum_large_tags {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    #[encoding(tags = "u16")]
    enum E {
        #[encoding(tag = 256)]
        A,
        #[encoding(tag = 257)]
        B,
        #[encoding(tag = 10)]
        C,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(E::A);
        check_roundtrip(E::B);
        check_roundtrip(E::C);
        check_serialization(E::A, &[1, 0]);
        check_serialization(E::B, &[1, 1]);
        check_serialization(E::C, &[0, 10])
    }
}

mod struct_unit {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    struct Unit {
        unit: (),
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(Unit { unit: () });
        check_serialization(Unit { unit: () }, &[])
    }
}

mod struct_dynamic {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    struct Bytes {
        bytes: Vec<u8>,
    }

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    #[encoding(dynamic)]
    struct DynBytes {
        bytes: Vec<u8>,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(Bytes {
            bytes: vec![1, 2, 3],
        });
        check_roundtrip(DynBytes {
            bytes: vec![1, 2, 3],
        });
        check_serialization(
            Bytes {
                bytes: vec![1, 2, 3],
            },
            &[1, 2, 3],
        );
        check_serialization(
            DynBytes {
                bytes: vec![1, 2, 3],
            },
            &[0, 0, 0, 3, 1, 2, 3],
        )
    }
}

mod struct_bytes_twice {
    use super::*;

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    struct S {
        #[encoding(dynamic, list)]
        bytes_1: Vec<u8>,
        bytes_2: Vec<u8>,
    }

    #[test]
    fn test_roundtrip() {
        check_roundtrip(S {
            bytes_1: vec![1, 2, 3],
            bytes_2: vec![2, 3, 4],
        });
        check_serialization(
            S {
                bytes_1: vec![1, 2, 3],
                bytes_2: vec![2, 3, 4],
            },
            &[0, 0, 0, 3, 1, 2, 3, 2, 3, 4],
        )
    }
}
