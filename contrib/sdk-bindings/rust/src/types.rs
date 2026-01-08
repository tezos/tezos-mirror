// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;

#[derive(Debug, Clone, Eq, PartialEq, uniffi::Object)]
#[uniffi::export(Debug, Display, Eq)]
pub struct BigInt(pub(crate) num_bigint::BigInt);

impl std::fmt::Display for BigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[uniffi::export]
impl BigInt {
    #[uniffi::constructor]
    pub fn from_int(num: i64) -> Self {
        Self(num_bigint::BigInt::from(num))
    }

    #[uniffi::constructor]
    pub fn from_string(str: &str) -> Result<Self, Error> {
        use std::str::FromStr;
        num_bigint::BigInt::from_str(str)
            .map(Self)
            .map_err(|err| Error::Parsing(err.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_big_int_from_int {
        ($name:ident, $i64:expr, $big_int:expr) => {
            #[test]
            fn $name() {
                let big_int = BigInt::from_int($i64);
                assert_eq!(big_int.0, $big_int, "BigInts should be equal");
            }
        };
    }

    test_big_int_from_int!(build_zero_big_int_from_int, 0, num_bigint::BigInt::ZERO);

    test_big_int_from_int!(
        build_positiv_big_int_from_int,
        17,
        num_bigint::BigInt::from(17)
    );

    test_big_int_from_int!(
        build_negativ_big_int_from_int,
        -17,
        num_bigint::BigInt::from(-17)
    );

    test_big_int_from_int!(
        build_very_large_big_int_from_int,
        9223372036854775807,
        num_bigint::BigInt::from(i64::MAX)
    );

    test_big_int_from_int!(
        build_very_small_big_int_from_int,
        -9223372036854775808,
        num_bigint::BigInt::from(i64::MIN)
    );

    macro_rules! test_big_int_from_string {
        ($name:ident, $str:expr, $big_int:expr) => {
            #[test]
            fn $name() {
                let big_int =
                    BigInt::from_string($str).expect("Building from a correct integer format");
                assert_eq!(big_int.0, $big_int, "BigInts should be equal");
            }
        };
    }

    test_big_int_from_string!(
        build_zero_big_int_from_string,
        "0",
        num_bigint::BigInt::ZERO
    );

    test_big_int_from_string!(
        build_positiv_big_int_from_string,
        "17",
        num_bigint::BigInt::from(17)
    );

    test_big_int_from_string!(
        build_negativ_big_int_from_string,
        "-17",
        num_bigint::BigInt::from(-17)
    );

    test_big_int_from_string!(
        build_very_large_big_int_from_string,
        "9223372036854775808",
        num_bigint::BigInt::from(i64::MAX) + 1
    );

    test_big_int_from_string!(
        build_very_small_big_int_from_string,
        "-9223372036854775809",
        num_bigint::BigInt::from(i64::MIN) - 1
    );

    #[test]
    fn fail_build_wrong_formatted_big_int_from_string() {
        let big_int = BigInt::from_string("big_int");

        assert!(
            matches!(big_int, Err(Error::Parsing(_))),
            "Build BigInt with wrong string format must fail with Parsing(_) error"
        );
    }
}
