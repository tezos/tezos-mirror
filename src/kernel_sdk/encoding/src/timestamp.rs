// SPDX-FileCopyrightText: 2020-2022 SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Timestamp representation corresponding to L1 one
//! Initially copied from tezedge
//! [messages crate](https://github.com/emturner/tezedge/blob/7f4e1cd6e29282a5b6b5128381f42dab345ab50a/tezos/messages/src/lib.rs)
#[cfg(feature = "alloc")]
pub use encoding::Timestamp;

#[cfg(feature = "alloc")]
mod encoding {
    use tezos_data_encoding::{
        enc::BinWriter,
        encoding::{Encoding, HasEncoding},
        nom::NomReader,
    };
    use time::{format_description::well_known::Rfc3339, OffsetDateTime};

    /// Timestamp representation corresponding to one used in L1 messages
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Timestamp(i64);

    impl Timestamp {
        /// Convert timestamp to signed 64 bit int
        pub fn i64(self) -> i64 {
            self.0
        }

        /// Convert timestamp to unsigned 64 bit int
        pub fn as_u64(self) -> u64 {
            self.0 as u64
        }
    }

    impl From<i64> for Timestamp {
        fn from(source: i64) -> Self {
            Self(source)
        }
    }

    impl From<Timestamp> for i64 {
        fn from(source: Timestamp) -> Self {
            source.0
        }
    }

    impl NomReader for Timestamp {
        fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
            nom::combinator::map(
                nom::number::complete::i64(nom::number::Endianness::Big),
                Timestamp,
            )(input)
        }
    }

    impl BinWriter for Timestamp {
        fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
            tezos_data_encoding::enc::i64(&self.0, output)
        }
    }

    impl HasEncoding for Timestamp {
        fn encoding() -> Encoding {
            Encoding::Timestamp
        }
    }

    impl std::fmt::Debug for Timestamp {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Display::fmt(self, f)
        }
    }

    impl std::fmt::Display for Timestamp {
        /// Display timestamp in the RFC3339 human readable format.
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let rfc_3339 = OffsetDateTime::from_unix_timestamp(self.0)
                .map_err(|_| String::from("timestamp out of range"))
                .and_then(|t| {
                    t.format(&Rfc3339)
                        .map_err(|_| String::from("invalid timestamp"))
                });

            match rfc_3339 {
                Ok(ts) => ts.fmt(f),
                Err(err) => write!(f, "<invalid timestamp: {err}>"),
            }
        }
    }
}
