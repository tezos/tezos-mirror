// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Smart contract entrypoint.
//!
//! Port of the *simple encoding* scheme from [entrypoint_repr.ml].
//!
//! [entrypoint_repr.ml]: <https://gitlab.com/tezos/tezos/-/blob/80b2cccb9c663dde2d86a6c94806fc149b7d1ef3/src/proto_alpha/lib_protocol/entrypoint_repr.ml>

use nom::branch;
use nom::bytes::complete::{take_while, take_while1};
use nom::combinator::{self, map, map_res, recognize};
use nom::sequence::pair;
use tezos_data_encoding::enc::{self, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::has_encoding;
use tezos_data_encoding::nom::{bounded_dynamic, NomReader};

/// The entrypoint of a smart contract.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entrypoint {
    name: String,
}

impl Entrypoint {
    const MAX_LEN: usize = 31;
    const DEFAULT: &'static str = "default";

    /// Entrypoint name, 1-31 bytes long.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl Default for Entrypoint {
    fn default() -> Self {
        Self {
            name: String::from(Self::DEFAULT),
        }
    }
}

/// Possible errors when creating entrypoints.
#[derive(Debug, PartialEq, Eq)]
pub enum EntrypointError {
    /// The maximum size of an entrpoint is 31 bytes.
    TooLarge(String),
    /// Entrypoint must match with `([A-Za-z0-9_][A-Za-z0-9_.%@]*)?`.
    InvalidChars(String),
}

impl TryFrom<String> for Entrypoint {
    type Error = EntrypointError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        if name.is_empty() {
            return Ok(Self::default());
        } else if name.len() > Self::MAX_LEN {
            return Err(EntrypointError::TooLarge(name));
        };

        let first_char_valid = match name.as_bytes()[0] {
            b'_' => true,
            c => c.is_ascii_alphanumeric(),
        };

        if first_char_valid
            && name[1..].bytes().all(|c: u8| {
                c.is_ascii_alphanumeric() || matches!(c, b'_' | b'.' | b'%' | b'@')
            })
        {
            Ok(Entrypoint { name })
        } else {
            Err(EntrypointError::InvalidChars(name))
        }
    }
}

has_encoding!(Entrypoint, ENTRYPOINT_SIMPLE_ENCODING, { Encoding::Custom });

impl NomReader for Entrypoint {
    fn nom_read(input: &[u8]) -> tezos_data_encoding::nom::NomResult<Self> {
        map(
            // Can't use tezos_data_encoding 'bounded_string' here, as we have an
            // additional constraint on the string, that need to match entrypoint encoding
            map_res(
                bounded_dynamic(
                    Self::MAX_LEN,
                    branch::alt((
                        combinator::eof,
                        recognize(pair(
                            take_while1(|byte: u8| {
                                byte.is_ascii_alphanumeric() || byte == b'_'
                            }),
                            take_while(|byte: u8| match byte {
                                b'_' | b'.' | b'@' | b'%' => true,
                                b => b.is_ascii_alphanumeric(),
                            }),
                        )),
                    )),
                ),
                |bytes| alloc::str::from_utf8(bytes).map(str::to_string),
            ),
            |name| {
                if name.is_empty() {
                    Self::default()
                } else {
                    Self { name }
                }
            },
        )(input)
    }
}

impl BinWriter for Entrypoint {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        enc::bounded_string(Self::MAX_LEN)(&self.name, output)
    }
}

#[cfg(feature = "testing")]
mod testing {
    use super::*;
    use proptest::prelude::*;
    use proptest::string::string_regex;

    impl Entrypoint {
        /// Generate an arbitrary entrypoint
        pub fn arb() -> BoxedStrategy<Entrypoint> {
            string_regex("([A-Za-z0-9_][A-Za-z0-9._%@]*)?")
                .unwrap()
                .prop_map(|mut s| {
                    s.truncate(Entrypoint::MAX_LEN);
                    Entrypoint::try_from(s).unwrap()
                })
                .boxed()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn default_entrypoint() {
        let default = Entrypoint::default();
        assert_eq!("default", default.name());
        assert_eq!(
            default,
            Entrypoint {
                name: "default".into()
            }
        );
        assert_eq!(default, Entrypoint::try_from("".to_string()).unwrap());
        assert_eq!(
            default,
            Entrypoint::try_from("default".to_string()).unwrap()
        );

        let mut bin = Vec::new();
        default.bin_write(&mut bin).unwrap();

        assert_eq!(
            vec![0, 0, 0, 7, b'd', b'e', b'f', b'a', b'u', b'l', b't'],
            bin
        );

        let parsed = Ok(([].as_slice(), default));
        assert_eq!(parsed, Entrypoint::nom_read(bin.as_slice()));

        assert_eq!(
            parsed,
            Entrypoint::nom_read(
                [0, 0, 0, 7, b'd', b'e', b'f', b'a', b'u', b'l', b't'].as_slice()
            )
        );
    }

    #[test]
    fn encode_decode_non_default() {
        let entrypoint = Entrypoint::try_from("an_entrypoint".to_string()).unwrap();

        let mut bin = Vec::new();
        entrypoint
            .bin_write(&mut bin)
            .expect("serialization should work");

        let (remaining, deserde) =
            Entrypoint::nom_read(bin.as_slice()).expect("deserialization should work");

        assert!(remaining.is_empty());

        assert_eq!(entrypoint, deserde);
    }

    #[test]
    fn too_large_entrypoint() {
        let is_ok = vec![b'E'; 31];
        let large = vec![b'E'; 32];

        let ok_name = String::from_utf8(is_ok).unwrap();
        let large_name = String::from_utf8(large).unwrap();

        assert!(Entrypoint::try_from(ok_name).is_ok());
        assert_eq!(
            Err(EntrypointError::TooLarge(large_name.clone())),
            Entrypoint::try_from(large_name)
        );

        let mut is_ok = vec![0, 0, 0, 31];
        let mut large = vec![0, 0, 0, 32];

        is_ok.append(vec![b'A'; 31].as_mut());
        large.append(vec![b'A'; 32].as_mut());

        assert!(Entrypoint::nom_read(is_ok.as_slice()).is_ok());
        assert!(Entrypoint::nom_read(large.as_slice()).is_err());
    }

    #[test]
    fn non_valid_entrypoint() {
        let invalid_name = String::from("a-");

        assert_eq!(
            Err(EntrypointError::InvalidChars(invalid_name.clone())),
            Entrypoint::try_from(invalid_name)
        );

        let invalid = vec![0, 0, 0, 4, 0xe2, 0x8d, 0xa8, b'a'];

        assert!(Entrypoint::nom_read(invalid.as_slice()).is_err());
    }

    proptest! {
        #[test]
        fn encode_decode_valid_entrypoint(entrypoint in Entrypoint::arb(),
                                          remaining_input in any::<Vec<u8>>()) {
            let mut encoded = Vec::new();
            entrypoint.bin_write(&mut encoded).expect("encoding entrypoint should work");
            encoded.extend_from_slice(remaining_input.as_slice());

            let (remaining, decoded) = Entrypoint::nom_read(encoded.as_slice())
                .expect("decoding entrypoint should work");

            assert_eq!(remaining, remaining_input.as_slice());

            assert_eq!(entrypoint, decoded);
        }
    }
}
