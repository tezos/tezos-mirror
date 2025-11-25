// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Structures and utilities for [Tezos
//! entrypoints](https://docs.tezos.com/smart-contracts/entrypoints).

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, map_res, verify};
use nom::multi::length_data;
use nom::number::complete::u8 as nom_u8;
use nom::sequence::preceded;

use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::{NomReader, NomResult};

/// Errors that can happen when parsing bytes.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ByteReprError {
    /// Input format is in some way unexpected, with the details explained in
    /// the contained string.
    #[error("wrong format: {0}")]
    WrongFormat(String),
}

/// Structure representing address entrypoint on a Tezos address, in other
/// words, the part after `%` in `KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo`.
/// Tezos entrypoints are ASCII strings of at most 31 characters long.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq, Hash)]
pub struct Entrypoint(String);

impl std::fmt::Display for Entrypoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// NB: default entrypoint is represented as literal "default", because it
/// affects comparison for addresses.
pub const DEFAULT_EP_NAME: &str = "default";
const MAX_EP_LEN: usize = 31;

impl Default for Entrypoint {
    fn default() -> Self {
        Entrypoint(DEFAULT_EP_NAME.to_owned())
    }
}

#[derive(Copy, Clone)]
enum EntrypointTag {
    Default = 0,
    Root = 1,
    Do = 2,
    SetDelegate = 3,
    RemoveDelegate = 4,
    Deposit = 5,
    Stake = 6,
    Unstake = 7,
    FinalizeUnstake = 8,
    SetDelegateParameters = 9,
    Custom = 255,
}

impl EntrypointTag {
    #[allow(dead_code)]
    fn from_str(name: &str) -> Self {
        match name {
            "" | DEFAULT_EP_NAME => Self::Default,
            "root" => Self::Root,
            "do" => Self::Do,
            "set_delegate" => Self::SetDelegate,
            "remove_delegate" => Self::RemoveDelegate,
            "deposit" => Self::Deposit,
            "stake" => Self::Stake,
            "unstake" => Self::Unstake,
            "finalize_unstake" => Self::FinalizeUnstake,
            "set_delegate_parameters" => Self::SetDelegateParameters,
            _ => Self::Custom,
        }
    }

    #[allow(dead_code)]
    fn to_str(self) -> &'static str {
        match self {
            Self::Default => DEFAULT_EP_NAME,
            Self::Root => "root",
            Self::Do => "do",
            Self::SetDelegate => "set_delegate",
            Self::RemoveDelegate => "remove_delegate",
            Self::Deposit => "deposit",
            Self::Stake => "stake",
            Self::Unstake => "unstake",
            Self::FinalizeUnstake => "finalize_unstake",
            Self::SetDelegateParameters => "set_delegate_parameters",
            Self::Custom => "custom",
        }
    }
}

impl BinWriter for Entrypoint {
    fn bin_write(&self, output: &mut Vec<u8>) -> tezos_data_encoding::enc::BinResult {
        let tag = EntrypointTag::from_str(&self.0);
        output.push(tag as u8);

        if matches!(tag, EntrypointTag::Custom) {
            let bytes = self.0.as_bytes();
            output.push(bytes.len() as u8);
            output.extend_from_slice(bytes);
        }

        Ok(())
    }
}

impl NomReader<'_> for Entrypoint {
    fn nom_read(input: &[u8]) -> NomResult<Self> {
        alt((
            map(tag([EntrypointTag::Default as u8]), |_| {
                Entrypoint::default()
            }),
            map(tag([EntrypointTag::Root as u8]), |_| {
                Entrypoint("root".into())
            }),
            map(tag([EntrypointTag::Do as u8]), |_| Entrypoint("do".into())),
            map(tag([EntrypointTag::SetDelegate as u8]), |_| {
                Entrypoint("set_delegate".into())
            }),
            map(tag([EntrypointTag::RemoveDelegate as u8]), |_| {
                Entrypoint("remove_delegate".into())
            }),
            map(tag([EntrypointTag::Deposit as u8]), |_| {
                Entrypoint("deposit".into())
            }),
            map(tag([EntrypointTag::Stake as u8]), |_| {
                Entrypoint("stake".into())
            }),
            map(tag([EntrypointTag::Unstake as u8]), |_| {
                Entrypoint("unstake".into())
            }),
            map(tag([EntrypointTag::FinalizeUnstake as u8]), |_| {
                Entrypoint("finalize_unstake".into())
            }),
            map(tag([EntrypointTag::SetDelegateParameters as u8]), |_| {
                Entrypoint("set_delegate_parameters".into())
            }),
            preceded(
                tag([EntrypointTag::Custom as u8]),
                map(
                    verify(
                        map_res(length_data(nom_u8), std::str::from_utf8),
                        |s: &str| check_ep_name(s.as_bytes()).is_ok(),
                    ),
                    |s| Entrypoint(s.to_owned()),
                ),
            ),
        ))(input)
    }
}

impl Entrypoint {
    /// Returns `true` if entrypoint is the default entrypoint.
    pub fn is_default(&self) -> bool {
        self.0 == DEFAULT_EP_NAME
    }

    /// Returns a reference to the entrypoint name as bytes.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Returns a reference to the entrypoint name as [str].
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl TryFrom<&str> for Entrypoint {
    type Error = ByteReprError;
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Entrypoint::try_from(s.to_owned())
    }
}

impl TryFrom<String> for Entrypoint {
    type Error = ByteReprError;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        if s.is_empty() {
            Ok(Entrypoint::default())
        } else {
            check_ep_name(s.as_bytes())?;
            Ok(Entrypoint(s))
        }
    }
}

impl TryFrom<&[u8]> for Entrypoint {
    type Error = ByteReprError;
    fn try_from(s: &[u8]) -> Result<Self, ByteReprError> {
        if s.is_empty() {
            Ok(Entrypoint::default())
        } else {
            check_ep_name(s)?;
            // SAFETY: we just checked all bytes are valid ASCII
            let ep = Entrypoint(unsafe { std::str::from_utf8_unchecked(s).to_owned() });
            if ep.is_default() {
                return Err(ByteReprError::WrongFormat(
                    "explicit default entrypoint is forbidden in binary encoding".to_owned(),
                ));
            }
            Ok(ep)
        }
    }
}

impl Entrypoint {
    /// Converts a String to an Entrypoint without checking it validity.
    ///
    /// # Safety
    ///
    /// - The string passed in must be at most 31 characters long.
    /// - The string passed in must respect https://tezos.gitlab.io/alpha/michelson.html#syntax.
    ///
    pub fn from_string_unchecked(s: String) -> Entrypoint {
        Self(s)
    }
}

// Exposed only for `mir`.
pub fn check_ep_name_len(ep: &[u8]) -> Result<(), ByteReprError> {
    if ep.len() > MAX_EP_LEN {
        return Err(ByteReprError::WrongFormat(format!(
            "entrypoint name must be at most {} characters long, but it is {} characters long",
            MAX_EP_LEN,
            ep.len()
        )));
    }
    Ok(())
}

fn check_ep_name(ep: &[u8]) -> Result<(), ByteReprError> {
    check_ep_name_len(ep)?;
    let mut first_char = true;
    for c in ep {
        // direct encoding of the regex defined in
        // https://tezos.gitlab.io/alpha/michelson.html#syntax
        match c {
            b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' => Ok(()),
            b'.' | b'%' | b'@' if !first_char => Ok(()),
            c => Err(ByteReprError::WrongFormat(format!(
                "forbidden byte in entrypoint name: {}",
                hex::encode([*c])
            ))),
        }?;
        first_char = false;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default() {
        assert_eq!(
            Entrypoint::default(),
            Entrypoint(DEFAULT_EP_NAME.to_owned())
        )
    }

    #[test]
    fn test_from_str() {
        assert_eq!(Entrypoint::try_from(""), Ok(Entrypoint::default()));
        assert_eq!(Entrypoint::try_from("default"), Ok(Entrypoint::default()));
        assert_eq!(
            Entrypoint::try_from("foo"),
            Ok(Entrypoint("foo".to_owned()))
        );
        assert_eq!(
            Entrypoint::try_from("foo.bar"),
            Ok(Entrypoint("foo.bar".to_owned()))
        );
        assert_eq!(
            Entrypoint::try_from("q".repeat(31).as_str()),
            Ok(Entrypoint("q".repeat(31)))
        );
        // too long
        assert!(matches!(
            Entrypoint::try_from("q".repeat(32).as_str()),
            Err(ByteReprError::WrongFormat(_))
        ));
        // unicode
        assert!(matches!(
            Entrypoint::try_from("संसर"),
            Err(ByteReprError::WrongFormat(_))
        ));
        // forbidden character
        assert!(matches!(
            Entrypoint::try_from("!"),
            Err(ByteReprError::WrongFormat(_))
        ));
    }

    #[test]
    fn test_from_string() {
        // most of this is tested in test_from_str, as one delegates to the
        // other, so only the basic tests here
        assert_eq!(
            Entrypoint::try_from("".to_owned()),
            Ok(Entrypoint::default())
        );
        assert_eq!(
            Entrypoint::try_from("default".to_owned()),
            Ok(Entrypoint::default())
        );
        assert_eq!(
            Entrypoint::try_from("foo".to_owned()),
            Ok(Entrypoint("foo".to_owned()))
        );
    }

    #[test]
    fn test_from_bytes() {
        // explicit default entrypoints are forbidden in binary
        assert!(matches!(
            Entrypoint::try_from(b"default" as &[u8]),
            Err(ByteReprError::WrongFormat(_))
        ));
        assert_eq!(
            Entrypoint::try_from(b"" as &[u8]),
            Ok(Entrypoint::default())
        );
        assert_eq!(
            Entrypoint::try_from(b"foo" as &[u8]),
            Ok(Entrypoint("foo".to_owned()))
        );

        assert_eq!(
            Entrypoint::try_from(b"foo.bar" as &[u8]),
            Ok(Entrypoint("foo.bar".to_owned()))
        );
        assert_eq!(
            Entrypoint::try_from("q".repeat(31).as_bytes()),
            Ok(Entrypoint("q".repeat(31)))
        );
        // too long
        assert!(matches!(
            Entrypoint::try_from("q".repeat(32).as_bytes()),
            Err(ByteReprError::WrongFormat(_))
        ));
        // unicode
        assert!(matches!(
            Entrypoint::try_from("संसर".as_bytes()),
            Err(ByteReprError::WrongFormat(_))
        ));
        // forbidden character
        assert!(matches!(
            Entrypoint::try_from(b"!" as &[u8]),
            Err(ByteReprError::WrongFormat(_))
        ));
    }

    #[test]
    fn test_check_ep_name() {
        assert_eq!(check_ep_name(&[b'q'; 31]), Ok(()));

        // more than 31 bytes
        assert!(matches!(
            check_ep_name(&[b'q'; 32]),
            Err(ByteReprError::WrongFormat(_))
        ));

        // '.', '%', '@' are allowed
        for i in ['.', '%', '@'] {
            assert_eq!(check_ep_name(format!("foo{i}bar").as_bytes()), Ok(()));

            // but not as the first character
            assert!(matches!(
                check_ep_name(format!("{i}bar").as_bytes()),
                Err(ByteReprError::WrongFormat(_))
            ));
        }

        // ! is forbidden
        assert!(matches!(
            check_ep_name(b"foo!"),
            Err(ByteReprError::WrongFormat(_))
        ));

        // unicode is forbidden
        assert!(matches!(
            check_ep_name("नमस्ते".as_bytes()),
            Err(ByteReprError::WrongFormat(_))
        ));
    }

    #[test]
    fn test_nom_read_known_entrypoints() {
        let tests = [
            (vec![EntrypointTag::Default as u8], Entrypoint::default()),
            (vec![EntrypointTag::Root as u8], Entrypoint("root".into())),
            (vec![EntrypointTag::Do as u8], Entrypoint("do".into())),
            (vec![EntrypointTag::Stake as u8], Entrypoint("stake".into())),
        ];

        for (input, expected) in tests.iter() {
            let result = Entrypoint::nom_read(input).unwrap();
            assert_eq!(&result.1, expected);
        }
    }

    #[test]
    fn test_bin_write_known_entrypoints() {
        let tests = [
            (Entrypoint::default(), vec![EntrypointTag::Default as u8]),
            (Entrypoint("root".into()), vec![EntrypointTag::Root as u8]),
            (Entrypoint("do".into()), vec![EntrypointTag::Do as u8]),
            (Entrypoint("stake".into()), vec![EntrypointTag::Stake as u8]),
        ];

        for (entrypoint, expected) in tests.iter() {
            let mut output = vec![];
            entrypoint.bin_write(&mut output).unwrap();
            assert_eq!(&output, expected);
        }
    }

    #[test]
    fn test_nom_read_custom_entrypoint() {
        let input = vec![EntrypointTag::Custom as u8, 5, b'h', b'e', b'l', b'l', b'o'];
        let expected = Entrypoint("hello".into());

        let result = Entrypoint::nom_read(&input).unwrap();
        assert_eq!(result.1, expected);
    }

    #[test]
    fn test_bin_write_custom_entrypoint() {
        let entrypoint = Entrypoint("my_custom_ep".into());
        let mut output = vec![];
        entrypoint.bin_write(&mut output).unwrap();

        let expected = {
            let mut exp = vec![EntrypointTag::Custom as u8, 12]; // 12 bytes for "my_custom_ep"
            exp.extend_from_slice(b"my_custom_ep");
            exp
        };

        assert_eq!(output, expected);
    }

    #[test]
    fn test_round_trip_custom_entrypoint() {
        let original = Entrypoint("test_ep".into());
        let mut output = vec![];
        original.bin_write(&mut output).unwrap();

        let parsed = Entrypoint::nom_read(&output).unwrap().1;
        assert_eq!(original, parsed);
    }

    #[test]
    fn test_round_trip_all_known_entrypoints() {
        let entrypoints = [
            EntrypointTag::Default,
            EntrypointTag::Root,
            EntrypointTag::Do,
            EntrypointTag::SetDelegate,
            EntrypointTag::RemoveDelegate,
            EntrypointTag::Deposit,
            EntrypointTag::Stake,
            EntrypointTag::Unstake,
            EntrypointTag::FinalizeUnstake,
            EntrypointTag::SetDelegateParameters,
        ];

        for &ep_tag in entrypoints.iter() {
            let ep_name = ep_tag.to_str();
            let ep = Entrypoint(ep_name.into());

            let mut output = vec![];
            ep.bin_write(&mut output).unwrap();

            let parsed = Entrypoint::nom_read(&output).unwrap().1;
            assert_eq!(ep, parsed);
        }
    }
}
