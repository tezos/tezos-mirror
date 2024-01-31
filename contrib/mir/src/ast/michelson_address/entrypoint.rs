/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Structures and utilities for [Tezos
//! entrypoints](https://docs.tezos.com/smart-contracts/entrypoints).

use std::collections::HashMap;

use crate::ast::annotations::FieldAnnotation;
use crate::ast::Type;

use super::ByteReprError;

/// Structure representing address entrypoint on a Tezos address, in other
/// words, the part after `%` in `KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo`.
/// Tezos entrypoints are ASCII strings of at most 31 characters long.
#[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq, Hash)]
pub struct Entrypoint(String);

/// A structure mapping from entrypoints to their types. This is simply an alias
/// for a [HashMap].
pub type Entrypoints = HashMap<Entrypoint, Type>;

impl std::fmt::Display for Entrypoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// NB: default entrypoint is represented as literal "default", because it
// affects comparison for addresses.
const DEFAULT_EP_NAME: &str = "default";
const MAX_EP_LEN: usize = 31;

impl Default for Entrypoint {
    fn default() -> Self {
        Entrypoint(DEFAULT_EP_NAME.to_owned())
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

impl TryFrom<FieldAnnotation<'_>> for Entrypoint {
    type Error = ByteReprError;

    /// NB: This only checks for the entrypoint length. `default` is sometimes
    /// forbidden when converting from field annotations, other times not, it's
    /// left to the discretion of the caller to make that check.
    fn try_from(x: FieldAnnotation<'_>) -> Result<Self, Self::Error> {
        let s = x.as_str();
        // we already checked for allowed characters when constructing a field
        // annotation, so here we only check length.
        check_ep_name_len(s.as_bytes())?;
        Ok(Entrypoint(s.to_owned()))
    }
}

pub(crate) fn check_ep_name_len(ep: &[u8]) -> Result<(), ByteReprError> {
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
}
