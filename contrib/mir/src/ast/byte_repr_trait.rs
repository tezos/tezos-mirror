/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Trait for values representable by either raw bytes or base58check-derived
//! strings and related types.

use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_crypto_rs::hash::FromBytesError;

/// Errors that can happen when working with [ByteReprTrait].
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ByteReprError {
    /// Encountered an unknown prefix while trying to decode a value either from
    /// bytes or base58-check string.
    #[error("unknown prefix: {0}")]
    UnknownPrefix(String),
    /// Input format is in some way unexpected, with the details explained in
    /// the contained string.
    #[error("wrong format: {0}")]
    WrongFormat(String),
}

impl From<FromBase58CheckError> for ByteReprError {
    fn from(value: FromBase58CheckError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}

impl From<FromBytesError> for ByteReprError {
    fn from(value: FromBytesError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}

/// Trait for values representable by either raw bytes or base58check-derived
/// strings.
pub trait ByteReprTrait: Sized {
    /// Try to construct `Self` from base58-check encoded string slice.
    fn from_base58_check(data: &str) -> Result<Self, ByteReprError>;

    /// Try to construct `Self` from a raw byte slice. Note the slice is _not_
    /// expected to be base58-encoded.
    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError>;

    /// Construct base58-check representation of `Self`.
    fn to_base58_check(&self) -> String;

    /// Write raw byte represenation of `Self` to the output vector. Note this
    /// is _not_ base58-check encoded.
    fn to_bytes(&self, out: &mut Vec<u8>);

    /// Convenience function to construct a new [Vec] and write raw byte
    /// representation of `Self` to it.
    fn to_bytes_vec(&self) -> Vec<u8> {
        let mut out = Vec::new();
        self.to_bytes(&mut out);
        out
    }
}
