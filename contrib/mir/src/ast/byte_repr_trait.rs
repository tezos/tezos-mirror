/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_crypto_rs::hash::FromBytesError;

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ByteReprError {
    #[error("unknown prefix: {0}")]
    UnknownPrefix(String),
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
    fn from_base58_check(data: &str) -> Result<Self, ByteReprError>;

    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError>;

    fn to_base58_check(&self) -> String;

    fn to_bytes(&self, out: &mut Vec<u8>);

    fn to_bytes_vec(&self) -> Vec<u8> {
        let mut out = Vec::new();
        self.to_bytes(&mut out);
        out
    }
}
