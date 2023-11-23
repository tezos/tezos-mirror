/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_crypto_rs::hash::FromBytesError;

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum AddressError {
    #[error("unknown address prefix: {0}")]
    UnknownPrefix(String),
    #[error("wrong address format: {0}")]
    WrongFormat(String),
}

impl From<FromBase58CheckError> for AddressError {
    fn from(value: FromBase58CheckError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}

impl From<FromBytesError> for AddressError {
    fn from(value: FromBytesError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}
