// SPDX-FileCopyrightText: 2023 PK Lab <contact@pklab.io>
//
// SPDX-License-Identifier: MIT

//! FA bridge specific errors.

use tezos_data_encoding::enc::BinError;

#[derive(Debug, thiserror::Error)]
pub enum FaBridgeError {
    #[error("Binary codec error: {0}")]
    BinaryCodec(#[from] BinError),

    #[error("Primitive type error: {0:?}")]
    PrimitiveType(primitive_types::Error),

    #[error("Invalid routing info: {0}")]
    InvalidRoutingInfo(&'static str),

    #[error("Ticket parsing error: {0}")]
    TicketConstructError(&'static str),

    #[error("Entrypoint pasing error")]
    EntrypointParseError,

    #[error("Abi decode error: {0}")]
    AbiDecodeError(&'static str),
}

impl From<primitive_types::Error> for FaBridgeError {
    fn from(value: primitive_types::Error) -> Self {
        Self::PrimitiveType(value)
    }
}
