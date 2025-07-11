// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use tezos_protocol::entrypoint;

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum EntrypointError {
    #[error("Entrypoint format failure: {0:?}")]
    Format(#[from] entrypoint::ByteReprError),
}

#[derive(uniffi::Object, Debug, Clone, PartialEq, Eq)]
#[uniffi::export(Debug, Display, Eq)]
pub struct Entrypoint(pub(crate) entrypoint::Entrypoint);

#[uniffi::export]
impl Entrypoint {
    #[uniffi::constructor]
    pub fn new(name: &str) -> Result<Self, Error> {
        name.try_into()
            .map(Self)
            .map_err(|e| Error::Entrypoint(EntrypointError::Format(e)))
    }
}

impl ::std::fmt::Display for Entrypoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
