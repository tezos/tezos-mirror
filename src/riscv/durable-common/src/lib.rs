// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Shared types and logic for the RISC-V durable storage OCaml API.

pub mod api_common;
pub mod registry;

use bytes::Bytes;
use octez_riscv_api_common::OcamlFallible;
use octez_riscv_durable_storage::errors as ds_errors;
use octez_riscv_durable_storage::key::Key;

/// KeyParam receiving a `bytes` value from OCaml.
#[derive(ocaml::FromValue)]
pub struct KeyParam<'a>(pub &'a [u8]);

impl<'a> TryFrom<KeyParam<'a>> for Key {
    type Error = ds_errors::InvalidArgumentError;

    fn try_from(value: KeyParam<'a>) -> Result<Self, Self::Error> {
        Key::new(value.0)
    }
}

/// BytesParam receiving a `bytes` value from OCaml.
#[derive(ocaml::FromValue)]
pub struct BytesParam<'a>(pub &'a [u8]);

impl<'a> From<BytesParam<'a>> for Bytes {
    fn from(value: BytesParam<'a>) -> Self {
        Bytes::copy_from_slice(value.0)
    }
}

/// Result type for durable storage operations (no default error type).
pub type SplitDsResult<T, Err> = OcamlFallible<Result<T, Err>>;

/// Split handling of durable storage errors.
///
/// - Operational errors are converted to OCaml exceptions.
/// - InvalidArgument errors are returned as the error variant of an OCaml result.
pub fn split_ds_errors<T, E: From<ds_errors::InvalidArgumentError>>(
    res: Result<T, ds_errors::Error>,
) -> OcamlFallible<Result<T, E>> {
    let inner_res = match res {
        Ok(inner) => Ok(inner),
        Err(ds_errors::Error::InvalidArgument(err)) => Err(err.into()),
        Err(ds_errors::Error::Operational(err)) => return Err(err.into()),
    };

    Ok(inner_res)
}

/// Map a fallible operation over inner value. Errors in mapping are converted to OCaml exceptions.
pub fn map_fallible<T, U, Err, E: 'static + std::error::Error>(
    res: Result<T, Err>,
    map: impl Fn(T) -> Result<U, E>,
) -> OcamlFallible<Result<U, Err>> {
    let inner_res = match res {
        Ok(inner) => Ok(map(inner)?),
        Err(err) => Err(err),
    };

    Ok(inner_res)
}
