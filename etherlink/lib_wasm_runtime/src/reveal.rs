// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

use std::{
    fmt::{Display, Pointer},
    fs,
    path::PathBuf,
};

use log::error;
use tezos_crypto_rs::blake2b;
use wasmer::RuntimeError;

use crate::bindings::{self, BindingsError};

pub enum RevealPreimageError {
    InvalidHash,
    IORead(std::io::Error),
    IOWrite(std::io::Error),
    OCaml(ocaml::Error),
    NotFound,
    InvalidPreimage,
}

impl Display for RevealPreimageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RevealPreimageError::InvalidHash => write!(
                f,
                "Invalid or unsupported hash (only blake2B hashes are supported)"
            ),
            RevealPreimageError::IORead(io_err) => {
                write!(
                    f,
                    "Cannot read the preimage from the preimages directory: {}",
                    io_err
                )
            }
            RevealPreimageError::IOWrite(io_err) => {
                write!(f, "Cannot write the preimage downloaded from the endpoint to the preimages directory: {}", io_err)
            }
            RevealPreimageError::OCaml(err) => write!(
                f,
                "OCaml runtime error when trying to download the preimage: {:?}",
                err
            ),
            RevealPreimageError::NotFound => {
                write!(f, "Could not find the preimage")
            }
            RevealPreimageError::InvalidPreimage => {
                write!(f, "Preimage is corrupted")
            }
        }
    }
}

pub fn preimage(
    hash_bytes: &[u8],
    preimages_dir: &str,
    preimages_endpoint: Option<&str>,
) -> Result<Vec<u8>, RevealPreimageError> {
    let hash_hex = hex::encode(&hash_bytes);
    if hash_bytes.len() != 32 && hash_bytes[0] != 0u8 {
        return Err(RevealPreimageError::InvalidHash);
    }

    let mut path = PathBuf::from(preimages_dir);
    path.push(&hash_hex);

    let res = if path.exists() {
        fs::read(path).map_err(|io_err| RevealPreimageError::IORead(io_err))?
    } else {
        if let Some(preimages_endpoint) = preimages_endpoint {
            match bindings::fetch_preimage_from_remote(&preimages_endpoint, &hash_hex) {
                Ok(buffer) => {
                    let buffer = buffer.as_bytes();
                    fs::write(path, &buffer)
                        .map_err(|io_err| RevealPreimageError::IOWrite(io_err))?;
                    Ok(buffer.to_vec())
                }
                Err(BindingsError::HostFuncError(_)) => {
                    unreachable!("`fetch_preimage_from_remote` bindings is not a host function")
                }
                Err(BindingsError::OCamlError(err)) => Err(RevealPreimageError::OCaml(err)),
            }?
        } else {
            error!(
                "Preimage {} was not found in {} and preimages endpoint is not provided",
                hash_hex, preimages_dir
            );
            return Err(RevealPreimageError::NotFound);
        }
    };

    if !blake2b::digest_256(&res).eq(&hash_bytes[1..]) {
        return Err(RevealPreimageError::InvalidPreimage);
    }

    return Ok(res);
}
