// SPDX-FileCopyrightText: 2023-2024 Trilitech <contact@trili.tech>
// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
//
// Ported from octez: lib_crypto/signature_v1.ml
//
// Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>
// Copyright (c) 2020 Metastate AG <hello@metastate.dev>
// Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::base58::FromBase58CheckError;
use crate::hash::{
    BlsSignature, Ed25519Signature, FromBytesError, HashTrait, HashType, P256Signature,
    Secp256k1Signature, UnknownSignature,
};
use nom::Err;
use serde::{Deserialize, Serialize};
use tezos_data_encoding::enc::{BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::nom::error::BoundedEncodingKind;
use tezos_data_encoding::nom::error::DecodeError;
use tezos_data_encoding::nom::{NomReader, NomResult};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Signature {
    Ed25519(Ed25519Signature),
    Secp256k1(Secp256k1Signature),
    P256(P256Signature),
    Bls(BlsSignature),
    Unknown(UnknownSignature),
}

impl Signature {
    pub fn from_base58_check(data: &str) -> Result<Self, FromBase58CheckError> {
        if data.starts_with("edsig") {
            Ok(Signature::Ed25519(Ed25519Signature::from_b58check(data)?))
        } else if data.starts_with("spsig1") {
            Ok(Signature::Secp256k1(Secp256k1Signature::from_b58check(
                data,
            )?))
        } else if data.starts_with("p2sig") {
            Ok(Signature::P256(P256Signature::from_b58check(data)?))
        } else if data.starts_with("BLsig") {
            Ok(Signature::Bls(BlsSignature::from_b58check(data)?))
        } else {
            Ok(Signature::Unknown(UnknownSignature::from_b58check(data)?))
        }
    }

    pub fn to_base58_check(&self) -> String {
        match self {
            Self::Ed25519(s) => s.to_b58check(),
            Self::Secp256k1(s) => s.to_b58check(),
            Self::P256(s) => s.to_b58check(),
            Self::Bls(s) => s.to_b58check(),
            Self::Unknown(s) => s.to_b58check(),
        }
    }

    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        Self::from_base58_check(data)
    }

    pub fn to_b58check(&self) -> String {
        self.to_base58_check()
    }

    pub fn hash_type(&self) -> HashType {
        match self {
            Self::Ed25519(_) => HashType::Ed25519Signature,
            Self::Secp256k1(_) => HashType::Secp256k1Signature,
            Self::P256(_) => HashType::P256Signature,
            Self::Bls(_) => HashType::BlsSignature,
            Self::Unknown(_) => HashType::UnknownSignature,
        }
    }
}

impl AsRef<[u8]> for Signature {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::Ed25519(s) => s.as_ref(),
            Self::Secp256k1(s) => s.as_ref(),
            Self::P256(s) => s.as_ref(),
            Self::Bls(s) => s.as_ref(),
            Self::Unknown(s) => s.as_ref(),
        }
    }
}

impl From<Signature> for Vec<u8> {
    fn from(s: Signature) -> Self {
        match s {
            Signature::Ed25519(s) => s.into(),
            Signature::Secp256k1(s) => s.into(),
            Signature::P256(s) => s.into(),
            Signature::Bls(s) => s.into(),
            Signature::Unknown(s) => s.into(),
        }
    }
}

impl TryFrom<Vec<u8>> for Signature {
    type Error = FromBytesError;

    fn try_from(hash: Vec<u8>) -> Result<Self, Self::Error> {
        if hash.len() == BlsSignature::hash_size() {
            Ok(Signature::Bls(BlsSignature::try_from(hash)?))
        } else {
            Ok(Signature::Unknown(UnknownSignature::try_from(hash)?))
        }
    }
}

impl TryFrom<&[u8]> for Signature {
    type Error = FromBytesError;

    fn try_from(hash: &[u8]) -> Result<Self, Self::Error> {
        if let Ok(s) = BlsSignature::try_from(hash) {
            Ok(Signature::Bls(s))
        } else {
            Ok(Signature::Unknown(UnknownSignature::try_from(hash)?))
        }
    }
}

impl TryFrom<&str> for Signature {
    type Error = FromBase58CheckError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        Signature::from_base58_check(s)
    }
}

impl ::core::str::FromStr for Signature {
    type Err = FromBase58CheckError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Signature::from_base58_check(s)
    }
}

impl ::std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO - this could be done without the need to perform a heap allocation.
        write!(f, "{}", self.to_base58_check())
    }
}

macro_rules! from_s_for_sig {
    ($sig:ident, $name:ident) => {
        impl From<$sig> for Signature {
            fn from(s: $sig) -> Self {
                Self::$name(s)
            }
        }
    };
}
from_s_for_sig!(Ed25519Signature, Ed25519);
from_s_for_sig!(Secp256k1Signature, Secp256k1);
from_s_for_sig!(P256Signature, P256);
from_s_for_sig!(BlsSignature, Bls);
from_s_for_sig!(UnknownSignature, Unknown);

#[derive(Debug, Error, PartialEq, Eq, Clone)]
pub enum TryFromSignatureError {
    #[error("Incorrect signature kind {0:?}.")]
    InvalidKind(HashType),
}

macro_rules! from_sig_for_s {
    ($sig:ident, $name:ident) => {
        impl TryFrom<Signature> for $sig {
            type Error = TryFromSignatureError;

            fn try_from(s: Signature) -> Result<Self, Self::Error> {
                match s {
                    Signature::$name(s) => Ok(s),
                    Signature::Unknown(s) => Ok(s.into()),
                    s => Err(Self::Error::InvalidKind(s.hash_type())),
                }
            }
        }
    };
}
from_sig_for_s!(Ed25519Signature, Ed25519);
from_sig_for_s!(Secp256k1Signature, Secp256k1);
from_sig_for_s!(P256Signature, P256);

impl TryFrom<Signature> for BlsSignature {
    type Error = TryFromSignatureError;

    fn try_from(s: Signature) -> Result<Self, Self::Error> {
        match s {
            Signature::Bls(s) => Ok(s),
            s => Err(Self::Error::InvalidKind(s.hash_type())),
        }
    }
}

impl TryFrom<Signature> for UnknownSignature {
    type Error = TryFromSignatureError;

    fn try_from(s: Signature) -> Result<Self, Self::Error> {
        match s {
            Signature::Ed25519(s) => Ok(s.into()),
            Signature::Secp256k1(s) => Ok(s.into()),
            Signature::P256(s) => Ok(s.into()),
            Signature::Unknown(s) => Ok(s),
            s => Err(Self::Error::InvalidKind(s.hash_type())),
        }
    }
}

impl BinWriter for Signature {
    fn bin_write(&self, out: &mut Vec<u8>) -> BinResult {
        use tezos_data_encoding::enc::*;

        dynamic(bytes)(self, out)
    }
}
impl HasEncoding for Signature {
    fn encoding() -> Encoding {
        Encoding::Custom
    }
}

impl<'a> NomReader<'a> for Signature {
    fn nom_read(input: &'a [u8]) -> NomResult<'a, Self> {
        use tezos_data_encoding::nom::*;

        let (rest, v) = dynamic(bytes)(input)?;
        if let Ok(v) = Self::try_from(v) {
            Ok((rest, v))
        } else {
            Err(Err::Error(DecodeError::limit(
                input,
                BoundedEncodingKind::Signature,
            )))
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test() {
        assert_eq!(
            &super::Signature::try_from([0; 64].to_vec()).unwrap().to_base58_check(),
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq"
        );
    }
}
