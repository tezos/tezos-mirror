/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use crate::ast::michelson_key_hash::KeyHash;

use super::{ByteReprError, ByteReprTrait};

use tezos_crypto_rs::hash::{
    ContractKt1Hash, ContractTz1Hash, ContractTz2Hash, ContractTz3Hash, ContractTz4Hash, Hash,
    HashTrait, SmartRollupHash,
};

macro_rules! address_hash_type_and_impls {
    ($($con:ident($ty:ident)),* $(,)*) => {
        #[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq, Hash)]
        pub enum AddressHash {
            $($con($ty)),*
        }

        $(impl From<$ty> for AddressHash {
            fn from(value: $ty) -> Self {
                AddressHash::$con(value)
            }
        })*

        impl AsRef<[u8]> for AddressHash {
            fn as_ref(&self) -> &[u8] {
                match self {
                    $(AddressHash::$con(h) => h.as_ref()),*
                }
            }
        }

        impl From<AddressHash> for Vec<u8> {
            fn from(value: AddressHash) -> Self {
                match value {
                    $(AddressHash::$con(h) => h.into(),)*
                }
            }
        }
    };
}

address_hash_type_and_impls! {
    Implicit(KeyHash),
    Kt1(ContractKt1Hash),
    Sr1(SmartRollupHash),
}

impl From<ContractTz1Hash> for AddressHash {
    fn from(x: ContractTz1Hash) -> Self {
        Self::Implicit(x.into())
    }
}

impl From<ContractTz2Hash> for AddressHash {
    fn from(x: ContractTz2Hash) -> Self {
        Self::Implicit(x.into())
    }
}

impl From<ContractTz3Hash> for AddressHash {
    fn from(x: ContractTz3Hash) -> Self {
        Self::Implicit(x.into())
    }
}

impl From<ContractTz4Hash> for AddressHash {
    fn from(x: ContractTz4Hash) -> Self {
        Self::Implicit(x.into())
    }
}

impl TryFrom<&[u8]> for AddressHash {
    type Error = ByteReprError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(value)
    }
}

impl TryFrom<&str> for AddressHash {
    type Error = ByteReprError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_base58_check(value)
    }
}

pub(super) fn check_size(data: &[u8], min_size: usize, name: &str) -> Result<(), ByteReprError> {
    let size = data.len();
    if size < min_size {
        Err(ByteReprError::WrongFormat(format!(
            "address must be at least {min_size} {name} long, but it is {size} {name} long"
        )))
    } else {
        Ok(())
    }
}

const TAG_IMPLICIT: u8 = 0;
const TAG_KT1: u8 = 1;
const TAG_SR1: u8 = 3;
const PADDING_ORIGINATED: &[u8] = &[0];

impl AddressHash {
    // all address hashes are 20 bytes in length
    pub const HASH_SIZE: usize = 20;
    // +2 for tags: implicit addresses use 2-byte, and KT1/sr1 add zero-byte
    // padding to the end
    pub const BYTE_SIZE: usize = Self::HASH_SIZE + 2;
    pub const BASE58_SIZE: usize = 36;
}

impl ByteReprTrait for AddressHash {
    fn from_base58_check(data: &str) -> Result<Self, ByteReprError> {
        use AddressHash::*;

        check_size(data.as_bytes(), Self::BASE58_SIZE, "characters")?;

        Ok(match &data[0..3] {
            "KT1" => Kt1(HashTrait::from_b58check(data)?),
            "sr1" => Sr1(HashTrait::from_b58check(data)?),
            // tz1..tz4 are delegated to KeyHash
            _ => Implicit(KeyHash::from_base58_check(data)?),
        })
    }

    fn to_base58_check(&self) -> String {
        use AddressHash::*;
        match self {
            Kt1(hash) => hash.to_base58_check(),
            Sr1(hash) => hash.to_base58_check(),
            Implicit(hash) => hash.to_base58_check(),
        }
    }

    fn from_bytes(bytes: &[u8]) -> Result<Self, ByteReprError> {
        use AddressHash::*;

        check_size(bytes, Self::BYTE_SIZE, "bytes")?;
        let validate_padding_byte = || match bytes.last().unwrap() {
            0 => Ok(()),
            b => Err(ByteReprError::WrongFormat(format!(
                "address must be padded with byte 0x00, but it was padded with 0x{}",
                hex::encode([*b])
            ))),
        };
        Ok(match bytes[0] {
            // implicit addresses
            TAG_IMPLICIT => Implicit(KeyHash::from_bytes(&bytes[1..])?),
            TAG_KT1 => {
                validate_padding_byte()?;
                Kt1(HashTrait::try_from_bytes(&bytes[1..bytes.len() - 1])?)
            }
            // 2 is txr1 addresses, which are deprecated
            TAG_SR1 => {
                validate_padding_byte()?;
                Sr1(HashTrait::try_from_bytes(&bytes[1..bytes.len() - 1])?)
            }
            _ => {
                return Err(ByteReprError::UnknownPrefix(format!(
                    "0x{}",
                    hex::encode(&bytes[..1])
                )))
            }
        })
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        use AddressHash::*;
        fn originated_account(out: &mut Vec<u8>, tag: u8, hash: impl AsRef<Hash>) {
            out.push(tag);
            out.extend_from_slice(hash.as_ref());
            out.extend_from_slice(PADDING_ORIGINATED);
        }
        match self {
            Implicit(hash) => {
                out.push(TAG_IMPLICIT);
                hash.to_bytes(out);
            }
            Kt1(hash) => originated_account(out, TAG_KT1, hash),
            Sr1(hash) => originated_account(out, TAG_SR1, hash),
        }
    }
}
