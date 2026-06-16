// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Hash of Layer1 contract ids.

use std::fmt::Display;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

use crate::base58::{FromBase58Check, FromBase58CheckError};
use crate::hash::{
    ContractTz1Hash, ContractTz2Hash, ContractTz3Hash, ContractTz4Hash, HashTrait, HashType,
};

/// Hash of Layer1 contract ids.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, HasEncoding, BinWriter, NomReader, Hash)]
pub enum PublicKeyHash {
    /// Tz1-contract
    Ed25519(ContractTz1Hash),
    /// Tz2-contract
    Secp256k1(ContractTz2Hash),
    /// Tz3-contract
    P256(ContractTz3Hash),
    /// Tz4-contract
    Bls(ContractTz4Hash),
}

impl Display for PublicKeyHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ed25519(tz1) => write!(f, "{tz1}"),
            Self::Secp256k1(tz2) => write!(f, "{tz2}"),
            Self::P256(tz3) => write!(f, "{tz3}"),
            Self::Bls(tz4) => write!(f, "{tz4}"),
        }
    }
}

impl PublicKeyHash {
    /// Size of the underlying byte array.
    pub const SIZE: usize = ContractTz1Hash::SIZE;
    /// Conversion from base58-encoding string (with prefix).
    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = data.from_base58check()?;
        match bytes {
            _ if bytes.starts_with(HashType::ContractTz1Hash.base58check_prefix()) => Ok(
                PublicKeyHash::Ed25519(ContractTz1Hash::from_b58check(data)?),
            ),
            _ if bytes.starts_with(HashType::ContractTz2Hash.base58check_prefix()) => Ok(
                PublicKeyHash::Secp256k1(ContractTz2Hash::from_b58check(data)?),
            ),
            _ if bytes.starts_with(HashType::ContractTz3Hash.base58check_prefix()) => {
                Ok(PublicKeyHash::P256(ContractTz3Hash::from_b58check(data)?))
            }
            _ if bytes.starts_with(HashType::ContractTz4Hash.base58check_prefix()) => {
                Ok(PublicKeyHash::Bls(ContractTz4Hash::from_b58check(data)?))
            }
            _ => Err(FromBase58CheckError::InvalidBase58),
        }
    }

    /// Conversion to base58-encoding string (with prefix).
    pub fn to_b58check(&self) -> String {
        match self {
            Self::Ed25519(tz1) => tz1.to_b58check(),
            Self::Secp256k1(tz2) => tz2.to_b58check(),
            Self::P256(tz3) => tz3.to_b58check(),
            Self::Bls(tz4) => tz4.to_b58check(),
        }
    }
}

impl From<PublicKeyHash> for [u8; PublicKeyHash::SIZE] {
    fn from(pkh: PublicKeyHash) -> Self {
        match pkh {
            PublicKeyHash::Ed25519(tz1) => tz1.into(),
            PublicKeyHash::Secp256k1(tz2) => tz2.into(),
            PublicKeyHash::P256(tz3) => tz3.into(),
            PublicKeyHash::Bls(tz4) => tz4.into(),
        }
    }
}

impl From<PublicKeyHash> for Vec<u8> {
    fn from(pkh: PublicKeyHash) -> Self {
        match pkh {
            PublicKeyHash::Ed25519(tz1) => tz1.into(),
            PublicKeyHash::Secp256k1(tz2) => tz2.into(),
            PublicKeyHash::P256(tz3) => tz3.into(),
            PublicKeyHash::Bls(tz4) => tz4.into(),
        }
    }
}

impl TryFrom<&str> for PublicKeyHash {
    type Error = FromBase58CheckError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_b58check(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn tz1_b58check() {
        let tz1 = "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w";

        let pkh = PublicKeyHash::from_b58check(tz1);

        assert!(matches!(pkh, Ok(PublicKeyHash::Ed25519(_))));

        let tz1_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz1, &tz1_from_pkh);
    }

    #[test]
    fn tz2_b58check() {
        let tz2 = "tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc";

        let pkh = PublicKeyHash::from_b58check(tz2);

        assert!(matches!(pkh, Ok(PublicKeyHash::Secp256k1(_))));

        let tz2_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz2, &tz2_from_pkh);
    }

    #[test]
    fn tz3_b58check() {
        let tz3 = "tz3WEJYwJ6pPwVbSL8FrSoAXRmFHHZTuEnMA";

        let pkh = PublicKeyHash::from_b58check(tz3);

        assert!(matches!(pkh, Ok(PublicKeyHash::P256(_))));

        let tz3_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz3, &tz3_from_pkh);
    }

    #[test]
    fn tz4_b58check() {
        let tz4 = "tz4DWZXsrP3bdPaZ5B3M3iLVoRMAyxw9oKLH";

        let pkh = PublicKeyHash::from_b58check(tz4);

        assert!(matches!(pkh, Ok(PublicKeyHash::Bls(_))));

        let tz4_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz4, &tz4_from_pkh);
    }

    #[test]
    fn tz1_encoding() {
        let tz1 = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";

        let pkh = PublicKeyHash::from_b58check(tz1).expect("expected valid tz1 hash");

        let mut bin = Vec::new();
        pkh.bin_write(&mut bin).expect("serialization should work");

        let deserde_pkh = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(0_u8, bin[0]);
        assert_eq!(pkh, deserde_pkh);
    }

    #[test]
    fn tz2_encoding() {
        let tz2 = "tz2KZPgf2rshxNUBXFcTaCemik1LH1v9qz3F";

        let pkh = PublicKeyHash::from_b58check(tz2).expect("expected valid tz2 hash");

        let mut bin = Vec::new();
        pkh.bin_write(&mut bin).expect("serialization should work");

        let deserde_pkh = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(1_u8, bin[0]);
        assert_eq!(pkh, deserde_pkh);
    }

    #[test]
    fn tz3_encoding() {
        let tz3 = "tz3fTJbAxj1LQCEKDKmYLWKP6e5vNC9vwvyo";

        let pkh = PublicKeyHash::from_b58check(tz3).expect("expected valid tz3 hash");

        let mut bin = Vec::new();
        pkh.bin_write(&mut bin).expect("serialization should work");

        let deserde_pkh = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(2_u8, bin[0]);
        assert_eq!(pkh, deserde_pkh);
    }

    #[test]
    fn tz4_encoding() {
        let tz4 = "tz4DWZXsrP3bdPaZ5B3M3iLVoRMAyxw9oKLH";

        let pkh = PublicKeyHash::from_b58check(tz4).expect("expected valid tz4 hash");

        let mut bin = Vec::new();
        pkh.bin_write(&mut bin).expect("serialization should work");

        let deserde_pkh = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(3_u8, bin[0]);
        assert_eq!(pkh, deserde_pkh);
    }
}
