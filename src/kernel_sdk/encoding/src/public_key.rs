// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Public Key of Layer1.

use std::fmt::Display;
use tezos_crypto_rs::hash::{PublicKeyEd25519, PublicKeyP256, PublicKeySecp256k1};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

use crypto::base58::{FromBase58Check, FromBase58CheckError};
use crypto::hash::{Hash, HashTrait, HashType};

/// Public Key of Layer1.
#[derive(Debug, Clone, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub enum PublicKey {
    /// Tz1 - public key
    Ed25519(PublicKeyEd25519),
    /// Tz2 - public key
    Secp256k1(PublicKeySecp256k1),
    /// Tz3 - public key
    P256(PublicKeyP256),
}

impl Display for PublicKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ed25519(tz1) => write!(f, "{}", tz1),
            Self::Secp256k1(tz2) => write!(f, "{}", tz2),
            Self::P256(tz3) => write!(f, "{}", tz3),
        }
    }
}

impl PublicKey {
    /// Conversion from base58-encoding string (with prefix).
    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = data.from_base58check()?;
        let public_key = if bytes
            .starts_with(HashType::PublicKeyEd25519.base58check_prefix())
        {
            PublicKey::Ed25519(PublicKeyEd25519::from_b58check(data)?)
        } else if bytes.starts_with(HashType::PublicKeySecp256k1.base58check_prefix()) {
            PublicKey::Secp256k1(PublicKeySecp256k1::from_b58check(data)?)
        } else if bytes.starts_with(HashType::PublicKeyP256.base58check_prefix()) {
            PublicKey::P256(PublicKeyP256::from_b58check(data)?)
        } else {
            return Err(FromBase58CheckError::InvalidBase58);
        };
        Ok(public_key)
    }

    /// Conversion to base58-encoding string (with prefix).
    pub fn to_b58check(&self) -> String {
        match self {
            Self::Ed25519(tz1) => tz1.to_b58check(),
            Self::Secp256k1(tz2) => tz2.to_b58check(),
            Self::P256(tz3) => tz3.to_b58check(),
        }
    }
}

impl From<PublicKey> for Hash {
    fn from(pkh: PublicKey) -> Self {
        match pkh {
            PublicKey::Ed25519(tz1) => tz1.into(),
            PublicKey::Secp256k1(tz2) => tz2.into(),
            PublicKey::P256(tz3) => tz3.into(),
        }
    }
}

impl TryFrom<&str> for PublicKey {
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
        let tz1 = "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK";

        let pkh = PublicKey::from_b58check(tz1);

        assert!(matches!(pkh, Ok(PublicKey::Ed25519(_))));

        let tz1_from_pkh = pkh.unwrap().to_b58check();

        assert_eq!(tz1, &tz1_from_pkh);
    }

    #[test]
    fn tz2_b58check() {
        let tz2 = "sppk7Zik17H7AxECMggqD1FyXUQdrGRFtz9X7aR8W2BhaJoWwSnPEGA";

        let public_key = PublicKey::from_b58check(tz2);

        assert!(matches!(public_key, Ok(PublicKey::Secp256k1(_))));

        let tz2_from_pk = public_key.unwrap().to_b58check();

        assert_eq!(tz2, &tz2_from_pk);
    }

    #[test]
    fn tz3_b58check() {
        let tz3 = "p2pk67VpBjWwoPULwXCpayec6rFxaAKv8VjJ8cVMHmLDCYARu31zx5Z";

        let public_key = PublicKey::from_b58check(tz3);

        assert!(matches!(public_key, Ok(PublicKey::P256(_))));

        let tz3_from_pk = public_key.unwrap().to_b58check();

        assert_eq!(tz3, &tz3_from_pk);
    }

    #[test]
    fn tz1_encoding() {
        let tz1 = "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK";

        let public_key = PublicKey::from_b58check(tz1).expect("expected valid tz1 hash");

        let mut bin = Vec::new();
        public_key
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_pk = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(0_u8, bin[0]);
        assert_eq!(public_key, deserde_pk);
    }

    #[test]
    fn tz2_encoding() {
        let tz2 = "sppk7Zik17H7AxECMggqD1FyXUQdrGRFtz9X7aR8W2BhaJoWwSnPEGA";

        let public_key = PublicKey::from_b58check(tz2).expect("expected valid tz2 hash");

        let mut bin = Vec::new();
        public_key
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_pk = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(1_u8, bin[0]);
        assert_eq!(public_key, deserde_pk);
    }

    #[test]
    fn tz3_encoding() {
        let tz3 = "p2pk67VpBjWwoPULwXCpayec6rFxaAKv8VjJ8cVMHmLDCYARu31zx5Z";

        let public_key = PublicKey::from_b58check(tz3).expect("expected valid tz3 hash");

        let mut bin = Vec::new();
        public_key
            .bin_write(&mut bin)
            .expect("serialization should work");

        let deserde_pk = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(2_u8, bin[0]);
        assert_eq!(public_key, deserde_pk);
    }
}
