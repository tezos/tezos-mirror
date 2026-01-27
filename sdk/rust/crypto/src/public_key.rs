// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Public Key of Layer1.

use crate::base58::{FromBase58Check, FromBase58CheckError};
use crate::hash::{Hash, HashTrait, HashType};
use crate::hash::{PublicKeyBls, PublicKeyEd25519, PublicKeyP256, PublicKeySecp256k1};
use crate::signature::Signature;
use crate::{CryptoError, PublicKeySignatureVerifier};
use std::fmt::Display;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;

/// Public Key of Layer1.
#[derive(PartialOrd, Debug, Clone, PartialEq, Eq, HasEncoding, BinWriter, NomReader)]
pub enum PublicKey {
    /// Tz1 - public key
    Ed25519(PublicKeyEd25519),
    /// Tz2 - public key
    Secp256k1(PublicKeySecp256k1),
    /// Tz3 - public key
    P256(PublicKeyP256),
    /// Tz4 - public key
    Bls(PublicKeyBls),
}

impl Display for PublicKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ed25519(tz1) => write!(f, "{tz1}"),
            Self::Secp256k1(tz2) => write!(f, "{tz2}"),
            Self::P256(tz3) => write!(f, "{tz3}"),
            Self::Bls(tz4) => write!(f, "{tz4}"),
        }
    }
}

impl PublicKey {
    /// Conversion from base58-encoding string (with prefix).
    pub fn from_b58check(data: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = data.from_base58check()?;
        let public_key = if bytes.starts_with(HashType::PublicKeyEd25519.base58check_prefix()) {
            PublicKey::Ed25519(PublicKeyEd25519::from_b58check(data)?)
        } else if bytes.starts_with(HashType::PublicKeySecp256k1.base58check_prefix()) {
            PublicKey::Secp256k1(PublicKeySecp256k1::from_b58check(data)?)
        } else if bytes.starts_with(HashType::PublicKeyP256.base58check_prefix()) {
            PublicKey::P256(PublicKeyP256::from_b58check(data)?)
        } else if bytes.starts_with(HashType::PublicKeyBls.base58check_prefix()) {
            PublicKey::Bls(PublicKeyBls::from_b58check(data)?)
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
            Self::Bls(tz4) => tz4.to_b58check(),
        }
    }
}

impl From<PublicKey> for Hash {
    fn from(pkh: PublicKey) -> Self {
        match pkh {
            PublicKey::Ed25519(tz1) => tz1.into(),
            PublicKey::Secp256k1(tz2) => tz2.into(),
            PublicKey::P256(tz3) => tz3.into(),
            PublicKey::Bls(tz4) => tz4.into(),
        }
    }
}

impl TryFrom<&str> for PublicKey {
    type Error = FromBase58CheckError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_b58check(value)
    }
}

impl PublicKeySignatureVerifier for PublicKey {
    type Signature = Signature;
    type Error = CryptoError;

    // TODO: This can be made more effecient by avoiding a clone of the internal buffer.
    //
    fn verify_signature(
        &self,
        signature: &Self::Signature,
        msg: &[u8],
    ) -> Result<bool, Self::Error> {
        let signature = signature.clone();
        match self {
            PublicKey::Ed25519(ed25519) => ed25519.verify_signature(&signature.try_into()?, msg),
            PublicKey::Secp256k1(secp256k1) => {
                secp256k1.verify_signature(&signature.try_into()?, msg)
            }
            PublicKey::P256(p256) => p256.verify_signature(&signature.try_into()?, msg),
            #[cfg(feature = "bls")]
            PublicKey::Bls(bls) => bls.verify_signature(&signature.try_into()?, msg),
            #[cfg(not(feature = "bls"))]
            PublicKey::Bls(_) => Err(CryptoError::Unsupported(
                "bls feature disabled, tz4 signature verification not supported",
            )),
        }
    }
}

impl crate::PublicKeyWithHash for PublicKey {
    type Hash = crate::public_key_hash::PublicKeyHash;

    fn pk_hash(&self) -> Self::Hash {
        match self {
            Self::Ed25519(pk) => Self::Hash::Ed25519(pk.pk_hash()),
            Self::Secp256k1(pk) => Self::Hash::Secp256k1(pk.pk_hash()),
            Self::P256(pk) => Self::Hash::P256(pk.pk_hash()),
            Self::Bls(pk) => Self::Hash::Bls(pk.pk_hash()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::PublicKeyWithHash;

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
    fn tz4_b58check() {
        let tz4 = "BLpk1yE462s3cPX5t2HhvGPg3HSEUgqLi9q9Knwx7mbN4VuhEsvBYFvEz5Eu9shR7vZRY1k5PCtV";

        let public_key = PublicKey::from_b58check(tz4);

        assert!(matches!(public_key, Ok(PublicKey::Bls(_))));

        let tz4_from_pk = public_key.unwrap().to_b58check();

        assert_eq!(tz4, &tz4_from_pk);
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

    #[test]
    fn tz4_encoding() {
        let tz4 = "BLpk1yE462s3cPX5t2HhvGPg3HSEUgqLi9q9Knwx7mbN4VuhEsvBYFvEz5Eu9shR7vZRY1k5PCtV";

        let public_key = PublicKey::from_b58check(tz4).expect("expected valid tz4 hash");
        let expected_bytes = hex::decode("03b46a862ef09f7f994c3d3570464dd8ab39305161e27b144ef900fc57e92c9429878b81af9678478d63f2eb36b01b2418").unwrap();

        let mut bin = Vec::new();
        public_key
            .bin_write(&mut bin)
            .expect("serialization should work");

        assert_eq!(expected_bytes, bin, "Unexpected serialisation");

        let deserde_pk = NomReader::nom_read(bin.as_slice())
            .expect("deserialization should work")
            .1;

        // Check tag encoding
        assert_eq!(3_u8, bin[0]);
        assert_eq!(public_key, deserde_pk);
    }

    #[test]
    fn tz1_signature_signature_verification_succeeds() {
        // sk: edsk3vifWnPCr8jXyhnt1YLa5KeNYTPfHENDq9gxqAA8ERkvEigYMe
        let tz1 =
            PublicKey::from_b58check("edpkurrsBe7UjF59ciHHmBRnS76WHx3YNL9m7owYta6ticPrdP9DG4")
                .expect("public key decoding should work");
        let sig: Signature = Signature::from_base58_check(
            "edsigtoeXp3xFtGugwCTDSDuifQ9Ka81X4gXFoxRQ6Xao2Ryc3yioptrKMfNy5c9pHhbA9Xn3sYZdx2SPiCGTFXjjXx9xKCPDoq"
        ).expect("signature decoding should work");

        let msg = b"hello, world";

        let result = tz1
            .verify_signature(&sig, msg)
            .expect("signature should be correct");
        assert!(result);
    }

    #[test]
    fn tz1_signature_signature_verification_fails() {
        let tz1 =
            PublicKey::from_b58check("edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK")
                .expect("public key decoding should work");
        let sig = Signature::from_base58_check(
            "sigdGBG68q2vskMuac4AzyNb1xCJTfuU8MiMbQtmZLUCYydYrtTd5Lessn1EFLTDJzjXoYxRasZxXbx6tHnirbEJtikcMHt3"
        ).expect("signature decoding should work");
        let msg = hex::decode("bcbb7b77cb0712e4cd02160308cfd53e8dde8a7980c4ff28b62deb12304913c2")
            .expect("payload decoding should work");

        let result = tz1.verify_signature(&sig, &msg);
        assert!(result.is_err());
    }

    #[test]
    fn tz2_signature_signature_verification_succeeds() {
        // sk: spsk1sheno8Jt8FoBEoamFoNBxUEpjEggNNpepTFc8cEoJBA9QjDJq
        let tz2 =
            PublicKey::from_b58check("sppk7a2WEfU54QzcQZ2EMjihtcxLeRtNTVxHw4FW2e8W5kEJ8ZargSb")
                .expect("public key decoding should work");
        // todo use sig not spsig
        let sig = Signature::from_base58_check("siggWynZ1jzFuv67FWSAvhX8948jgL5szpwT2fZAL5brmU9egqoXd3fDXCLQJ2EBcYVLBkev3HvkQ6xnFxSBjthdonajN8JX").expect("signature decoding should work");
        let msg = b"hello, test";

        let result = tz2.verify_signature(&sig, msg).unwrap();
        assert!(result);
    }

    #[test]
    fn tz2_signature_signature_verification_fails() {
        let tz2 = "sppk7Zik17H7AxECMggqD1FyXUQdrGRFtz9X7aR8W2BhaJoWwSnPEGA";
        let tz2 = PublicKey::from_b58check(tz2).expect("parsing should world");
        let sig = Signature::from_base58_check("sigrJ2jqanLupARzKGvzWgL1Lv6NGUqDovHKQg9MX4PtNtHXgcvG6131MRVzujJEXfvgbuRtfdGbXTFaYJJjuUVLNNZTf5q1").expect("signature decoding should work");
        let msg = hex::decode("5538e2cc90c9b053a12e2d2f3a985aff1809eac59501db4d644e4bb381b06b4b")
            .expect("payload decoding should work");

        let result = tz2.verify_signature(&sig, &msg).unwrap();
        assert!(!result);
    }

    #[test]
    fn tz3_signature_signature_verification_succeeds() {
        // sk: p2sk2bixvFTFTuw9HtD4ucuDsktZTcwRJ5V3gDsQauwE2VTuh6hBiP
        let tz3 =
            PublicKey::from_b58check("p2pk65p7HKSGvkMdeK5yckM2nmi59oGNw4ksqdcvwxxF3AV3hopkfGS")
                .expect("decoding public key should work");
        let sig = Signature::from_base58_check(
            "sigfMaQ3pkpywf3q5ZqfNzJuKd6apUa1gRpoGb4hK25dBuiTY5u2vVCJcPGdpUqDT1RwfeGy6gvnHuhbTgfKhn2EZVYMatnN"
        ).expect("signature decoding should work");
        let msg = b"hello, message";
        let result = tz3.verify_signature(&sig, msg).unwrap();
        assert!(result);
    }

    #[test]
    fn tz3_signature_signature_verification_fails() {
        let tz3 =
            PublicKey::from_b58check("p2pk67VpBjWwoPULwXCpayec6rFxaAKv8VjJ8cVMHmLDCYARu31zx5Z")
                .expect("decoding public key should work");
        let sig = Signature::from_base58_check(
            "sigNCaj9CnmD94eZH9C7aPPqBbVCJF72fYmCFAXqEbWfqE633WNFWYQJFnDUFgRUQXR8fQ5tKSfJeTe6UAi75eTzzQf7AEc1"
        ).expect("signature decoding should work");
        let msg = hex::decode("5538e2cc90c9b053a12e2d2f3a985aff1809eac59501db4d644e4bb381b06b4b")
            .expect("payload decoding should work");

        let result = tz3.verify_signature(&sig, &msg).unwrap();
        assert!(!result);
    }

    #[cfg_attr(feature = "bls", test)]
    #[cfg(feature = "bls")]
    fn tz4_signature_signature_verification_succeeds() {
        // sk: BLsk2wHXLW6gN9sbEN2rU84mmCSNZKn9KRKrw74LwHqEaLGwL3qQ31
        let tz4 = PublicKey::from_b58check(
            "BLpk1yE462s3cPX5t2HhvGPg3HSEUgqLi9q9Knwx7mbN4VuhEsvBYFvEz5Eu9shR7vZRY1k5PCtV",
        )
        .expect("decoding public key should work");
        let sig = Signature::from_base58_check(
            "BLsig9WknWnGmPcJw1q9oCBr53UyjAWxxYNS5wz5HBKmCcuxCfK1Hwhs92YDFocvxUhXfUosgcTuzAEuAAjKzjy7isNhU3o2e8snmZyo9E85oRudCpM1MNtkeAAYEkSXUPLKtRYa9yFwni"
        ).expect("signature decoding should work");
        let msg = b"hello, bls";
        let result = tz4.verify_signature(&sig, msg).unwrap();
        assert!(result);
    }

    #[cfg_attr(feature = "bls", test)]
    #[cfg(feature = "bls")]
    fn tz4_signature_signature_verification_fails() {
        // sk: BLsk2wHXLW6gN9sbEN2rU84mmCSNZKn9KRKrw74LwHqEaLGwL3qQ31
        let tz4 = PublicKey::from_b58check(
            "BLpk1yE462s3cPX5t2HhvGPg3HSEUgqLi9q9Knwx7mbN4VuhEsvBYFvEz5Eu9shR7vZRY1k5PCtV",
        )
        .expect("decoding public key should work");
        let sig = Signature::from_base58_check(
            "BLsig9WknWnGmPcJw1q9oCBr53UyjAWxxYNS5wz5HBKmCcuxCfK1Hwhs92YDFocvxUhXfUosgcTuzAEuAAjKzjy7isNhU3o2e8snmZyo9E85oRudCpM1MNtkeAAYEkSXUPLKtRYa9yFwni"
        ).expect("signature decoding should work");
        let msg = b"not the correct message";
        let result = tz4.verify_signature(&sig, msg).unwrap();
        assert!(!result);
    }

    #[test]
    fn pk_hash() {
        let test_hash = |pk, pkh| {
            let pk = PublicKey::from_b58check(pk).unwrap();
            let hash = pk.pk_hash().to_b58check();

            assert_eq!(hash, pkh);
        };

        test_hash(
            "edpkuDMUm7Y53wp4gxeLBXuiAhXZrLn8XB1R83ksvvesH8Lp8bmCfK",
            "tz1QFD9WqLWZmmAuqnnTPPUjfauitYEWdshv",
        );
        test_hash(
            "sppk7a2WEfU54QzcQZ2EMjihtcxLeRtNTVxHw4FW2e8W5kEJ8ZargSb",
            "tz2DzfieD6mjjYYFqbGwotsW5ivfRogQE6c4",
        );
        test_hash(
            "p2pk65p7HKSGvkMdeK5yckM2nmi59oGNw4ksqdcvwxxF3AV3hopkfGS",
            "tz3SYfXSu1mJnKx37BcJvhHyRXM4gbHy8o8i",
        );
        test_hash(
            "BLpk1yE462s3cPX5t2HhvGPg3HSEUgqLi9q9Knwx7mbN4VuhEsvBYFvEz5Eu9shR7vZRY1k5PCtV",
            "tz4DWZXsrP3bdPaZ5B3M3iLVoRMAyxw9oKLH",
        );
    }
}
