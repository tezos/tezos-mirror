/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use tezos_crypto_rs::{
    base58::*,
    blake2b,
    hash::{self, HashTrait},
    hash::{BlsSignature, FromBytesError},
    PublicKeySignatureVerifier,
};

use base58::*;

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum SignatureError {
    #[error("unknown key prefix: {0}")]
    UnknownPrefix(String),
    #[error("wrong key format: {0}")]
    WrongFormat(String),
}

impl From<FromBase58CheckError> for SignatureError {
    fn from(value: FromBase58CheckError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}

impl From<FromBytesError> for SignatureError {
    fn from(value: FromBytesError) -> Self {
        Self::WrongFormat(value.to_string())
    }
}

/* *** Note: reimplementation of signature types. ***

tezos_crypto_rs has some unfortunate quirks with signatures:

- signature of all zeros is always converted to base58 representation for edsig,
  but tests with octez-client show that it should be generic
- spsig1 and p2sig are entirely missing
- `verify_signature` requires generic `Signature` as argument, making it
  very inconvenient to use `tezos_crypto_rs::hash::Ed25519Signature`

These issues should be addressed upstream, but for the time being,
reimplementation.

BLS signatures also have their quirks, but they're not reimplemented, since
they can't be represented by generic signatures. Quirks are worked around as
needed.

-- @lierdakil
*/

pub trait SignatureTrait: Sized + AsRef<[u8]> {
    const BYTE_SIZE: usize;
    const BASE58_PREFIX: &'static [u8];

    fn from_bytes(bs: &[u8]) -> Self;

    fn try_from_bytes(bs: &[u8]) -> Result<Self, FromBytesError> {
        if bs.len() == Self::BYTE_SIZE {
            Ok(Self::from_bytes(bs))
        } else {
            Err(FromBytesError::InvalidSize)
        }
    }

    fn to_base58_check(&self) -> String {
        let data = self.as_ref();
        let mut hash = Vec::with_capacity(Self::BASE58_PREFIX.len() + data.len());
        hash.extend_from_slice(Self::BASE58_PREFIX);
        hash.extend_from_slice(data);
        hash.to_base58check()
            .expect("should always be convertible to base58")
    }

    fn from_b58check(s: &str) -> Result<Self, FromBase58CheckError> {
        let bytes = s.from_base58check()?;
        let expected_len = Self::BASE58_PREFIX.len() + Self::BYTE_SIZE;
        if bytes.len() != expected_len {
            return Err(FromBase58CheckError::MismatchedLength {
                expected: expected_len,
                actual: bytes.len(),
            });
        }
        if let Some(bs) = bytes.strip_prefix(Self::BASE58_PREFIX) {
            Ok(Self::from_bytes(bs))
        } else {
            Err(FromBase58CheckError::InvalidBase58)
        }
    }
}

macro_rules! defsignature {
    ($name:ident, $size:literal, $prefix:expr) => {
        #[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
        pub struct $name(hash::Signature);

        impl SignatureTrait for $name {
            const BYTE_SIZE: usize = $size;
            const BASE58_PREFIX: &'static [u8] = &$prefix; // spsig1(99)
            fn from_bytes(bs: &[u8]) -> Self {
                $name(hash::Signature(bs.to_vec()))
            }
        }

        impl AsRef<[u8]> for $name {
            fn as_ref(&self) -> &[u8] {
                self.0.as_ref()
            }
        }
    };
}

defsignature!(Ed25519Signature, 64, [9, 245, 205, 134, 18]); // edsig(99)
defsignature!(Secp256k1Signature, 64, [13, 115, 101, 19, 63]); // spsig1(99)
defsignature!(P256Signature, 64, [54, 240, 44, 52]); // p2sig(98)
defsignature!(GenericSignature, 64, [4, 130, 43]); // sig(96)

macro_rules! key_type_and_impls {
    ($($con:ident($ty:path)),* $(,)*) => {
        #[derive(Debug, Clone, Eq, PartialOrd, Ord, PartialEq)]
        pub enum Signature {
            $($con($ty)),*
        }

        $(impl From<$ty> for Signature {
            fn from(value: $ty) -> Self {
                Signature::$con(value)
            }
        })*

        impl AsRef<[u8]> for Signature {
            fn as_ref(&self) -> &[u8] {
                match self {
                    $(Signature::$con($ty(h)) => h.as_ref()),*
                }
            }
        }

        impl From<Signature> for Vec<u8> {
            fn from(value: Signature) -> Self {
                match value {
                    $(Signature::$con($ty(h)) => h.into()),*
                }
            }
        }

        impl Signature {
            pub fn to_base58_check(&self) -> String {
                match self {
                    $(Signature::$con(h) => h.to_base58_check()),*
                }
            }
        }
    };
}

key_type_and_impls! {
    Ed25519(Ed25519Signature),
    Secp256k1(Secp256k1Signature),
    P256(P256Signature),
    Bls(BlsSignature),
    Generic(GenericSignature), // See Note: [Generic signatures]
}

/* Note: [Generic signatures]

Existence of "generic signatures" is a quirk of the protocol. Signatures' byte
representations are not tagged, so when parsing a signature from raw bytes, the
only way to determine its type is length, and 3 out of 4 signature variants have
the same byte length. Hence, we need to have a separate variant for yet-unknown
signature variant (which will be determined when/if it's checked from the public
key variant).

Note that we can't represent all signatures as generic, as their base58
representations are different, and it needs to survive the round-trip to match
the protocol. Also note that generic signatures have their own base58
representation.
*/

impl TryFrom<&[u8]> for Signature {
    type Error = SignatureError;
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes(value)
    }
}

impl TryFrom<&str> for Signature {
    type Error = SignatureError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::from_base58_check(value)
    }
}

/// tezos_crypto_rs refuses to accept strings of length >= 128 for some ungodly
/// reason. This is a reimplementation without that check.
fn from_b58check(s: &str) -> Result<Vec<u8>, FromBase58CheckError> {
    const CHECKSUM_BYTE_SIZE: usize = 4;
    let bytes_with_checksum = s
        .from_base58()
        .map_err(|_| FromBase58CheckError::InvalidBase58)?;
    if bytes_with_checksum.len() < CHECKSUM_BYTE_SIZE {
        return Err(FromBase58CheckError::MissingChecksum);
    }
    let (bytes, checksum) =
        bytes_with_checksum.split_at(bytes_with_checksum.len() - CHECKSUM_BYTE_SIZE);

    use cryptoxide::hashing::sha256;

    let checksum_expected = &sha256(&sha256(bytes))[..4];

    if checksum_expected != checksum {
        return Err(FromBase58CheckError::InvalidChecksum);
    }
    Ok(bytes.to_vec())
}

impl Signature {
    /// This is byte-length of `BLsig` variant.
    pub const BLS_BYTE_LENGTH: usize = 96;

    /// This is byte-length of `edsig`, `spsig1`, `p2sig` and `sig` variants.
    pub const GENERIC_BYTE_LENGTH: usize = 64;

    pub fn from_base58_check(data: &str) -> Result<Self, SignatureError> {
        use Signature::*;

        Ok(if data.starts_with("edsig") {
            Ed25519(SignatureTrait::from_b58check(data)?)
        } else if data.starts_with("spsig1") {
            Secp256k1(SignatureTrait::from_b58check(data)?)
        } else if data.starts_with("p2sig") {
            P256(SignatureTrait::from_b58check(data)?)
        } else if data.starts_with("BLsig") {
            // BLS signatures are broken in tezos_crypto_rs
            let raw_bytes = from_b58check(data)?;
            let bytes = &raw_bytes[4..]; // strip 4-byte prefix
            Bls(BlsSignature(bytes.to_vec()))
        } else if data.starts_with("sig") {
            Generic(SignatureTrait::from_b58check(data)?)
        } else {
            return Err(SignatureError::UnknownPrefix(data.to_owned()));
        })
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SignatureError> {
        use Signature::*;

        match bytes.len() {
            Self::GENERIC_BYTE_LENGTH => Ok(Generic(GenericSignature::try_from_bytes(bytes)?)),
            Self::BLS_BYTE_LENGTH => Ok(Bls(BlsSignature::try_from_bytes(bytes)?)),
            len => Err(SignatureError::WrongFormat(format!(
                "signature must be either {} or {} bytes long, but it is {} bytes long",
                Self::GENERIC_BYTE_LENGTH,
                Self::BLS_BYTE_LENGTH,
                len
            ))),
        }
    }

    pub fn to_bytes(&self, out: &mut Vec<u8>) {
        use Signature::*;

        match self {
            Ed25519(hash) => out.extend_from_slice(hash.as_ref()),
            Secp256k1(hash) => out.extend_from_slice(hash.as_ref()),
            P256(hash) => out.extend_from_slice(hash.as_ref()),
            Bls(hash) => out.extend_from_slice(hash.as_ref()),
            Generic(hash) => out.extend_from_slice(hash.as_ref()),
        }
    }

    pub fn to_bytes_vec(&self) -> Vec<u8> {
        let mut out = Vec::new();
        self.to_bytes(&mut out);
        out
    }

    pub fn check(&self, key: &super::michelson_key::Key, msg: &[u8]) -> bool {
        use super::michelson_key::Key;
        use Signature::*;
        fn hash_msg(data: &[u8]) -> Vec<u8> {
            // error is, in fact, impossible
            blake2b::digest_256(data).unwrap()
        }
        match key {
            Key::Ed25519(key) => match self {
                Ed25519(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                Generic(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                P256(..) | Secp256k1(..) | Bls(..) => Ok(false),
            },
            Key::Secp256k1(key) => match self {
                Secp256k1(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                Generic(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                P256(..) | Ed25519(..) | Bls(..) => Ok(false),
            },
            Key::P256(key) => match self {
                P256(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                Generic(sig) => key.verify_signature(&sig.0, &hash_msg(msg)),
                Ed25519(..) | Secp256k1(..) | Bls(..) => Ok(false),
            },
            Key::Bls(key) => match self {
                Bls(sig) => sig.aggregate_verify(&mut [(msg, key)].into_iter()),
                // can't be represented as generic
                P256(..) | Secp256k1(..) | Ed25519(..) | Generic(..) => Ok(false),
            },
        }
        .unwrap_or(false)
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::ast::michelson_key::Key;

    #[test]
    fn test_base58_to_bin() {
        for (in_b58, _, hex) in FIXTURES {
            assert_eq!(
                hex::encode(Signature::from_base58_check(in_b58).unwrap().to_bytes_vec()),
                hex,
            );
        }
    }

    #[test]
    fn test_bin_to_base58() {
        for (_, out_b58, hex) in FIXTURES {
            assert_eq!(
                Signature::from_bytes(&hex::decode(hex).unwrap())
                    .unwrap()
                    .to_base58_check(),
                out_b58,
            );
        }
    }

    // Triples of (readable, output readable, binary) representations of signatures
    // binary representation produced by running
    // `octez-client --mode mockup normalize data <readable> of type key --unparsing-mode Optimized`
    // output readable, respectively, by running
    // `octez-client --mode mockup normalize data <binary> of type key --unparsing-mode Readable`
    const FIXTURES: [(&str, &str, &str); 6] = [
        (
            "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q",
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq",
            "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
        ),
        (
            "edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb",
            "sigh3fUFu26jXP18drVUjgYGj5PgVUUGA8T71bYDuxQE7oaNHm3bQkWTdgFXNfLfTL6ugGnvgxyEYReTFcDBk2Cpe7zpfJBX",
            "91ac1e7fd668854fc7a40feec4034e42c06c068cce10622c607fda232db34c8cf5d8da83098dd891cd4cb4299b3fa0352ae323ad99b24541e54b91888fdc8201",
        ),
        (
            "spsig1Ng2bs4PXCbjaFGuojk9K5Pt3CkfbUZyHLLrBxHSmTqrUUxQggi4yJBit3Ljqnqr61UpdTewTLiu4schSCfZvaRwu412oZ",
            "sigerMGoT93rdbC5H9qbBzfAiW8Ny6XQwGomDXRA88xZv93RJbW7dh2AhsBfmenPmz1HjftVoK2CaAyauavKz51MtqMHNjHr",
            "80e4e72ffecf72953789625b1125e9f45f432c14e53a01ec68a1e1b77d60cfe96a97443733ba0f7f42db3a56d7a433df2b4fc0035c05ab92d062f33c5bab0244",
        ),
        (
            "p2sigRmXDp38VNVaEQH28LYukfLPn8QB5hPEberhvQrrUpRscDZJrrApbRh2u46PTVTwKXjxTLKNN9dyLhPQU6U6jWPGxe4d9v",
            "sigSTJNiwaPuZXmU2FscxNy9scPjjwpbxpPD5rY1QRBbyb4gHXYU7jN9Wcbs9sE4GMzuiSSG5S2egeyJhUjW1uJEgw4AWAXj",
            "22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
        ),
        (
            "sigrZRt6CTBNtRzjMFQYSZhUm1QcDg5gopVgiRTLMQsgikeRLmrmsA5vmFqjrnBhofzqvKtc9k5VhTzCMCio5epRvu9no73S",
            "sigrZRt6CTBNtRzjMFQYSZhUm1QcDg5gopVgiRTLMQsgikeRLmrmsA5vmFqjrnBhofzqvKtc9k5VhTzCMCio5epRvu9no73S",
            "da632d7f267673fab5a40562778a6890b6ada9665d53d7ff318e3399e032b3986588dadcf3bf3b549592f7b8ea1365273fbef5f4883c3430ed32e8ae24017be1",
        ),
        (
            "BLsig9f4r1yc8yPXdPWN46mxCBtw7FLdGZ8LZY7S2KrUVjh44xB4yAmNtwX6h9NQJq8hMHXTpgbJEva1Z15hgL6sQkUjaJqERuVNoAMzx3ZcWHTwsNd3xSwn4crREmHHhpomaouFkvn5Qy",
            "BLsig9f4r1yc8yPXdPWN46mxCBtw7FLdGZ8LZY7S2KrUVjh44xB4yAmNtwX6h9NQJq8hMHXTpgbJEva1Z15hgL6sQkUjaJqERuVNoAMzx3ZcWHTwsNd3xSwn4crREmHHhpomaouFkvn5Qy",
            "83e26eb573d0e54e7249df52e0f2006cb12c677333910559cce99fd75b39cab5daf64e75367b4beabccc1c5a2bf35436076fa3f8455b52a4532d6c96aa94998ba0e0e2221411397feec7be4c8c3b597b3bd7d4b1f6f24eb1c8cb351a5ec7db6b",
        ),
    ];

    #[test]
    fn test_sspk_sig() {
        let key = super::super::michelson_key::Key::from_base58_check(
            "sppk7cdA7Afj8MvuBFrP6KsTLfbM5DtH9GwYaRZwCf5tBVCz6UKGQFR",
        )
        .unwrap();
        let sig = Signature::from_base58_check("spsig1Ng2bs4PXCbjaFGuojk9K5Pt3CkfbUZyHLLrBxHSmTqrUUxQggi4yJBit3Ljqnqr61UpdTewTLiu4schSCfZvaRwu412oZ").unwrap();
        let msg = b"\0";
        assert!(sig.check(&key, msg));
    }

    #[test]
    fn test_signature_verification() {
        for (key, msg, sig, res) in signature_fixtures() {
            assert_eq!(sig.check(&key, msg), res)
        }
    }

    /// tuples of (key, message, signature, is_valid).
    /// signatures are produced via `octez-client sign bytes <msg> for <key>`
    pub fn signature_fixtures() -> Vec<(Key, &'static [u8], Signature, bool)> {
        vec!
          [ ( Key::from_base58_check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"\0"
            , Signature::from_base58_check("BLsigAmLKnuw12tethjMmotFPaQ6u4XCKrVk6c15dkRXKkjDDjHywbhS3nd4rBT31yrCvvQrS2HntWhDRu7sX8Vvek53zBUwQHqfcHRiVKVj1ehq8CBYs1Z7XW2rkL2XkVNHua4cnvxY7F").unwrap()
            , true
            )
          , ( Key::from_base58_check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("BLsigBR1jcWq3w6yAFyDn2X6fxBGjB1E7YoywyUhpvHfisPpCgeMQfJHXoj2YW1BZoujsuZRXdU1BTQjWwqT3xAZRGVcsXovVAgEXbMuuaKLSYYbbMQM92gDDT1UCCRZ1RFvutdavYoumy").unwrap()
            , true
            )
          , ( Key::from_base58_check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"kot"
            , Signature::from_base58_check("BLsigBR1jcWq3w6yAFyDn2X6fxBGjB1E7YoywyUhpvHfisPpCgeMQfJHXoj2YW1BZoujsuZRXdU1BTQjWwqT3xAZRGVcsXovVAgEXbMuuaKLSYYbbMQM92gDDT1UCCRZ1RFvutdavYoumy").unwrap()
            , false
            )
          , ( Key::from_base58_check("edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V").unwrap()
            , b"\0"
            , Signature::from_base58_check("edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb").unwrap()
            , true
            )
          , ( Key::from_base58_check("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("edsigtj8LhbJ2B3qhZvqzA49raG65dydFcWZW9b9L7ntF3bb29zxaBFFL8SM1jeBUY66hG122znyVA4wpzLdwxcNZwSK3Szu7iD").unwrap()
            , true
            )
          , ( Key::from_base58_check("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH").unwrap()
            , b"kot"
            , Signature::from_base58_check("edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb").unwrap()
            , false
            )
          , ( Key::from_base58_check("sppk7cdA7Afj8MvuBFrP6KsTLfbM5DtH9GwYaRZwCf5tBVCz6UKGQFR").unwrap()
            , b"\0"
            , Signature::from_base58_check("spsig1Ng2bs4PXCbjaFGuojk9K5Pt3CkfbUZyHLLrBxHSmTqrUUxQggi4yJBit3Ljqnqr61UpdTewTLiu4schSCfZvaRwu412oZ").unwrap()
            , true
            )
          , ( Key::from_base58_check("sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("spsig1aP7D9oheiraNuM1NgziMPSPKS1F9kSWyFqkE8WigaeU5Uzb3LwY34F7Y7RsF6sY5ZfUda1NWdrC5V4KEfm9jeU1eniHmy").unwrap()
            , true
            )
          , ( Key::from_base58_check("sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD").unwrap()
            , b"kot"
            , Signature::from_base58_check("spsig1PJ9LG9ovbpVJ3CucFWL7iBaQZjqEWMvppgLjYiiSwzcxpuUqHr2BUVZDUwkmZKzMNDWJdgtyhYiicz197TbhS4LPpnxDY").unwrap()
            , false
            )
          , ( Key::from_base58_check("p2pk66qfVMXhFJWhtFDCT6F3JUM3M1iQpfWe4nPZKWcsqsKQtXXHFkQ").unwrap()
            , b"\0"
            , Signature::from_base58_check("p2sigv6HrN6xB5gQDnmKLC2P3ynwiPn4zfUj7CcZD1cepfFzX7xBDWFQu9uoKWbEzVgxCQxrE1J5X6FGYwF2dpoYcjpdPCBhuD").unwrap()
            , true
            )
          , ( Key::from_base58_check("p2pk64bybDUtSjSQnsexpzhedhBo4vkoRX4tWfQQbBxKbA58wJqKkT2").unwrap()
            , b"\x0A"
            , Signature::from_base58_check("p2siggzjojhabur7zZvmNnhkhnU3nYA1ZUR9JSas57RVhNdAmQk6y3hns3F2zPBGsC964PFAE2HC3fbPkcqpVFbjoQQq9dFiZg").unwrap()
            , true
            )
          , ( Key::from_base58_check("p2pk64bybDUtSjSQnsexpzhedhBo4vkoRX4tWfQQbBxKbA58wJqKkT2").unwrap()
            , b"kot"
            , Signature::from_base58_check("p2siggzjojhabur7zZvmNnhkhnU3nYA1ZUR9JSas57RVhNdAmQk6y3hns3F2zPBGsC964PFAE2HC3fbPkcqpVFbjoQQq9dFiZg").unwrap()
            , false
            )
            // binary representation
            // edpk
          , ( Key::from_bytes(&hex::decode("00aad3f16293766169f7db278c5e0e9db4fb82ffe1cbcc35258059617dc0fec082").unwrap()).unwrap()
            , b"\0"
            , Signature::from_bytes(&hex::decode("91ac1e7fd668854fc7a40feec4034e42c06c068cce10622c607fda232db34c8cf5d8da83098dd891cd4cb4299b3fa0352ae323ad99b24541e54b91888fdc8201").unwrap()).unwrap()
            , true
            )
            // sppk
          , ( Key::from_bytes(&hex::decode("0103b524d0184276467c848ac13557fb0ff8bec5907960f72683f22af430503edfc1").unwrap()).unwrap()
            , b"\0"
            , Signature::from_bytes(&hex::decode("80e4e72ffecf72953789625b1125e9f45f432c14e53a01ec68a1e1b77d60cfe96a97443733ba0f7f42db3a56d7a433df2b4fc0035c05ab92d062f33c5bab0244").unwrap()).unwrap()
            , true
            )
            // p2pk
          , ( Key::from_bytes(&hex::decode("0202041e5cb7fb3d7bc6fb7b9e94790919a9e76ccc372e6cc9cae925027c08ff95f3").unwrap()).unwrap()
            , b"\x0A"
            , Signature::from_bytes(&hex::decode("12d25210bb02998516bf6a776e1cd55a06c5fbe3c21afbeef29b99d96305e43263c75a4449906e0f2d79ecc973fff9ce7f8c43fee40b04d07c191f00ee176175").unwrap()).unwrap()
            , true
            )
            // BLpk
          , ( Key::from_bytes(&hex::decode("03ade3c5ec9e1be3dd08eb355f6e23b8e162b90f563fa5cf0b0299fb9f3aa29218483ead20efa8b350559be88bd99cea6c").unwrap()).unwrap()
            , b"\0"
            , Signature::from_bytes(&hex::decode("a065340a9c902829a4d77312c3327b558d310a37305049fc144021ea837325f994e270537a03acfdf9ef276530366b7c1629cb4d71a2b5967b582bfcfd280becb8c918463eb0e5dd0165702a2494b8856baee31e0e7b9f9e5ae5b4af980e88ee").unwrap()).unwrap()
            , true
            )
          ]
    }
}
