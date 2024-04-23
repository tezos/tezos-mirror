// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! ECDSA signature wrapper.
//!
//! Provides convenient high-level API for signing / verifying.
//! Implements RLP codec for using in the Etherlink kernel.

use libsecp256k1::{Message, RecoveryId};

use crate::helpers::{rlp_decode_array, Bytes32};

/// Length of a signature in compact form plus one (recovery byte)
pub const SIGNATURE_SIZE: usize = 1 + libsecp256k1::util::SIGNATURE_SIZE;

/// Serialized signature in the compact form with a recovery byte: [ V | R | S ]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature(pub [u8; SIGNATURE_SIZE]);

impl Signature {
    /// Sign 32 byte digest with the provided key
    pub fn create(digest: &Bytes32, signing_key: &libsecp256k1::SecretKey) -> Self {
        let message = Message::parse(digest);
        let (rs, v) = libsecp256k1::sign(&message, signing_key);
        let mut signature = [0u8; SIGNATURE_SIZE];
        signature[0] = v.into();
        signature[1..].copy_from_slice(&rs.serialize());
        Self(signature)
    }

    /// Verify signature and recover public key
    pub fn verify_recover(
        &self,
        digest: &Bytes32,
    ) -> Result<libsecp256k1::PublicKey, libsecp256k1::Error> {
        let message = Message::parse(digest);
        let recovery_id = RecoveryId::parse(self.0[0])?;
        let signature = libsecp256k1::Signature::parse_standard_slice(&self.0[1..])?;
        if signature.s.is_high() {
            // See https://eips.ethereum.org/EIPS/eip-2
            return Err(libsecp256k1::Error::InvalidSignature);
        }
        libsecp256k1::recover(&message, &signature, &recovery_id)
    }

    /// Create zero (invalid) signature
    pub fn zero() -> Self {
        Self([0u8; SIGNATURE_SIZE])
    }
}

impl rlp::Encodable for Signature {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.append_iter(self.0);
    }
}

impl rlp::Decodable for Signature {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        rlp_decode_array(Some(decoder)).map(Signature)
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::OsRng;
    use rlp::Encodable;

    use super::Signature;

    #[test]
    fn test_sign_verify_recover() {
        let secret_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let public_key = libsecp256k1::PublicKey::from_secret_key(&secret_key);

        let sig = Signature::create(&[1u8; 32], &secret_key);
        let pk = sig.verify_recover(&[1u8; 32]).unwrap();
        assert_eq!(pk, public_key);
    }

    #[test]
    fn test_signature_codec() {
        let sig = Signature::zero();
        let encoded = sig.rlp_bytes().to_vec();
        let decoded: Signature = rlp::decode(&encoded).unwrap();
        assert_eq!(sig, decoded);
    }
}
