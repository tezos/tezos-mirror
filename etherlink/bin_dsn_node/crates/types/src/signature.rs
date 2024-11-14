// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! ECDSA signature wrapper.
//!
//! Provides convenient high-level API for signing / verifying.
//! Implements RLP codec for using in the Etherlink kernel.

use libsecp256k1::{Message, RecoveryId};
use primitive_types::H160;
use threshold_encryption::helpers::{keccak_256, rlp_decode_array, Bytes32};

/// Length of a signature in compact form plus one (recovery byte)
pub const SIGNATURE_SIZE: usize = libsecp256k1::util::SIGNATURE_SIZE + 1;

/// Serialized signature in the compact form with a recovery byte: [ R | S | V ]
/// Compatible with ethers-rs serialized format:
/// https://github.com/gakonst/ethers-rs/blob/51fe937f6515689b17a3a83b74a05984ad3a7f11/ethers-core/src/types/signature.rs#L240
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
    pub fn recover_pubkey(
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

/// Get EVM address from secp256k1 public key
///
/// EVM address is last 20 bytes of keccak(X || Y) where X,Y is the coordinates of the EC point
/// representing public key.
pub fn public_key_to_h160(public_key: &libsecp256k1::PublicKey) -> H160 {
    // libsecp256k1 adds extra tag so we need to strip it
    let pk_bytes = &public_key.serialize()[1..];
    let pk_digest = keccak_256(pk_bytes);
    let value: [u8; 20] = pk_digest.as_slice()[12..]
        .try_into()
        .expect("Keccak256 output must be 32 bytes");
    H160(value)
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
        let pk = sig.recover_pubkey(&[1u8; 32]).unwrap();
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
