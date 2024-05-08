// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Encrypted bundle.
//!
//! A special Etherlink transaction type that bundles multiple encrypted transactions,
//! and which is signed by the Bundler.
//!
//! EIP-2718 compatible format: [ Transaction type | Transaction payload ]

use primitive_types::H160;
use rlp::{Encodable, RlpDecodable, RlpEncodable, RlpStream};
use threshold_encryption::{
    encrypted_payload::EncryptedPayload,
    helpers::{keccak_256, Bytes32},
};

use crate::signature::{public_key_to_h160, Signature};

/// EIP-2718 compatible transaction type
pub const ENCRYPTED_BUNDLE_TAG: u8 = 0x7e;

/// Encrypted envelope containing multiple encrypted EVM transaction as inner payload.
/// NOTE: RLP encoding does not append tx type tag, use `to_bytes` method instead.
#[derive(Debug, Clone, PartialEq, Eq, RlpEncodable, RlpDecodable)]
pub struct EncryptedBundle {
    /// Encrypted payload
    pub payload: EncryptedPayload,
    /// Outer transaction nonce (related to the bundler's account)
    pub nonce: u64,
    /// Authentication tag produced by the transaction bundler
    /// If this field is set to None, the bundle is unsigned
    pub signature: Option<Signature>,
}

#[derive(Debug, thiserror::Error)]
pub enum EncryptedBundleError {
    #[error("Bundle is not signed")]
    SignatureMissing,
    #[error("Signature is not valid: {0}")]
    SignatureInvalid(libsecp256k1::Error),
    #[error("Cannot decode from empty input")]
    EmptyBytes,
    #[error("Failed to decode from bytes: {0}")]
    DecodeFromBytes(#[source] rlp::DecoderError),
    #[error("Unexpected binary tag: {0}")]
    UnexpectedBinaryTag(u8),
    #[error("Failed to decode from hex: {0}")]
    DecodeFromHex(#[source] hex::FromHexError),
}

impl EncryptedBundle {
    /// Create new unsigned bundle given encrypted payload and Bundler's nonce
    pub fn new_unsigned(payload: EncryptedPayload, nonce: u64) -> Self {
        Self {
            payload,
            nonce,
            signature: None,
        }
    }

    /// Get data that is to be signed by the Bundler
    pub fn as_sign_data(&self) -> Vec<u8> {
        let mut stream = rlp::RlpStream::new();
        stream.append(&self.payload).append(&self.nonce);
        stream.out().to_vec()
    }

    /// Sign the bundle using a secret key (chaining method)
    pub fn sign_with_key(mut self, signing_key: &libsecp256k1::SecretKey) -> Self {
        let digest = keccak_256(&self.as_sign_data());
        self.signature = Some(Signature::create(&digest, signing_key));
        self
    }

    /// Set the signature obtained externally (chaining method)
    pub fn with_signature<S: Into<Signature>>(mut self, signature: S) -> Self {
        self.signature = Some(signature.into());
        self
    }

    /// Verify transaction signature and recover public key
    pub fn recover_pubkey(&self) -> Result<libsecp256k1::PublicKey, EncryptedBundleError> {
        let digest = keccak_256(&self.as_sign_data());
        self.signature
            .as_ref()
            .ok_or(EncryptedBundleError::SignatureMissing)?
            .recover_pubkey(&digest)
            .map_err(EncryptedBundleError::SignatureInvalid)
    }

    /// Verify transaction signature and recover signer's EVM address
    pub fn recover_address(&self) -> Result<H160, EncryptedBundleError> {
        Ok(public_key_to_h160(&self.recover_pubkey()?))
    }

    /// Compute transaction hash
    pub fn hash(&self) -> Bytes32 {
        keccak_256(&self.to_bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<EncryptedBundle, EncryptedBundleError> {
        let tx_type = *bytes.first().ok_or(EncryptedBundleError::EmptyBytes)?;
        if tx_type == ENCRYPTED_BUNDLE_TAG {
            rlp::decode(&bytes[1..]).map_err(EncryptedBundleError::DecodeFromBytes)
        } else {
            Err(EncryptedBundleError::UnexpectedBinaryTag(tx_type))
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut stream = RlpStream::new();
        stream.append_raw(&[ENCRYPTED_BUNDLE_TAG], 1);
        self.rlp_append(&mut stream);
        stream.out().to_vec()
    }

    pub fn from_hex(e: String) -> Result<EncryptedBundle, EncryptedBundleError> {
        let tx = hex::decode(e).map_err(EncryptedBundleError::DecodeFromHex)?;
        Self::from_bytes(&tx)
    }

    pub fn to_hex(&self) -> String {
        hex::encode(self.to_bytes())
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::OsRng;
    use threshold_encryption::{
        ciphertext::Ciphertext, encrypted_payload::EncryptedPayload,
        encrypted_transaction::EncryptedTransaction,
    };

    use super::{EncryptedBundle, ENCRYPTED_BUNDLE_TAG};

    fn dummy_payload() -> EncryptedPayload {
        EncryptedPayload {
            payload: vec![EncryptedTransaction {
                encrypted_tx: vec![1u8; 128],
                tx_hash: [2u8; 32],
            }],
            ciphertext: Ciphertext {
                payload_hash: [3u8; 32],
                encrypted_key: [4u8; 32],
                key_hash: [5u8; 32],
                u_nonce: [6u8; 48],
                w_sig: [7u8; 96],
            },
        }
    }

    #[test]
    fn test_encrypted_transaction_sign_verify() {
        // Ensure we calculate digest correctly
        let secret_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let public_key = libsecp256k1::PublicKey::from_secret_key(&secret_key);
        let tx = EncryptedBundle::new_unsigned(dummy_payload(), 1).sign_with_key(&secret_key);
        let pk = tx.recover_pubkey().unwrap();
        assert_eq!(pk, public_key);
    }

    #[test]
    fn test_encrypted_transaction_codec() {
        let signing_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let tx = EncryptedBundle::new_unsigned(dummy_payload(), 0).sign_with_key(&signing_key);
        let encoded = tx.to_bytes();
        assert_eq!(encoded[0], ENCRYPTED_BUNDLE_TAG);
        let decoded = EncryptedBundle::from_bytes(&encoded).unwrap();
        assert_eq!(decoded, tx);
    }
}
