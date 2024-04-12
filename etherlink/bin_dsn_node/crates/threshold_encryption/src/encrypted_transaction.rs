// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Encrypted transaction type.
//!
//! EIP-2718 compatible format: [ Transaction type | Transaction payload ]

use rlp::{DecoderError, Encodable, RlpDecodable, RlpEncodable, RlpStream};

use crate::{
    ciphertext::Ciphertext,
    error::ThresholdEncryptionError,
    helpers::{keccak_256, multi_keccak_256, Bytes32},
    signature::Signature,
};

/// EIP-2718 encrypted transaction type
pub const ENCRYPTED_TX_TYPE: u8 = 0x7e;

/// Encrypted envelope containing a common EVM transaction as inner payload.
/// NOTE: RLP encoding does not append tx type tag, use `to_bytes` method instead.
#[derive(Debug, Clone, PartialEq, Eq, RlpEncodable, RlpDecodable)]
pub struct EncryptedTransaction {
    /// Encrypted payload
    pub ciphertext: Ciphertext,
    /// Outer transaction nonce (related to the bundler's account)
    pub nonce: u64,
    /// Authentication tag produced by the transaction bundler
    pub signature: Signature,
}

impl EncryptedTransaction {
    /// Assemble encrypted transaction from parts and sign with a provided key
    pub fn create_signed(
        ciphertext: Ciphertext,
        nonce: u64,
        signing_key: &libsecp256k1::SecretKey,
    ) -> Self {
        let digest = multi_keccak_256([&ciphertext.rlp_bytes(), nonce.to_be_bytes().as_slice()]);
        Self {
            signature: Signature::create(&digest, signing_key),
            nonce,
            ciphertext,
        }
    }

    /// Verify transaction signature and recover public key
    pub fn verify_recover(&self) -> Result<libsecp256k1::PublicKey, ThresholdEncryptionError> {
        let digest = multi_keccak_256([
            &self.ciphertext.rlp_bytes(),
            self.nonce.to_be_bytes().as_slice(),
        ]);
        self.signature
            .verify_recover(&digest)
            .map_err(ThresholdEncryptionError::SignatureInvalid)
    }

    /// Compute transaction hash
    pub fn hash(&self) -> Bytes32 {
        keccak_256(&self.to_bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<EncryptedTransaction, DecoderError> {
        let tx_type = *bytes.first().ok_or(DecoderError::Custom("Empty bytes"))?;
        if tx_type == ENCRYPTED_TX_TYPE {
            rlp::decode(&bytes[1..])
        } else {
            Err(DecoderError::Custom("Unexpected transaction type"))
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut stream = RlpStream::new();
        stream.append_raw(&[ENCRYPTED_TX_TYPE], 1);
        self.rlp_append(&mut stream);
        stream.out().to_vec()
    }

    pub fn from_hex(e: String) -> Result<EncryptedTransaction, DecoderError> {
        let tx = hex::decode(e).or(Err(DecoderError::Custom("Couldn't parse hex value")))?;
        Self::from_bytes(&tx)
    }

    pub fn to_hex(&self) -> String {
        hex::encode(self.to_bytes())
    }
}

#[cfg(test)]
mod tests {
    use rand::rngs::OsRng;

    use crate::{ciphertext::tests::dummy_ciphertext, encrypted_transaction::ENCRYPTED_TX_TYPE};

    use super::EncryptedTransaction;

    #[test]
    fn test_encrypted_transaction_sign_verify() {
        // Ensure we calculate digest correctly
        let secret_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let public_key = libsecp256k1::PublicKey::from_secret_key(&secret_key);
        let tx = EncryptedTransaction::create_signed(dummy_ciphertext(), 1, &secret_key);
        let pk = tx.verify_recover().unwrap();
        assert_eq!(pk, public_key);
    }

    #[test]
    fn test_encrypted_transaction_codec() {
        let signing_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let tx = EncryptedTransaction::create_signed(dummy_ciphertext(), 0, &signing_key);
        let encoded = tx.to_bytes();
        assert_eq!(encoded[0], ENCRYPTED_TX_TYPE);
        let decoded = EncryptedTransaction::from_bytes(&encoded).unwrap();
        assert_eq!(decoded, tx);
    }
}
