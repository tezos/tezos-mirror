// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Encrypted transaction.

use rlp::{Decodable, Encodable, RlpDecodable, RlpEncodable};

use crate::{
    error::ThresholdEncryptionError,
    helpers::{chacha20_apply, keccak_256, rlp_decode_array, rlp_decode_vec, Bytes32},
};

#[derive(Debug, Clone, PartialEq, Eq, RlpEncodable, RlpDecodable)]
pub struct EncryptedPayload(pub Vec<EncryptedTransaction>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EncryptedTransaction {
    /// Payload transaction encrypted with symmetric (ChaCha) cipher
    pub encrypted_tx: Vec<u8>,
    /// Hash of the unencrypted transaction
    pub tx_hash: Bytes32,
}

impl EncryptedTransaction {
    /// Encrypt transaction bytes using symmetric (ChaCha) key.
    pub fn encrypt_with_key(
        tx_bytes: &[u8],
        key: &Bytes32,
    ) -> Result<Self, ThresholdEncryptionError> {
        let tx_hash = keccak_256(tx_bytes);
        let encrypted_tx = chacha20_apply(tx_bytes, key)
            .map_err(|_| ThresholdEncryptionError::TransactionEncryptionFailed(tx_hash.into()))?;
        Ok(Self {
            encrypted_tx,
            tx_hash,
        })
    }

    /// Decrypt transaction bytes using symmetric (ChaCha) key.
    ///
    /// Will fail if the result does not match the known tx hash.
    pub fn decrypt_checked(&self, key: &Bytes32) -> Result<Vec<u8>, ThresholdEncryptionError> {
        let tx_bytes = chacha20_apply(&self.encrypted_tx, key).map_err(|_| {
            ThresholdEncryptionError::TransactionEncryptionFailed(self.tx_hash.into())
        })?;
        let tx_hash = keccak_256(&tx_bytes);
        if tx_hash != self.tx_hash {
            return Err(ThresholdEncryptionError::TransactionHashMismatch {
                actual: tx_hash.into(),
                expected: self.tx_hash.into(),
            });
        }
        Ok(tx_bytes)
    }
}

impl Encodable for EncryptedTransaction {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(2)
            .append_iter(self.encrypted_tx.clone())
            .append_iter(self.tx_hash);
    }
}

impl Decodable for EncryptedTransaction {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let mut it = rlp.iter();
        Ok(Self {
            encrypted_tx: rlp_decode_vec(it.next().as_ref())?,
            tx_hash: rlp_decode_array(it.next().as_ref())?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::EncryptedTransaction;

    #[test]
    fn test_decrypt_tx() {
        let key = [2u8; 32];
        let tx_bytes = [1u8; 128];
        let encrypted_tx = EncryptedTransaction::encrypt_with_key(&tx_bytes, &key).unwrap();
        let res = encrypted_tx.decrypt_checked(&key).unwrap();
        assert_eq!(tx_bytes.to_vec(), res);
    }

    #[test]
    fn test_encrypted_tx_codec() {
        let key = [2u8; 32];
        let tx_bytes = [1u8; 128];
        let encrypted_tx = EncryptedTransaction::encrypt_with_key(&tx_bytes, &key).unwrap();
        let res = encrypted_tx.decrypt_checked(&key).unwrap();
        assert_eq!(tx_bytes.to_vec(), res);
    }
}
