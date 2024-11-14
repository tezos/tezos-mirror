// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Encrypted payload.
//!
//! Combines multiple transactions encrypted with the same key and the TE ciphertext
//! which contains the encrypted key and extra fields necessary for verification and
//! reconstruction.

use blstrs::G1Affine;
use rand::{rngs::OsRng, CryptoRng, RngCore};
use rlp::{RlpDecodable, RlpEncodable};

use crate::{
    ciphertext::Ciphertext,
    encrypted_transaction::EncryptedTransaction,
    error::ThresholdEncryptionError,
    helpers::{multi_keccak_256, Bytes32},
};

/// Encrypted envelope containing a common EVM transaction as inner payload.
/// NOTE: RLP encoding does not append tx type tag, use `to_bytes` method instead.
#[derive(Debug, Clone, PartialEq, Eq, RlpEncodable, RlpDecodable)]
pub struct EncryptedPayload {
    /// Encrypted transactions
    pub payload: Vec<EncryptedTransaction>,
    /// Encrypted key
    pub ciphertext: Ciphertext,
}

impl EncryptedPayload {
    /// Encrypt multiple transactions with a randomly (system RNG) generated key,
    /// and then encrypt that key using TE master public key
    pub fn encrypt<T, I>(transactions: I, mpk: &G1Affine) -> Result<Self, ThresholdEncryptionError>
    where
        I: IntoIterator<Item = T>,
        T: AsRef<[u8]>,
    {
        Self::encrypt_with_rng(transactions, mpk, &mut OsRng)
    }

    /// Encrypt multiple transactions with a randomly (specified RNG) generated key,
    /// and then encrypt that key using TE master public key
    pub fn encrypt_with_rng<R, T, I>(
        transactions: I,
        mpk: &G1Affine,
        rng: &mut R,
    ) -> Result<Self, ThresholdEncryptionError>
    where
        I: IntoIterator<Item = T>,
        T: AsRef<[u8]>,
        R: RngCore + CryptoRng,
    {
        // Encrypt transactions with the session symmetric key (ChaCha)
        let mut key = [0u8; 32];
        rng.fill_bytes(&mut key);

        let payload = transactions
            .into_iter()
            .map(|tx| EncryptedTransaction::encrypt_with_key(tx.as_ref(), &key))
            .collect::<Result<Vec<EncryptedTransaction>, ThresholdEncryptionError>>()?;

        let payload_hash = Self::payload_hash(&payload);
        let ciphertext = Ciphertext::encrypt_with_rng(&key, payload_hash, mpk, rng)?;

        Ok(Self {
            payload,
            ciphertext,
        })
    }

    /// Get payload hash which is hash of hashes of unencrypted transactions
    pub fn payload_hash(payload: &[EncryptedTransaction]) -> Bytes32 {
        multi_keccak_256(payload.iter().map(|tx| tx.tx_hash))
    }

    /// Decrypt transactions using the result of combination of decryption shares
    pub fn decrypt_checked(
        &self,
        y_shared_key: &G1Affine,
    ) -> Result<Vec<Vec<u8>>, ThresholdEncryptionError> {
        let key = self.ciphertext.decrypt_checked(y_shared_key)?;
        self.payload
            .iter()
            .map(|tx| tx.decrypt_checked(&key))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Mul;

    use blstrs::{G1Affine, Scalar};
    use pairing_lib::group::prime::PrimeCurveAffine;

    use super::EncryptedPayload;

    #[test]
    fn test_payload_encrypt() {
        let secret_key = Scalar::from(42u64);
        let mpk = G1Affine::generator().mul(&secret_key).into();

        let transactions = vec![vec![1u8; 100], vec![2u8; 200], vec![3u8; 300]];
        let payload = EncryptedPayload::encrypt(&transactions, &mpk).unwrap();

        let (u, _, _) = payload.ciphertext.verify_decode().unwrap();
        let y = u.mul(secret_key).into();
        let res = payload.decrypt_checked(&y).unwrap();

        assert_eq!(res, transactions);
    }
}
