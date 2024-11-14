// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Threshold encryption error type.

use primitive_types::H256;

#[derive(Debug, thiserror::Error)]
pub enum ThresholdEncryptionError {
    #[error("Ciphertext is invalid: {0}")]
    CiphertextInvalid(&'static str),
    #[error("Decryption share is invalid: {0}")]
    DecryptionShareInvalid(&'static str),
    #[error("Decrypted transaction hash {actual} does not match the expected one {expected}")]
    TransactionHashMismatch { expected: H256, actual: H256 },
    #[error(
        "Decrypted key hash {actual} does not match the expected one {expected} (tx {tx_hash})"
    )]
    KeyHashMismatch {
        expected: H256,
        actual: H256,
        tx_hash: H256,
    },
    #[error(
        "Failed to encrypt transaction: ChaCha cipher has reached the end of a keystream (tx {0})"
    )]
    TransactionEncryptionFailed(H256),
}
