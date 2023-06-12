// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Encoding of Dac certificates. Dac certificates are versioned: the
//! first byte in a serialized Dac certificate is indicative of the
//! version of the certificate itself.

// TODO: <https://github.com/trilitech/tezedge/issues/17>
//       lint triggered by issue in encoding macro
#![allow(clippy::useless_format)]
#![cfg(feature = "alloc")]

use super::PreimageHash;
use tezos_crypto_rs::hash::{BlsSignature, PublicKeyBls};
use tezos_crypto_rs::CryptoError;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::Zarith;
use thiserror::Error;

/// Errors that can be obtained when verifying an invalid certificate
#[derive(Debug, Error)]
pub enum CertificateVerificationError {
    /// Number of signatures is lower than the required threshold
    #[error(
        "Insufficient number of signatures - threshold: {threshold}, actual: {actual}"
    )]
    InsufficientNumberOfSignatures {
        /// the threshold of signatures required for the certificate to be valid
        threshold: usize,
        /// the number of signatures provided by the certificate
        actual: usize,
    },
    /// Cryptographic primitives result in error while verifying the signature
    #[error("Error propagated by cryptographic primitives while verifying the aggregate signature: {0}")]
    SignatureVerificationFailed(CryptoError),
    /// Signature Verification failed
    #[error("Verification of aggregate signature failed")]
    InvalidAggregateSignature,
}

/// DAC certificates are signed by committee members to ensure validity.
///
/// Currently only one certificate type is supported, but more will be added in future.
#[derive(Debug, HasEncoding, NomReader, BinWriter)]
#[encoding(tags = "u8")]
pub enum Certificate {
    /// A V0 certificate - see [`V0Certificate`]
    #[encoding(tag = 0)]
    V0(V0Certificate),
}

/// A Dac V0 certificate.
#[derive(Debug, HasEncoding, NomReader, BinWriter)]
pub struct V0Certificate {
    /// The preimage hash of the root [`V0HashPage`].
    ///
    /// [`V0HashPage`]: crate::dac::pages::V0HashPage
    pub root_hash: PreimageHash,
    /// Aggregated signature of the DAC committee.
    pub aggregated_signature: BlsSignature,
    /// Data_encoding.Bit_set.t is actually a Z.t
    pub witnesses: Zarith,
}

impl Certificate {
    /// Verifies that a certificate is valid.
    ///
    /// For a [`V0Certificate`], the aggregated signature is verified against the public keys
    /// of the committee members that have signed the root hash.
    /// This set is determined by the witness field of the certificate.
    pub fn verify(
        &self,
        committee_members_pks: &[PublicKeyBls],
        threshold: u8,
    ) -> Result<(), CertificateVerificationError> {
        match self {
            Certificate::V0(V0Certificate {
                root_hash,
                aggregated_signature,
                witnesses,
            }) => {
                let root_hash = root_hash.as_ref();
                let root_hash_with_signing_committee_members: Vec<(
                    &[u8],
                    &PublicKeyBls,
                )> = committee_members_pks
                    .iter()
                    .enumerate()
                    .filter_map(|(i, member)| {
                        if witnesses.0.bit(i as u64) {
                            Some((root_hash.as_slice(), member))
                        } else {
                            None
                        }
                    })
                    .collect();
                let num_of_signatures = root_hash_with_signing_committee_members.len();
                if num_of_signatures < threshold.into() {
                    return Err(
                        CertificateVerificationError::InsufficientNumberOfSignatures {
                            threshold: threshold.into(),
                            actual: num_of_signatures,
                        },
                    );
                }
                let is_valid_signature = aggregated_signature
                    .aggregate_verify(
                        &mut root_hash_with_signing_committee_members.into_iter(),
                    )
                    .map_err(|e| {
                        CertificateVerificationError::SignatureVerificationFailed(e)
                    })?;
                if is_valid_signature {
                    Ok(())
                } else {
                    Err(CertificateVerificationError::InvalidAggregateSignature)
                }
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;

    // taken from the output of octez-dac-client GET certificate
    const EXAMPLE_CERTIFICATE: &[u8] = &[
        0, 0, 204, 9, 129, 240, 102, 52, 10, 53, 123, 37, 68, 170, 38, 233, 224, 134, 20,
        232, 167, 184, 41, 144, 214, 112, 159, 30, 168, 167, 136, 31, 88, 20, 175, 112,
        85, 58, 94, 159, 20, 223, 225, 53, 195, 22, 34, 234, 126, 147, 73, 182, 121, 246,
        203, 68, 148, 68, 97, 148, 131, 235, 45, 166, 56, 40, 14, 35, 157, 61, 50, 244,
        168, 9, 53, 163, 157, 147, 72, 7, 64, 42, 18, 28, 64, 100, 19, 84, 217, 144, 137,
        8, 222, 131, 163, 114, 139, 9, 171, 110, 148, 66, 196, 7, 29, 186, 90, 182, 223,
        116, 220, 113, 160, 153, 106, 149, 6, 54, 87, 46, 44, 175, 105, 218, 26, 187,
        254, 6, 132, 30, 3,
    ];

    #[test]
    fn encode_decode_certificate() {
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE)
            .expect("Deserialization should work");
        let mut buffer = Vec::new();
        certificate
            .bin_write(&mut buffer)
            .expect("Serialization should work");
        assert_eq!(buffer.as_slice(), EXAMPLE_CERTIFICATE);
    }
}
