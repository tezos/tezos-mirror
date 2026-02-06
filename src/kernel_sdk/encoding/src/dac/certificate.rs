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
use super::SlicePage;
use super::SlicePageError;
use super::V0SliceContentPage;
use super::V0SliceHashPage;
use super::MAX_PAGE_SIZE;
use tezos_crypto_rs::hash::BlsSignature;
#[cfg(feature = "bls")]
use tezos_crypto_rs::hash::PublicKeyBls;
use tezos_crypto_rs::CryptoError;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::encoding::HasEncoding;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::Zarith;
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

/// Errors that can be obtained when handling a certificate.
#[derive(Debug, Error)]
pub enum CertificateError {
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
    /// Storage error when revealing a certificate
    #[error("Failed to write certificate contents to {0}:{1}")]
    StorageError(String, RuntimeError),
    /// Error occurred revealing page of content
    #[error("Could not reveal content of {0:?}:{1}")]
    RevealError(PreimageHash, RuntimeError),
    /// Revealed page has invalid encoding
    #[error("Revealed page is invalid: {0:?}")]
    PageError(SlicePageError),
    /// Revealed content is/would be larger than 10,063,860 bytes
    #[error("Payload too large")]
    PayloadTooLarge,
}

impl From<SlicePageError> for CertificateError {
    fn from(e: SlicePageError) -> Self {
        Self::PageError(e)
    }
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
    #[cfg(feature = "bls")]
    pub fn verify(
        &self,
        committee_members_pks: &[PublicKeyBls],
        threshold: u8,
    ) -> Result<(), CertificateError> {
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
                    return Err(CertificateError::InsufficientNumberOfSignatures {
                        threshold: threshold.into(),
                        actual: num_of_signatures,
                    });
                }
                let is_valid_signature = aggregated_signature
                    .aggregate_verify(
                        &mut root_hash_with_signing_committee_members.into_iter(),
                    )
                    .map_err(CertificateError::SignatureVerificationFailed)?;
                if is_valid_signature {
                    Ok(())
                } else {
                    Err(CertificateError::InvalidAggregateSignature)
                }
            }
        }
    }

    /// Reveal the contents of the certificate to a path in storage.
    ///
    /// Write the revealed content of the certificate to the given path. Any previous value is
    /// overwritten.
    ///
    /// Up to ~10MB of content may be revealed in one go (`10,063,860 bytes`). The
    /// content overwrites any pre-existing value in `path`. If an error occurs,
    /// the content revealed up-to the error occuring will be written in `path`.
    ///
    /// **CAUTION**: this function will consume up to `~1.3 Billion` ticks, it is
    /// recommended to limit any other computation done within the same `kernel_run`,
    /// and to perform your own benchmarking.
    ///
    /// If `reveal_to_store` returns an error, any content succesfully revealed
    /// up until the error occurred will remain in storage; use [StorageV1::store_value_size] to
    /// determine the size of this value.
    ///
    /// [StorageV1::store_value_size]: tezos_smart_rollup_host::storage::StorageV1::store_value_size
    pub fn reveal_to_store<Host: Runtime>(
        &self,
        host: &mut Host,
        path: &impl Path,
    ) -> Result<usize, CertificateError> {
        const MAX_TOP_LEVEL_HASHES: usize = 20;

        // We reveal the root_hash, followed by twenty top level hashes. Each of these
        // hashes can reveal up to MAX_HASHES_PER_PAGE, which then all reveal to content
        // pages.
        //
        // Therefore, we can reveal up to:
        //   [1 * MAX_TOP_LEVEL_HASHES * MAX_HASHES_PER_PAGE * MAX_PAGE_SIZE] bytes
        // = [1 * 20                   * 123                 * 4091         ] bytes
        // = [10063860                                                      ] bytes
        const MAX_REVEALS: usize = 1
            + MAX_TOP_LEVEL_HASHES
            + V0SliceHashPage::MAX_HASHES_PER_PAGE * MAX_TOP_LEVEL_HASHES;

        host.store_delete_value(path)
            .map_err(|error| CertificateError::StorageError(path.to_string(), error))?;

        let buffer = &mut [0u8; MAX_PAGE_SIZE];
        let mut written = 0;

        let mut save = |host: &mut Host, content: V0SliceContentPage| {
            let content = content.as_ref();
            host.store_write(path, content, written)
                .map_err(|e| CertificateError::StorageError(path.to_string(), e))
                .map(|()| {
                    written += content.len();
                    written
                })
        };

        let Self::V0(V0Certificate { root_hash, .. }) = self;

        let mut revealed = 0;
        let mut hashes = Vec::with_capacity(MAX_REVEALS);
        hashes.push(*root_hash.as_ref());

        while let Some(hash) = hashes.get(revealed) {
            let (page, _) = fetch_page(host, hash, buffer)?;
            revealed += 1;

            match page {
                SlicePage::V0HashPage(page) => {
                    let num_allowed = MAX_REVEALS - hashes.len();

                    if page.inner.len() > num_allowed * PREIMAGE_HASH_SIZE {
                        return Err(CertificateError::PayloadTooLarge);
                    }

                    for hash in page.hashes() {
                        hashes.push(*hash);
                    }
                }
                SlicePage::V0ContentPage(page) => {
                    save(host, page)?;
                }
            }
        }

        Ok(written)
    }
}

fn fetch_page<'a>(
    host: &impl Runtime,
    hash: &[u8; PREIMAGE_HASH_SIZE],
    buffer: &'a mut [u8],
) -> Result<(SlicePage<'a>, &'a mut [u8]), CertificateError> {
    super::fetch_page_raw(host, hash, buffer)
        .map_err(|err| CertificateError::RevealError(hash.into(), err))
        .and_then(|(page, buffer)| Ok((SlicePage::try_from(page)?, buffer)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_data_encoding::enc::BinWriter;
    use tezos_data_encoding::nom::NomReader;

    // taken from the output of octez-dac-client GET certificate
    // Committee member 0 - public key hash: tz4Ate2Fj1QpVXBGLXioe57s3a1RUtavMS5P
    // Committee member 0 - public key: BLpk1tsVzqCokL6dZEiCQgEvwqQp4btiHYm3A1HoEUxKUwq5jCNZMJQ7bU71QE969KioUWCKtK9F
    // Committee member 1 - public key hash: tz4PA6aEFXbaSZXSmdTi933GQZPodn6VX8Q3
    // Committee member 1 - public key: BLpk1xQMdGocMdiiuU2pGvNMeu8vP91nNfrKk5tCssvPzP4z9EY7k5bbEisrqN3pT9vaoN2dsSiW
    // Hex payload - 0000000000000000
    // Root hash - hex value: 005b55fa3bb27fa3644faa7bd1e4ce79319a41ff35a3c2128089224e2fbf918143
    const EXAMPLE_CERTIFICATE: &[u8] = &[
        0, 0, 91, 85, 250, 59, 178, 127, 163, 100, 79, 170, 123, 209, 228, 206, 121, 49,
        154, 65, 255, 53, 163, 194, 18, 128, 137, 34, 78, 47, 191, 145, 129, 67, 130,
        182, 229, 184, 224, 94, 136, 21, 243, 179, 240, 183, 241, 10, 232, 158, 214, 59,
        15, 133, 100, 251, 67, 218, 154, 230, 151, 140, 184, 73, 49, 113, 11, 82, 243,
        76, 154, 144, 156, 200, 188, 66, 24, 25, 43, 143, 115, 199, 11, 121, 55, 113, 90,
        110, 66, 245, 66, 38, 36, 56, 169, 135, 207, 146, 121, 205, 25, 89, 34, 12, 160,
        6, 64, 8, 169, 87, 137, 69, 56, 134, 18, 251, 240, 113, 214, 158, 98, 122, 80,
        34, 80, 223, 83, 14, 126, 42, 3,
    ];

    const COMMITTEE_MEMBER_0_B58_PK: &str =
        "BLpk1tsVzqCokL6dZEiCQgEvwqQp4btiHYm3A1HoEUxKUwq5jCNZMJQ7bU71QE969KioUWCKtK9F";

    const COMMITTEE_MEMBER_1_B58_PK: &str =
        "BLpk1xQMdGocMdiiuU2pGvNMeu8vP91nNfrKk5tCssvPzP4z9EY7k5bbEisrqN3pT9vaoN2dsSiW";

    const COMMITTEE_MEMBER_2_B58_PK: &str =
        "BLpk1xeM4fERgfDR13qxjRgT9DCtqL9qUo7PHxncNmo8NEQgW93QyJm4ySvYbwc4YwJxj6d9Jd8t";

    fn to_public_key(b58_pk: &str) -> PublicKeyBls {
        PublicKeyBls::from_base58_check(b58_pk).unwrap()
    }

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

    #[test]
    fn verify_valid_certificate_signed_by_all_committee_members() {
        let committee = vec![
            to_public_key(COMMITTEE_MEMBER_0_B58_PK),
            to_public_key(COMMITTEE_MEMBER_1_B58_PK),
        ];
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE).unwrap();
        assert!(matches!(certificate.verify(&committee, 2), Ok(())))
    }

    #[test]
    fn verify_valid_certificate_signed_by_enough_committee_members() {
        let committee = vec![
            to_public_key(COMMITTEE_MEMBER_0_B58_PK),
            to_public_key(COMMITTEE_MEMBER_1_B58_PK),
            to_public_key(COMMITTEE_MEMBER_2_B58_PK),
        ];
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE).unwrap();
        assert!(matches!(certificate.verify(&committee, 2), Ok(())))
    }

    #[test]
    fn verify_invalid_certificate_insufficient_number_of_signatures() {
        let committee = vec![
            to_public_key(COMMITTEE_MEMBER_0_B58_PK),
            to_public_key(COMMITTEE_MEMBER_1_B58_PK),
        ];
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE).unwrap();
        assert!(matches!(
            certificate.verify(&committee, 3),
            Err(CertificateError::InsufficientNumberOfSignatures {
                threshold: 3,
                actual: 2
            })
        ))
    }

    #[test]
    fn verify_invalid_certificate_invalid_aggregate_signature() {
        let committee = vec![
            to_public_key(COMMITTEE_MEMBER_0_B58_PK),
            to_public_key(COMMITTEE_MEMBER_2_B58_PK),
        ];
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE).unwrap();
        assert!(matches!(
            certificate.verify(&committee, 2),
            Err(CertificateError::InvalidAggregateSignature),
        ))
    }

    #[test]
    fn verify_invalid_certificate_committee_members_out_of_order() {
        // To check this scenario, we swap a committee member that signed the
        // certificate with one that did not. If we swapped committee members
        // that signed the certificate, then the set of committee members used
        // to verify the signature will not change, and the verification will pass.
        let committee = vec![
            to_public_key(COMMITTEE_MEMBER_2_B58_PK),
            to_public_key(COMMITTEE_MEMBER_0_B58_PK),
            to_public_key(COMMITTEE_MEMBER_1_B58_PK),
        ];
        let (_, certificate) = Certificate::nom_read(EXAMPLE_CERTIFICATE).unwrap();
        assert!(matches!(
            certificate.verify(&committee, 2),
            Err(CertificateError::InvalidAggregateSignature),
        ));
    }
}
