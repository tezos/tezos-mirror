// SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>

//
// SPDX-License-Identifier: MIT

//! Representation of a DAC message communicating root hashes to the kernel to
//! download pages.

use super::v1::verifiable::TransactionError;
use crypto::hash::{BlsSignature, PublicKeyBls};
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::Zarith;
use tezos_smart_rollup_encoding::dac::{reveal_loop, PreimageHash, MAX_PAGE_SIZE};
use tezos_smart_rollup_host::runtime::Runtime;
use tezos_smart_rollup_host::runtime::RuntimeError;
use thiserror::Error;

const MAX_DAC_LEVELS: usize = 3;

/// DAC external message is a message sent from DAC to rollups to indicate that there
/// is data ready to be revealed. By the time the kernel receives this message, data
/// should already be available locally and ready to be revealed. The raw structure
/// of the external message is root hash ^ aggregate signature ^ witnesses
#[derive(Debug, PartialEq, Eq, NomReader, Clone)]
pub struct ParsedDacMessage {
    /// Root page hash
    pub root_hash: PreimageHash,
    /// Aggregated signature of the DAC committee.
    pub aggregated_signature: BlsSignature,
    /// Data_encoding.Bit_set.t is actually a Z.t
    pub witnesses: Zarith,
}

impl ParsedDacMessage {
    /// Verifies that parsed_dac_message is valid against the given dac_committee
    pub fn verify_signature(
        &self,
        dac_committee: &[PublicKeyBls],
    ) -> Result<(), TransactionError> {
        if cfg!(not(feature = "tx-kernel-no-sig-verif")) {
            let root_hash = self.root_hash.as_ref();
            let mut pk_msg_iter =
                dac_committee.iter().enumerate().filter_map(|(i, member)| {
                    if self.witnesses.0.bit(i as u64) {
                        Some((root_hash.as_slice(), member))
                    } else {
                        None
                    }
                });
            let is_verified = self
                .aggregated_signature
                .aggregate_verify(&mut pk_msg_iter)?;
            if !is_verified {
                return Err(TransactionError::SignatureVerificationError);
            }
        }
        Ok(())
    }

    /// Reveals all message data referenced by [self.root_hash]. Verifies
    /// that the [self.aggregated_signature] was indeed signed by the DAC committee
    pub fn verify_and_reveal_dac_messages(
        &self,
        host: &mut impl Runtime,
        dac_committee: &[PublicKeyBls],
        result_buf: &mut Vec<u8>,
    ) -> Result<(), RevealDacMessageError> {
        self.verify_signature(dac_committee)?;
        self.reveal_dac_message(host, result_buf)
    }

    /// Reveals all message data from referenced by [self.root_hash].
    pub fn reveal_dac_message(
        &self,
        host: &mut impl Runtime,
        result_buf: &mut Vec<u8>,
    ) -> Result<(), RevealDacMessageError> {
        let mut buffer = [0u8; MAX_PAGE_SIZE * MAX_DAC_LEVELS];
        reveal_loop(
            host,
            0,
            self.root_hash.as_ref(),
            buffer.as_mut_slice(),
            MAX_DAC_LEVELS,
            &mut |_, bytes| {
                result_buf.extend_from_slice(bytes.as_ref());
                Ok(())
            },
        )
        .map_err(RevealDacMessageError::RevealLoopError)?;
        Ok(())
    }
}

/// parse_dac_message errors
#[derive(Error, Debug)]
pub enum RevealDacMessageError {
    /// Error from the host's low-level reveal_preimage func
    #[error("Reveal pre-image failed: {0:?}")]
    RevealPreImageError(RuntimeError),

    /// Propagate error from SlicePageError
    #[error("{0:?}")]
    RevealLoopError(&'static str),

    /// Propagate error from TransactionError
    #[error(transparent)]
    SignatureVerificationError(#[from] TransactionError),

    /// Decode errors from fetching page
    #[error("{0}")]
    DecodeError(String),
}

#[cfg(test)]
mod tests {

    use super::*;
    use crypto::blake2b::digest_256;
    use crypto::bls::bls_generate_keypair;
    use crypto::hash::BlsSignature;
    use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
    use tezos_smart_rollup_encoding::dac::make_preimage_hash;
    use tezos_smart_rollup_encoding::testing::make_witnesses;

    #[test]
    fn nom_read_parses_dac_message() {
        let root_hash = make_preimage_hash("example content".as_bytes()).unwrap();
        // Random aggregated signature generated in tezos.
        let aggregated_signature: [u8; 96] = [
            152, 154, 156, b'V', b'i', b'|', 2, b'\'', b'd', 216, 229, 140, 144, b'8',
            b'x', b'k', b'q', 209, 23, b'6', 217, 21, b';', b' ', b'q', 166, 251, 213,
            b'K', 16, b'.', b'a', 129, b'P', 21, b'x', 162, b'c', b'n', 212, 132, b'\t',
            b'(', b'5', b'H', 253, b'>', 179, 7, 19, 229, 164, b'Z', 158, 197, 220, 200,
            b')', b'\'', b'a', b't', 165, 161, 147, b'8', b'K', 136, b'R', b' ', b']',
            227, b'K', b'G', 227, 225, 169, 244, 19, b'd', 21, 228, 127, 179, 249, 233,
            195, 159, b'[', 163, b'O', b'0', 167, 200, b'#', 213, 243,
        ];
        let witnesses = 1;
        let mut valid_bytes = Vec::<u8>::new();
        valid_bytes.extend_from_slice(&root_hash);
        valid_bytes.extend_from_slice(&aggregated_signature);
        valid_bytes.extend_from_slice(&[1]);

        let expected_message = ParsedDacMessage {
            root_hash: PreimageHash::from(&root_hash),
            aggregated_signature: BlsSignature(aggregated_signature.to_vec()),
            witnesses: make_witnesses(witnesses),
        };
        let (_remaining, actual_message) =
            ParsedDacMessage::nom_read(&valid_bytes).expect("Parse failure");

        assert_eq!(expected_message, actual_message);
    }
    #[test]
    fn nom_read_fails_invalid_dac_message() {
        let message = b"bound to fail bound to fail bound to fail bound to fail";
        assert!(ParsedDacMessage::nom_read(message).is_err());
    }

    #[test]
    fn verify_root_hash() {
        let root_hash = make_preimage_hash("example content".as_bytes()).unwrap();
        let (_signatory1, pk1) = bls_generate_keypair().unwrap();
        let (signatory2, pk2) = bls_generate_keypair().unwrap();
        let (signatory3, pk3) = bls_generate_keypair().unwrap();

        let dac_committee = [pk1, pk2, pk3];

        let sig2 = signatory2.sign(root_hash).unwrap();
        let sig3 = signatory3.sign(root_hash).unwrap();
        let witnesses = make_witnesses(2 | 4);

        let aggregated_signature = BlsSignature::aggregate_sigs(&[&sig2, &sig3]).unwrap();

        let dac_message = ParsedDacMessage {
            root_hash: PreimageHash::from(&root_hash),
            aggregated_signature,
            witnesses,
        };

        // Verifies that only signatory 2 and 3 signed the message the signature
        // and that aggregated signature is correctly verified.
        let res =
            ParsedDacMessage::verify_signature(&dac_message, dac_committee.as_slice());
        assert!(res.is_ok());

        // Fails when the wrong bitset is provided
        let bad_dac_message = ParsedDacMessage {
            witnesses: make_witnesses(1 | 2 | 4),
            ..dac_message.clone()
        };
        let res = ParsedDacMessage::verify_signature(
            &bad_dac_message,
            dac_committee.as_slice(),
        );
        assert!(res.is_err());

        let invalid_hash: [u8; PREIMAGE_HASH_SIZE - 1] =
            digest_256(b"some bad content").unwrap().try_into().unwrap();

        let mut invalid_root_hash = [0_u8; PREIMAGE_HASH_SIZE];
        invalid_root_hash[1..].copy_from_slice(&invalid_hash);

        let bad_dac_message = ParsedDacMessage {
            root_hash: PreimageHash::from(&invalid_root_hash),
            ..dac_message
        };
        // Asert that an aggregated signature of the wrong root hash is rejected
        // by the signature's verification.
        let res = ParsedDacMessage::verify_signature(
            &bad_dac_message,
            dac_committee.as_slice(),
        );
        assert!(res.is_err());
    }
}
