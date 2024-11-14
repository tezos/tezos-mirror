// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Attestation committee and quorum certificate.
//!
//! Both structures implement RLP codec to be usable in the Etherlink kernel.

use std::collections::HashSet;

use primitive_types::{H160, H256};
use rlp::{RlpDecodable, RlpEncodable};
use serde::{Deserialize, Serialize};
use threshold_encryption::helpers::multi_keccak_256;

use crate::signature::{public_key_to_h160, Signature};

/// Attestation committee is a list of EVM addresses of keyholders (aka authorities)
/// eligible to produce blueprint attestations. It also contains quorum level -
/// minimum necessary number of valid attestations to consider a particular blueprint valid.
///
/// Committee structure is stored in the kernel storage and is used to verify blueprint
/// attestations provided by keyholders. It is also used by the sequencer to check the
/// attestations before including them in the blueprint.
///
/// A certain committee is expected to be actual for a particular epoch, that in its
/// turn tied to a specified L1 block range. That way we can enable rotation of keyholders
/// as well as their keys.
///
/// Any changes to the committee can be done with the kernel upgrade through the
/// L1 governance vote. That is, bakers select committee members, attestation quorum, and
/// make sure the kernel upgrade reflecting that changes is applied.
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, RlpEncodable, RlpDecodable)]
pub struct Committee {
    /// List of EVM addresses of active keyholders (authorities)
    /// NOTE that index DOES matter in this case and it must be as per DKG/SSS instantiation
    pub authorities: Vec<H160>,
}

/// Quorum certificate (QC) is a list of signatures (attestations) provided by keyholders
/// (authorities) where signed message is hash of the current payload which includes:
///     - hash of content (list of transactions)
///     - hash of the previous payload
/// Having these parts separated helps to diagnose the issue in case QC is invalid.
#[derive(Debug, PartialEq, Eq, RlpEncodable, RlpDecodable)]
pub struct QuorumCertificate {
    /// List of signatures of payload hash
    pub attestations: Vec<Signature>,
    /// Multi keccak256 hash of serialized transactions in the blueprint
    pub content_hash: H256,
    /// Previous (last applied) payload hash
    pub prev_payload_hash: H256,
}

#[derive(Debug, thiserror::Error)]
pub enum QuorumCertificateError {
    #[error("Not enough attestations")]
    QuorumNotMet,
    #[error("Unknown signing authority")]
    AuthorityUnknown,
    #[error("Same authority attested twice")]
    AuthorityDoubleAttested,
    #[error("Signature is not valid: {0}")]
    SignatureInvalid(libsecp256k1::Error),
}

impl QuorumCertificate {
    /// Verify that:
    ///     - All signatures (attestations) are valid
    ///     - All signatures produced by distinct known authorities
    ///     - The number of signatures is larger than the required quorum
    ///
    /// NOTE that content hash and previous payload hash have to be checked
    /// separately to ensure consistency with the application state.
    pub fn verify(&self, committee: &Committee) -> Result<(), QuorumCertificateError> {
        if self.attestations.len() < committee.quorum() {
            return Err(QuorumCertificateError::QuorumNotMet);
        }
        let payload_hash = self.payload_hash();
        let mut checked_authorities = HashSet::new();

        for signature in self.attestations.iter() {
            let public_key = signature
                .recover_pubkey(&payload_hash.0)
                .map_err(QuorumCertificateError::SignatureInvalid)?;
            let authority = public_key_to_h160(&public_key);

            if checked_authorities.contains(&authority) {
                return Err(QuorumCertificateError::AuthorityDoubleAttested);
            }
            if !committee.authorities.contains(&authority) {
                return Err(QuorumCertificateError::AuthorityUnknown);
            }
            checked_authorities.insert(authority);
        }
        Ok(())
    }

    /// Calculate payload hash
    pub fn payload_hash(&self) -> H256 {
        multi_keccak_256([&self.content_hash, &self.prev_payload_hash].iter()).into()
    }

    /// Genesis QC
    pub fn genesis() -> Self {
        Self {
            attestations: vec![],
            content_hash: H256::zero(),
            prev_payload_hash: H256::zero(),
        }
    }
}

impl Committee {
    /// Minimum number of valid attestations to consider a particular blueprint valid
    pub fn quorum(&self) -> usize {
        // Total number of nodes is committee (keyhodlers) size + 1 (sequencer) = 4f
        // Attestation quorum is 3f - 1
        self.authorities.len() - (self.authorities.len() + 1) / 4
    }
}

#[cfg(test)]
mod tests {
    use primitive_types::{H160, H256};
    use rand::rngs::OsRng;
    use rlp::Encodable;
    use threshold_encryption::helpers::multi_keccak_256;

    use crate::signature::{public_key_to_h160, Signature};

    use super::{Committee, QuorumCertificate};

    #[test]
    fn test_qc_verification() {
        let secret_key = libsecp256k1::SecretKey::random(&mut OsRng);
        let public_key = libsecp256k1::PublicKey::from_secret_key(&secret_key);
        let authority = public_key_to_h160(&public_key);
        let committee = Committee {
            authorities: vec![authority],
        };

        let genesis_qc = QuorumCertificate::genesis();
        let content_hash: H256 = multi_keccak_256([&[0u8], &[1u8], &[2u8]].iter()).into();
        let payload_hash = multi_keccak_256([&content_hash, &genesis_qc.prev_payload_hash].iter());
        let signature = Signature::create(&payload_hash, &secret_key);
        let qc = QuorumCertificate {
            prev_payload_hash: genesis_qc.prev_payload_hash,
            content_hash,
            attestations: vec![signature],
        };

        assert!(qc.verify(&committee).is_ok());
    }

    #[test]
    fn test_qc_codec() {
        let qc = QuorumCertificate {
            attestations: vec![Signature([1u8; 65])],
            content_hash: H256::zero(),
            prev_payload_hash: H256::zero(),
        };
        let encoded = qc.rlp_bytes().to_vec();
        let decoded: QuorumCertificate = rlp::decode(&encoded).unwrap();
        assert_eq!(decoded, qc);
    }

    #[test]
    fn test_committee_codec() {
        let committee = Committee {
            authorities: vec![H160::zero()],
        };
        let encoded = committee.rlp_bytes().to_vec();
        let decoded: Committee = rlp::decode(&encoded).unwrap();
        assert_eq!(decoded, committee);
    }

    #[test]
    fn test_committee_quorum() {
        assert_eq!(
            Committee {
                authorities: vec![H160::zero()]
            }
            .quorum(),
            1
        );
        assert_eq!(
            Committee {
                authorities: vec![H160::zero(); 7]
            }
            .quorum(),
            5
        );
    }
}
