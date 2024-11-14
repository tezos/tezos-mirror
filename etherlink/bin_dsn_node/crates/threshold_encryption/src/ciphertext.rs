// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Ciphertext - the output of the threshold encryption.
//!
//! Comes in two forms:
//!     - Partially deserialized extended BZTE ciphertext
//!     - Fully deserialized BZTE ciphertext
//!
//! The reason for this redundance is that decompressing EC points is costly (mostly because
//! of the subgroup checks) and often we can return earlier skipping processing some items,
//! which can lead to unnecessary computations. It is especially important for the kernel.
//!
//! Baek-Zheng threshold cryptosystem:
//! https://cpb-us-w2.wpmucdn.com/sites.uab.edu/dist/a/68/files/2020/01/globecom03-p1491.pdf

use std::ops::Mul;

use blstrs::{Bls12, G1Affine, G2Affine, Gt};
use pairing_lib::{
    group::{prime::PrimeCurveAffine, Group},
    MillerLoopResult, MultiMillerLoop,
};
use rand::{rngs::OsRng, CryptoRng, RngCore};
use rlp::{Decodable, Encodable};

use crate::helpers::random_non_zero_scalar;
use crate::{
    error::ThresholdEncryptionError,
    helpers::{
        elgamal_apply, hash_to_g2, keccak_256, rlp_decode_array, Bytes32, G1Compressed,
        G2Compressed,
    },
};

/// BZTE ciphertext, where V (ElGamal encrypted bytes) replaced with H = hash_to_G2(U,V,AAD).
/// Used to produce and verify decryption shares.
pub type UHW = (G1Affine, G2Affine, G2Affine);

/// BZTE ciphertext extended with additionally authenticated data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ciphertext {
    /// Hash of all unencrypted transactions in the bundle [AAD]
    pub payload_hash: Bytes32,
    /// Symmetric key encrypted with TE master key [V]
    pub encrypted_key: Bytes32,
    // Hash of the unencrypted symmetric key
    // TODO: do we need it if dispute resolution is now obsolete?
    pub key_hash: Bytes32,
    /// BZTE nonce [U]
    pub u_nonce: G1Compressed,
    /// BZTE authentication tag that binds U, V, and AAD [W]
    pub w_sig: G2Compressed,
}

impl Ciphertext {
    /// Encrypt the symmetric key with the TE master key, with payload hash as AAD, using system RNG
    pub fn encrypt(
        key: &Bytes32,
        payload_hash: Bytes32,
        mpk: &G1Affine,
    ) -> Result<Ciphertext, ThresholdEncryptionError> {
        Self::encrypt_with_rng(key, payload_hash, mpk, &mut OsRng)
    }

    /// Encrypt the symmetric key with the TE master key, with payload hash as AAD, using specific RNG
    pub fn encrypt_with_rng<R: RngCore + CryptoRng>(
        key: &Bytes32,
        payload_hash: Bytes32,
        mpk: &G1Affine,
        rng: &mut R,
    ) -> Result<Ciphertext, ThresholdEncryptionError> {
        // Encrypt the key (El Gamal)
        let r = random_non_zero_scalar(rng);
        let u_nonce = G1Affine::generator().mul(r).to_compressed();
        let y = mpk.mul(r);
        let encrypted_key = elgamal_apply(key, &y.into());

        // The signature component (W) in the ciphertext binds encrypted key and transaction together,
        // preventing replay attacks. Without this trick, an attacker might censor the original encrypted
        // transaction, "rebundle" the inner transaction in its own ciphertext, and include it in the next block.
        //
        // Rebundled inner transaction would result in a different ciphertext (different keys) but
        // the same transaction payload. Unless we enforce the binding, bundler can specify arbitrary
        // transaction hash and thus make everyone think it's actually different payload.
        //
        // Another attack is when adversary keeps the encrypted key but swaps encrypted transaction with
        // some garbage. The goal is to obtain decryption shares without executing the inner transaction.
        // With tx_hash controbuting to the BLS signature it's not possible to submit different tx_hash
        // and therefore the attack would not succeed.
        let h = hash_to_g2(&[&u_nonce, &encrypted_key, &payload_hash]);
        let w_sig = h.mul(r).to_compressed();

        Ok(Ciphertext {
            key_hash: keccak_256(key),
            payload_hash,
            encrypted_key,
            u_nonce,
            w_sig,
        })
    }

    /// Decrypt symmetric key using the result of combination of decryption shares.
    /// This is basically El Gamal decryption where `y` is the shared key.
    ///
    /// Will fail if the result does not match the key hash specified in the ciphertext.
    pub fn decrypt_checked(
        &self,
        y_shared_key: &G1Affine,
    ) -> Result<Bytes32, ThresholdEncryptionError> {
        let key = elgamal_apply(&self.encrypted_key, y_shared_key);
        let key_hash = keccak_256(&key);
        if key_hash != self.key_hash {
            return Err(ThresholdEncryptionError::KeyHashMismatch {
                expected: self.key_hash.into(),
                actual: key_hash.into(),
                tx_hash: self.payload_hash.into(),
            });
        }
        Ok(key)
    }

    /// Verify ciphertext (check W auth tag) and output fully deserialized form
    pub fn verify_decode(&self) -> Result<UHW, ThresholdEncryptionError> {
        let u: G1Affine = match G1Affine::from_compressed(&self.u_nonce).into() {
            Some(g1) => g1,
            None => {
                return Err(ThresholdEncryptionError::CiphertextInvalid(
                    "failed to decompress U",
                ))
            }
        };
        let w: G2Affine = match G2Affine::from_compressed(&self.w_sig).into() {
            Some(g2) => g2,
            None => {
                return Err(ThresholdEncryptionError::CiphertextInvalid(
                    "failed to decompress W",
                ))
            }
        };

        let h = hash_to_g2(&[&self.u_nonce, &self.encrypted_key, &self.payload_hash]);
        let g_inv = -G1Affine::generator(); // TODO: lazy once?

        let res = Bls12::multi_miller_loop(&[(&g_inv, &w.into()), (&u, &h.into())])
            .final_exponentiation();
        if res != Gt::identity() {
            return Err(ThresholdEncryptionError::CiphertextInvalid(
                "product of pairings is not 1",
            ));
        }
        Ok((u, h, w))
    }

    /// Verify multiple ciphertexts at once and output fully deserialized form.
    /// NOTE that if verification fails it's not possible to tell which ciphertext is invalid in particular.
    pub fn batch_verify_decode(
        _ciphertexts: &[Ciphertext],
    ) -> Result<Vec<UHW>, ThresholdEncryptionError> {
        // See https://hackmd.io/@m-kus/rJXTo9_pT#Verify-ciphertext
        todo!()
    }
}

impl Encodable for Ciphertext {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(5)
            .append_iter(self.payload_hash)
            .append_iter(self.encrypted_key)
            .append_iter(self.key_hash)
            .append_iter(self.u_nonce)
            .append_iter(self.w_sig);
    }
}

impl Decodable for Ciphertext {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let mut it = rlp.iter();
        Ok(Ciphertext {
            payload_hash: rlp_decode_array(it.next().as_ref())?,
            encrypted_key: rlp_decode_array(it.next().as_ref())?,
            key_hash: rlp_decode_array(it.next().as_ref())?,
            u_nonce: rlp_decode_array(it.next().as_ref())?,
            w_sig: rlp_decode_array(it.next().as_ref())?,
        })
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use std::ops::Mul;

    use blstrs::{G1Affine, Scalar};
    use pairing_lib::group::prime::PrimeCurveAffine;

    use crate::helpers::{elgamal_apply, keccak_256};

    use super::Ciphertext;

    pub fn dummy_ciphertext() -> Ciphertext {
        Ciphertext {
            payload_hash: [0u8; 32],
            encrypted_key: [0u8; 32],
            key_hash: [0u8; 32],
            u_nonce: [0u8; 48],
            w_sig: [0u8; 96],
        }
    }

    #[test]
    fn test_decrypt_key() {
        let key = [1u8; 32];
        let y = G1Affine::generator().mul(Scalar::from(42u64)).into();
        let ct = Ciphertext {
            encrypted_key: elgamal_apply(&key, &y),
            key_hash: keccak_256(&key),
            ..dummy_ciphertext()
        };
        let res = ct.decrypt_checked(&y).unwrap();
        assert_eq!(key, res);
    }

    #[test]
    fn test_verify_decode() {
        let secret_key = Scalar::from(42u64);
        let mpk = G1Affine::generator().mul(&secret_key).into();
        let ct = Ciphertext::encrypt(&[0u8; 32], [1u8; 32], &mpk).unwrap();
        let _ = ct.verify_decode().unwrap();
    }

    #[test]
    fn test_ciphertext_codec() {
        let ciphertext = Ciphertext {
            payload_hash: [1u8; 32],
            encrypted_key: [2u8; 32],
            key_hash: [3u8; 32],
            u_nonce: [4u8; 48],
            w_sig: [5u8; 96],
        };
        let encoded = rlp::encode(&ciphertext);
        let decoded: Ciphertext = rlp::decode(&encoded).unwrap();
        assert_eq!(decoded, ciphertext);
    }
}
