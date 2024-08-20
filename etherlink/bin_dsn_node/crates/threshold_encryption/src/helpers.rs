// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Crypto and RLP encoding helpers.

use blst::{blst_fr, blst_fr_from_scalar};
use blstrs::{G1Affine, G2Affine, G2Projective, Scalar};
use chacha20::{
    cipher::{errors::LoopError, NewCipher, StreamCipher},
    ChaCha20,
};
use ff::Field;
use rand::{CryptoRng, RngCore};
use rlp::DecoderError;
use sha3::{Digest, Keccak256};

/// We create new random key for each plaintext, so we don't really need a nonce
const CHACHA_STATIC_NONCE: &[u8; 12] = b"etherlinkftw";
/// Domain separation tag ensures different inputs in hash-to-curve are treated differently
const DOMAIN_SEPARATOR_TAG: &[u8; 27] = b"BZTE_BLS12_381_PK:G1_DSH:G1";

/// A convenient alias for 32 byte array
pub type Bytes32 = [u8; 32];
/// G1 point is serialized compressed format
pub type G1Compressed = [u8; G1Affine::compressed_size()];
/// G2 point is serialized compressed format
pub type G2Compressed = [u8; G2Affine::compressed_size()];

/// Encrypt or decrypt raw input using ChaCha20 stream cipher
pub fn chacha20_apply(input: &[u8], key: &Bytes32) -> Result<Vec<u8>, LoopError> {
    // 12 bytes nonce
    let nonce = chacha20::Nonce::from_slice(CHACHA_STATIC_NONCE);
    // Key is expected to be 32 bytes
    let mut cipher = ChaCha20::new(chacha20::Key::from_slice(key), nonce);
    let mut output = input.to_vec();
    cipher.try_apply_keystream(&mut output)?;
    Ok(output)
}

/// Encrypt or decrypt 32 byte input using El Gamal algo in the context of BZTE
pub fn elgamal_apply(input: &Bytes32, key: &G1Affine) -> Bytes32 {
    let mut ret = keccak_256(&key.to_compressed());
    for i in 0..32 {
        ret[i] ^= input[i];
    }
    ret
}

/// Hash to curve (H2C) for multiple inputs
pub fn hash_to_g2(inputs: &[&[u8]]) -> G2Affine {
    G2Projective::hash_to_curve(&inputs.concat(), DOMAIN_SEPARATOR_TAG, &[]).into()
}

pub(crate) fn random_non_zero_scalar<R: RngCore + CryptoRng>(rng: &mut R) -> Scalar {
    loop {
        let alpha = Scalar::random(&mut *rng);
        if (!alpha.is_zero()).into() {
            return alpha;
        }
    }
}

/// Calculate Keccak256 hash digest (EVM compatible)
pub fn keccak_256(data: &[u8]) -> Bytes32 {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    hasher.finalize().into()
}

/// Calculate Keccak256 hash digest for multiple values
pub fn multi_keccak_256<I, T>(items: I) -> Bytes32
where
    I: Iterator<Item = T>,
    T: AsRef<[u8]>,
{
    let mut hasher = Keccak256::new();
    for item in items {
        hasher.update(item.as_ref());
    }
    hasher.finalize().into()
}

/// Evaluate Lagrange basis polynomial at x = 0 (to obtain the free term)
///
/// Expects that the indices set contains j.
pub fn lagrange_coeff(indices: &[Scalar], j: &Scalar) -> Scalar {
    let num = indices.iter().fold(Scalar::ONE, |mut acc, x| {
        acc *= if x == j { Scalar::ONE } else { -x };
        acc
    });

    let den = indices.iter().fold(Scalar::ONE, |mut acc, x| {
        acc *= if x == j { Scalar::ONE } else { j - x };
        acc
    });

    // Mult inverse always exists because our denominator cannot become zero
    num * den.invert().unwrap()
}

pub fn rlp_decode_vec(decoder: Option<&rlp::Rlp>) -> Result<Vec<u8>, DecoderError> {
    let res = decoder
        .ok_or(DecoderError::RlpIncorrectListLen)?
        .data()?
        .to_vec();
    Ok(res)
}

pub fn rlp_decode_array<const N: usize>(
    decoder: Option<&rlp::Rlp>,
) -> Result<[u8; N], DecoderError> {
    let raw_bytes_iter = decoder.ok_or(DecoderError::RlpIncorrectListLen)?.data()?;

    if raw_bytes_iter.len() != N {
        return Err(DecoderError::RlpIncorrectListLen);
    }

    let mut raw_bytes = [0u8; N];
    raw_bytes.copy_from_slice(raw_bytes_iter);
    Ok(raw_bytes)
}

pub(crate) fn hash_to_scalar(b: &[u8]) -> Scalar {
    let dst: &[u8; 50] = b"TEZOS_THRESHOLD_ENCRYPTION_V1_BLS12_381_SCALAR_DST"; // Domain Separator Tag, unique to the application see https://datatracker.ietf.org/doc/html/rfc9380#section-3.1
    let hash = blst::blst_scalar::hash_to(b, dst).unwrap();
    let mut ret = blst_fr::default();
    unsafe {
        blst_fr_from_scalar(&mut ret, &hash);
    }
    <blstrs::Scalar as From<blst_fr>>::from(ret)
}

#[cfg(test)]
mod tests {
    use blstrs::{G1Affine, Scalar};
    use ff::Field;
    use pairing_lib::group::prime::PrimeCurveAffine;
    use rand::{distributions::Standard, Rng};

    use crate::helpers::hash_to_g2;

    use super::{chacha20_apply, elgamal_apply, lagrange_coeff};

    #[test]
    fn test_chacha_stream_cipher() {
        let input = [1u8; 281];
        let key = [2u8; 32];
        let ciphertext = chacha20_apply(&input, &key).unwrap();
        let plaintext = chacha20_apply(&ciphertext, &key).unwrap();
        assert_eq!(plaintext, input);
    }

    #[test]
    fn test_elgamal_cipher() {
        let input = [2u8; 32];
        let key = G1Affine::generator();
        let ciphertext = elgamal_apply(&input, &key);
        let plaintext = elgamal_apply(&ciphertext, &key);
        assert_eq!(plaintext, input);
    }

    #[test]
    fn test_hash_to_g2() {
        let rng = rand::thread_rng();
        let msg: Vec<u8> = rng.sample_iter(&Standard).take(1000).collect();
        let msg_end0: Vec<u8> = msg.iter().chain(b"end0").cloned().collect();
        let msg_end1: Vec<u8> = msg.iter().chain(b"end1").cloned().collect();

        assert_eq!(hash_to_g2(&[&msg]), hash_to_g2(&[&msg]));
        assert_ne!(hash_to_g2(&[&msg]), hash_to_g2(&[&msg_end0]));
        assert_ne!(hash_to_g2(&[&msg_end0]), hash_to_g2(&[&msg_end1]));
    }

    #[test]
    fn test_lagrange_coeff() {
        // f(x) = x + 1
        let x1 = Scalar::ONE;
        let x2 = Scalar::from(2u64);
        let y1 = x1 + Scalar::ONE;
        let y2 = x2 + Scalar::ONE;
        // evaluating l_j at 0
        let l1 = lagrange_coeff(&[x1, x2], &x1);
        let l2 = lagrange_coeff(&[x1, x2], &x2);
        let y0 = y1 * l1 + y2 * l2;
        assert_eq!(y0, Scalar::ONE);
    }
}
