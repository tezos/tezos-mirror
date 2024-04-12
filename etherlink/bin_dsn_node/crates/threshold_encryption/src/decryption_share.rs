// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Decryption share - partially decrypted ciphertext.
//!
//! T decryption shares are enough to decrypt the ciphertext, where T is the threshold.
//! Similarly to ciphertext, two forms are provided: partially and fully deserialized.
//!
//! The reason for this redundance is that decompressing EC points is costly (mostly because
//! of the subgroup checks) and often we can return earlier skipping processing some items,
//! which can lead to unnecessary computations. It is especially important for the kernel.

use blstrs::{Bls12, G1Affine, G1Projective, Gt, Scalar};
use pairing_lib::{group::Group, MillerLoopResult, MultiMillerLoop};
use rlp::{Decodable, DecoderError, Encodable};
use std::ops::Mul;

use crate::{
    ciphertext::UHW,
    error::ThresholdEncryptionError,
    helpers::{lagrange_coeff, rlp_decode_array, G1Compressed},
    key_shares::{PublicKeyShare, SecretKeyShare},
};

/// A tuple containing authority ID (as per DKG/SSS) and fully deserialized decryption share (in G1).
/// Ready for combining.
pub type DSH = (u8, G1Affine);

/// Partially deserialized decryption share
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecryptionShare {
    /// Key holder ID as per DKG/SSS instantiation
    pub authority: u8,
    /// Decryption share in G1 (compressed, serialized)
    pub share: G1Compressed,
}

impl DecryptionShare {
    /// Partially decrypt given ciphertext with a given secret key share
    pub fn create(ciphertext: &UHW, secret_key_share: &SecretKeyShare) -> Self {
        let (u_nonce, _, _) = ciphertext;
        let share = u_nonce.mul(secret_key_share.as_ref()).to_compressed();
        Self {
            share,
            authority: secret_key_share.authority,
        }
    }

    /// Try to deserialize decryption share without doing subgroup check (can be unsafe)
    pub fn decode_unchecked(&self) -> Option<DSH> {
        G1Affine::from_compressed_unchecked(&self.share)
            .map(|g1| (self.authority, g1))
            .into()
    }

    /// Verify decryption share given a respective ciphertext and public key share,
    /// output fully deserialized  form, ready to be combined.
    pub fn verify_decode(
        &self,
        ciphertext: &UHW,
        public_key_share: &PublicKeyShare,
    ) -> Result<DSH, ThresholdEncryptionError> {
        let dsh: G1Affine = match G1Affine::from_compressed(&self.share).into() {
            Some(g1) => g1,
            None => {
                return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                    "failed to decompress decryption share",
                ))
            }
        };

        let (_, h, w) = *ciphertext;
        let pk_inv = public_key_share.inv();

        let res = Bls12::multi_miller_loop(&[(pk_inv, &w.into()), (&dsh, &h.into())])
            .final_exponentiation();
        if res != Gt::identity() {
            return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                "product of pairings is not 1",
            ));
        }
        Ok((self.authority, dsh))
    }

    /// Verify multiple decryption shares at once and output fully deserialized form.
    /// NOTE that if verification fails it's not possible to tell which decryption share is invalid in particular.
    pub fn batch_verify_decode(
        _shares: &[DecryptionShare],
        _ciphertexts: &[UHW],
        _public_key_share: &PublicKeyShare,
    ) -> Result<Vec<DSH>, ThresholdEncryptionError> {
        // See https://hackmd.io/@m-kus/rJXTo9_pT#Verify-share
        // Also https://github.com/anoma/ferveo/blob/1022ab2c7ccc689abcc05e5a08df6fb0c2a3fc65/tpke/src/decryption.rs#L24
        todo!()
    }

    /// Combine multiple decryption shares to obtain the shared ElGamal key.
    /// NOTE that this method does not check that there is enough shares or authorities IDs
    /// are in range.
    pub fn combine(shares: &[DSH]) -> Result<G1Affine, ThresholdEncryptionError> {
        // Authorities are indexed from 0, but their shares were obtained by evaluating
        // the polynomial in points j > 0, so we need to add one to all indices.
        let (indices, points): (Vec<Scalar>, Vec<G1Projective>) = shares
            .iter()
            .map(|(index, point)| (Scalar::from(*index as u64 + 1), G1Projective::from(*point)))
            .unzip();
        let scalars: Vec<Scalar> = indices
            .iter()
            .map(|j| lagrange_coeff(&indices, j))
            .collect();
        let y = G1Projective::multi_exp(&points, &scalars);
        Ok(y.into())
    }
}

impl Encodable for DecryptionShare {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(2)
            .append(&self.authority)
            .append_iter(self.share);
    }
}

impl Decodable for DecryptionShare {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let mut it = rlp.iter();
        Ok(DecryptionShare {
            authority: it
                .next()
                .ok_or(DecoderError::RlpIncorrectListLen)?
                .as_val()?,
            share: rlp_decode_array(it.next().as_ref())?,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Mul;

    use blstrs::{G1Affine, G2Affine, Scalar};
    use ff::Field;
    use pairing_lib::group::prime::PrimeCurveAffine;
    use rand::{rngs::OsRng, RngCore};
    use rlp::Encodable;

    use crate::{
        ciphertext::UHW,
        decryption_share::{DecryptionShare, DSH},
        helpers::{elgamal_apply, hash_to_g2, Bytes32},
        key_shares::{
            sss::{keygen, KeygenResult},
            SecretKeyShare,
        },
    };

    pub fn encrypt_data<R: RngCore>(data: &Bytes32, mpk: &G1Affine, rng: &mut R) -> UHW {
        let r = Scalar::random(rng);
        let u: G1Affine = G1Affine::generator().mul(r).into();
        let y = mpk.mul(r);
        let v = elgamal_apply(data, &y.into());
        let h = hash_to_g2(&[&u.to_compressed(), &v]);
        let w: G2Affine = h.mul(r).into();
        (u, h, w)
    }

    #[test]
    fn test_combine_decryption_shares_unverified() {
        let KeygenResult {
            master_public_key,
            public_key_shares: _,
            secret_key_shares,
            secret_key: _,
        } = keygen(7, 4, 1);

        let r = Scalar::random(&mut OsRng);
        let u_nonce = G1Affine::generator().mul(r).into();
        let y = master_public_key.mul(r).into();

        let shares: Vec<DSH> = secret_key_shares
            .iter()
            .take(4)
            .map(|sks| {
                DecryptionShare::create(&(u_nonce, G2Affine::identity(), G2Affine::identity()), sks)
            })
            .map(|dsh| dsh.decode_unchecked().unwrap())
            .collect();

        let res = DecryptionShare::combine(&shares).unwrap();
        assert_eq!(res, y);
    }

    #[test]
    fn test_verify_decryption_share() {
        let secret_key_share = SecretKeyShare::random(0, &mut OsRng);
        let public_key_share = secret_key_share.public_key_share();

        let secret_key = Scalar::from(42u64);
        let mpk: G1Affine = G1Affine::generator().mul(&secret_key).into();

        let data = [1u8; 32];
        let ciphertext = encrypt_data(&data, &mpk, &mut OsRng);

        let dsh = DecryptionShare::create(&ciphertext, &secret_key_share);
        let _ = dsh.verify_decode(&ciphertext, &public_key_share).unwrap();
    }

    #[test]
    fn test_decryption_share_codec() {
        let dsh = DecryptionShare {
            authority: 1u8,
            share: [2u8; 48],
        };
        let encoded = dsh.rlp_bytes().to_vec();
        let decoded: DecryptionShare = rlp::decode(&encoded).unwrap();
        assert_eq!(decoded, dsh);
    }
}
