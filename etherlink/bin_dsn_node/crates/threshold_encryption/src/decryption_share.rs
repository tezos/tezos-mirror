// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Decryption share - partially decrypted ciphertext.
//!
//! T decryption shares are enough to decrypt the ciphertext, where T is the threshold.
//! Similarly to ciphertext, two forms are provided: partially and fully deserialized.
//!
//! The reason for this redundancy is that decompressing EC points is costly (mostly because
//! of the subgroup checks) and often we can return earlier skipping processing some items,
//! which can lead to unnecessary computations. It is especially important for the kernel.

use std::ops::Mul;

use blstrs::{Bls12, G1Affine, G1Projective, G2Affine, G2Prepared, Gt, Scalar};
use ff::Field;
use pairing_lib::group::prime::PrimeCurveAffine;
use pairing_lib::{group::Group, MillerLoopResult, MultiMillerLoop};
use rlp::{Decodable, DecoderError, Encodable};

use crate::{
    ciphertext::UHW,
    error::ThresholdEncryptionError,
    helpers,
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
    /// see https://hackmd.io/s3gtlLiNQHmFYokL600cuA#Scaling-with-the-number-of-ciphertexts
    pub fn batch_verify_decode(
        decryption_shares: &[DecryptionShare],
        ciphertexts: &[UHW],
        public_key_share: &PublicKeyShare,
    ) -> Result<Vec<DSH>, ThresholdEncryptionError> {
        if decryption_shares.is_empty() {
            return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                "size of decryption shares array should be greater or equal to 1",
            ));
        };

        if decryption_shares.len() != ciphertexts.len() {
            return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                "dimension of decryption shares does not match that of ciphertexts",
            ));
        };

        let authority = decryption_shares[0].authority;
        if !decryption_shares.iter().all(|d| d.authority == authority) {
            return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                "decryption shares must come from the same authority",
            ));
        }

        // Hashing all arguments to the pairings
        let mut pairings_arguments: Vec<u8> = decryption_shares
            .iter()
            .flat_map(|s| s.share.to_vec())
            .collect();

        pairings_arguments.extend_from_slice(&public_key_share.share.to_compressed());
        pairings_arguments.extend(
            ciphertexts
                .iter()
                .flat_map(|e| e.0.to_compressed().to_vec()),
        );
        pairings_arguments.extend_from_slice(&public_key_share.share_g2.to_compressed());
        pairings_arguments.extend_from_slice(&G1Affine::generator().to_compressed());

        let random_scalar = helpers::hash_to_scalar(pairings_arguments.as_slice());

        let mut scalar_powers = Vec::with_capacity(decryption_shares.len());
        let mut pow = Scalar::ONE;
        for _ in 0..decryption_shares.len() {
            scalar_powers.push(pow);
            pow *= random_scalar;
        }

        let mut decompressed_shares: Vec<G1Affine> = vec![];
        for share in decryption_shares {
            match G1Affine::from_compressed(&share.share).into() {
                Some(g1) => decompressed_shares.push(g1),
                None => {
                    return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                        "failed to decompress decryption share",
                    ))
                }
            }
        }

        let combined_decryption_shares = G1Projective::multi_exp(
            decompressed_shares
                .iter()
                .map(|e| e.into())
                .collect::<Vec<G1Projective>>()
                .as_slice(),
            scalar_powers.as_slice(),
        )
        .into();

        let combined_ciphertexts = G1Projective::multi_exp(
            ciphertexts
                .iter()
                .map(|e| e.0.into())
                .collect::<Vec<G1Projective>>()
                .as_slice(),
            scalar_powers.as_slice(),
        )
        .into();

        let inverted_public_key_share = public_key_share.inv_g2();

        let res = Bls12::multi_miller_loop(&[
            (&combined_decryption_shares, &G2Affine::generator().into()),
            (
                &combined_ciphertexts,
                &G2Prepared::from(*inverted_public_key_share),
            ),
        ])
        .final_exponentiation();

        if res != Gt::identity() {
            return Err(ThresholdEncryptionError::DecryptionShareInvalid(
                "product of pairings is not 1",
            ));
        }

        Ok(decompressed_shares
            .into_iter()
            .map(|s| (authority, s))
            .collect())
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
    use std::ops::{Mul, Neg};
    use std::time::Instant;

    use blstrs::{G1Affine, G2Affine, Scalar};
    use pairing_lib::group::prime::PrimeCurveAffine;
    use rand::{rngs::OsRng, CryptoRng, Rng, RngCore};
    use rlp::Encodable;

    use crate::ciphertext::Ciphertext;
    use crate::helpers::{keccak_256, random_non_zero_scalar};
    use crate::{
        ciphertext::UHW,
        decryption_share::{DecryptionShare, DSH},
        helpers::{elgamal_apply, hash_to_g2, Bytes32},
        key_shares::{
            sss::{keygen, KeygenResult},
            SecretKeyShare,
        },
    };

    pub fn encrypt_data<R: RngCore + CryptoRng>(
        data: &Bytes32,
        mpk: &G1Affine,
        rng: &mut R,
    ) -> UHW {
        let r = random_non_zero_scalar(rng);
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

        let r = random_non_zero_scalar(&mut OsRng);
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
    fn test_batched_verify_decryption_share_invalid() {
        // Key generation for 7 participants, 4 of which are required for decryption
        let KeygenResult {
            master_public_key,
            public_key_shares,
            secret_key_shares,
            secret_key: _,
        } = keygen(7, 4, 1);

        let mut rng = rand::thread_rng();
        let mut messages = vec![];
        let mut ciphertexts = vec![];
        let mut uhws = vec![];
        let num_transactions = 100;

        // Generate random messages, encrypt them, and compute their UHW
        for _ in 0..num_transactions {
            let mut message = [0u8; 32];
            rng.fill(&mut message);
            messages.push(message);

            let ciphertext =
                Ciphertext::encrypt(&message, keccak_256(&message), &master_public_key)
                    .expect("Failed to encrypt message");
            ciphertexts.push(ciphertext.clone());

            let uhw = ciphertext
                .verify_decode()
                .expect("Failed to decode ciphertext");
            uhws.push(uhw);
        }

        // Create decryption shares for each transaction
        let mut decryption_shares = vec![];
        for i in 0..num_transactions {
            let shares: Vec<DecryptionShare> = secret_key_shares
                .iter()
                .enumerate()
                .map(|(_, sk)| DecryptionShare::create(&uhws[i], sk))
                .collect();
            decryption_shares.push(shares);
        }

        let mut shares = decryption_shares
            .iter()
            .map(|s| s[0].clone())
            .collect::<Vec<DecryptionShare>>();

        // Modify one decryption share so as to make the batch verification fail
        let mut res = shares[2].clone();
        let mut e = G1Affine::from_compressed(&res.share).unwrap();
        e = e.neg();
        res.share = e.to_compressed();
        shares[2] = res;

        let valid = DecryptionShare::batch_verify_decode(
            shares.as_slice(),
            uhws.as_slice(),
            &public_key_shares[0],
        );

        assert!(valid.is_err())
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
    fn test_batched_verify_decryption_share() {
        // Key generation for 7 participants, 4 of which are required for decryption
        let KeygenResult {
            master_public_key,
            public_key_shares,
            secret_key_shares,
            secret_key: _,
        } = keygen(7, 4, 1);

        let mut rng = rand::thread_rng();
        let mut messages = vec![];
        let mut ciphertexts = vec![];
        let mut uhws = vec![];
        let num_transactions = 100;

        // Generate random messages, encrypt them, and compute their UHW
        for _ in 0..num_transactions {
            let mut message = [0u8; 32];
            rng.fill(&mut message);
            messages.push(message);

            let ciphertext =
                Ciphertext::encrypt(&message, keccak_256(&message), &master_public_key)
                    .expect("Failed to encrypt message");
            ciphertexts.push(ciphertext.clone());

            let uhw = ciphertext
                .verify_decode()
                .expect("Failed to decode ciphertext");
            uhws.push(uhw);
        }

        // Create decryption shares for each transaction
        let mut decryption_shares = vec![];
        for i in 0..num_transactions {
            let shares: Vec<DecryptionShare> = secret_key_shares
                .iter()
                .enumerate()
                .map(|(_, sk)| DecryptionShare::create(&uhws[i], sk))
                .collect();
            decryption_shares.push(shares);
        }

        // Measure the time taken to batch verify decryption shares
        let start_time = Instant::now();
        let validated_shares: Vec<Vec<DSH>> = (0..4)
            .filter_map(|i| {
                DecryptionShare::batch_verify_decode(
                    decryption_shares
                        .iter()
                        .map(|s| s[i].clone())
                        .collect::<Vec<DecryptionShare>>()
                        .as_slice(),
                    uhws.as_slice(),
                    &public_key_shares[i],
                )
                .ok()
            })
            .collect();
        let batch_verify_duration = start_time.elapsed();
        println!(
            "Batch verification for 4 keyholders elapsed time: {:.2?}",
            batch_verify_duration
        );

        // Measure the time taken to verify each decryption share individually
        let start_time = Instant::now();
        for j in 0..num_transactions {
            for i in 0..4 {
                decryption_shares[j][i]
                    .verify_decode(&uhws[j], &public_key_shares[i])
                    .expect("Failed to verify and decode decryption share");
            }
        }
        let individual_verify_duration = start_time.elapsed();
        println!(
            "Individual verification elapsed time: {:.2?}",
            individual_verify_duration
        );

        // Measure the time taken to combine and decrypt
        let start_time = Instant::now();
        for j in 0..num_transactions {
            let combined_key = DecryptionShare::combine(
                &validated_shares
                    .iter()
                    .map(|s| s[j])
                    .collect::<Vec<_>>()
                    .as_slice(),
            )
            .expect("Failed to combine decryption shares");

            assert_eq!(
                ciphertexts[j]
                    .decrypt_checked(&combined_key)
                    .expect("Failed to decrypt ciphertext"),
                messages[j]
            );
        }
        let combine_decrypt_duration = start_time.elapsed();
        println!(
            "Combine and decryption for {} transactions elapsed time: {:.2?}",
            num_transactions, combine_decrypt_duration
        );
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
