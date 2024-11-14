// SPDX-FileCopyrightText: 2018 POA Networks
// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Individual key pairs of the TE committee used for decryption / verification.
//!
//! Expected to be obtained as a result of a DKG (distributed key generation) ceremony.
//! For testing purposes a centralized SSS (Secret Shamir Sharing) instance is used.

use std::ops::Mul;

use blstrs::{G1Affine, G2Affine, Scalar};
use ff::Field;
use pairing_lib::group::prime::PrimeCurveAffine;
use serde::{Deserialize, Serialize};
use zeroize::Zeroize;

/// Public key share, used for authentication
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PublicKeyShare {
    /// Key holder ID as per DKG/SSS instantiation
    pub authority: u8,
    /// Public keys in G1 (minpk)
    pub share: G1Affine,
    /// Public keys in G2 (minpk)
    pub share_g2: G2Affine,
    /// Public key inverted, used for faster pairing checks
    share_inv: G1Affine,
    share_g2_inv: G2Affine,
}

/// Secret key share, used for partial decryption
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SecretKeyShare {
    /// Key holder ID as per DKG/SSS instantiation
    pub authority: u8,
    /// Secret keys are scalars
    share: Scalar,
}

impl PublicKeyShare {
    pub fn new(authority: u8, share: G1Affine, share_g2: G2Affine) -> Self {
        Self {
            authority,
            share_inv: -share,
            share,
            share_g2,
            share_g2_inv: -share_g2,
        }
    }

    pub fn inv(&self) -> &G1Affine {
        &self.share_inv
    }

    pub fn inv_g2(&self) -> &G2Affine {
        &self.share_g2_inv
    }
}

impl SecretKeyShare {
    pub fn new(authority: u8, share: Scalar) -> Self {
        Self { authority, share }
    }

    pub fn public_key_share(&self) -> PublicKeyShare {
        PublicKeyShare::new(
            self.authority,
            G1Affine::generator().mul(self.share).into(),
            G2Affine::generator().mul(self.share).into(),
        )
    }

    #[cfg(any(test, feature = "testing"))]
    pub fn random<R: rand::RngCore>(authority: u8, rng: &mut R) -> Self {
        Self::new(authority, Scalar::random(rng))
    }
}

impl Zeroize for SecretKeyShare {
    fn zeroize(&mut self) {
        self.share = Scalar::ZERO;
    }
}

impl Drop for SecretKeyShare {
    fn drop(&mut self) {
        self.zeroize();
    }
}

impl AsRef<Scalar> for SecretKeyShare {
    fn as_ref(&self) -> &Scalar {
        &self.share
    }
}

/// Secret Shamir Sharing (for testing purposes only)
#[cfg(any(test, feature = "testing"))]
pub mod sss {
    use blstrs::{G1Affine, Scalar};
    use ff::{Field, PrimeField};
    use pairing_lib::group::prime::PrimeCurveAffine;
    use rand::rngs::StdRng;
    use rand::SeedableRng;
    use std::ops::Mul;

    use super::{PublicKeyShare, SecretKeyShare};

    pub struct KeygenResult {
        pub secret_key_shares: Vec<SecretKeyShare>,
        pub public_key_shares: Vec<PublicKeyShare>,
        pub master_public_key: G1Affine,
        pub secret_key: Scalar,
    }

    pub fn keygen(count: usize, threshold: usize, seed: u64) -> KeygenResult {
        let mut rng = StdRng::seed_from_u64(seed);
        let poly_coeff: Vec<Scalar> = (0..threshold).map(|_| Scalar::random(&mut rng)).collect();

        let secret_key = poly_coeff[0];
        let master_public_key = G1Affine::generator().mul(secret_key).into();

        let evaluate = |authority: usize| {
            let mut y = Scalar::ZERO;
            let mut x = Scalar::ONE;
            // Adding +1 because f(0) is the master secret key we need to dispose
            let j = Scalar::from_u128((authority + 1) as u128);
            for coeff in poly_coeff.iter() {
                y += x * coeff;
                x *= j;
            }
            SecretKeyShare::new(authority as u8, y)
        };

        let secret_key_shares: Vec<SecretKeyShare> = (0..count).map(evaluate).collect();
        let public_key_shares = secret_key_shares
            .iter()
            .map(|sks| sks.public_key_share())
            .collect();

        KeygenResult {
            secret_key_shares,
            public_key_shares,
            master_public_key,
            secret_key,
        }
    }
}

#[cfg(test)]
mod tests {
    use blstrs::Scalar;
    use ff::Field;
    use rand::rngs::OsRng;
    use zeroize::Zeroize;

    use crate::{helpers::lagrange_coeff, key_shares::SecretKeyShare};

    use super::sss::{keygen, KeygenResult};

    #[test]
    fn test_sss_keygen() {
        let KeygenResult {
            master_public_key: _,
            public_key_shares: _,
            secret_key_shares,
            secret_key,
        } = keygen(7, 4, 1);

        let mut res = Scalar::ZERO;
        let indices: Vec<Scalar> = [1u64, 2, 3, 4].iter().map(|x| Scalar::from(*x)).collect();
        for (j, share) in secret_key_shares.iter().take(4).enumerate() {
            let l = lagrange_coeff(&indices, &Scalar::from((j + 1) as u64));
            res += l * share.as_ref();
        }

        assert_eq!(secret_key, res);
    }

    #[test]
    fn test_zeroize() {
        let zero_sk = SecretKeyShare::new(0, Scalar::ZERO);
        let mut sk = SecretKeyShare::random(0, &mut OsRng);
        assert_ne!(zero_sk, sk);
        sk.zeroize();
        assert_eq!(zero_sk, sk);
    }
}
