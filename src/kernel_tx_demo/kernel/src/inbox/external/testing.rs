// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arbitrary generation of [Signer], linked to a [SecretKeyEd25519].
use crypto::hash::HashTrait;
use crypto::hash::HashType;
use crypto::hash::PublicKeyEd25519;
use crypto::hash::SecretKeyEd25519;
use crypto::hash::SeedEd25519;
use crypto::PublicKeyWithHash;
use proptest::prelude::*;

use super::Signer;

impl Signer {
    /// Generate an arbitrary `Signer`
    pub fn arb() -> BoxedStrategy<Signer> {
        (any::<bool>(), any::<[u8; HashType::SeedEd25519.size()]>())
            .prop_map(|(is_address, seed)| {
                let seed = SeedEd25519::try_from_bytes(&seed).unwrap();
                let (pk, _sk) = seed.keypair().unwrap();
                if is_address {
                    Signer::Tz1(pk.pk_hash())
                } else {
                    Signer::PublicKey(pk)
                }
            })
            .boxed()
    }

    /// Generate an arbitrary `Signer`, together with its associated [`SecretKeyEd25519`]
    pub fn arb_with_sk() -> BoxedStrategy<(Signer, SecretKeyEd25519)> {
        (any::<bool>(), any::<[u8; HashType::SeedEd25519.size()]>())
            .prop_map(|(is_address, seed)| {
                let seed = SeedEd25519::try_from_bytes(&seed).unwrap();
                let (pk, sk) = seed.keypair().unwrap();
                let signer = if is_address {
                    Signer::Tz1(pk.pk_hash())
                } else {
                    Signer::PublicKey(pk)
                };

                (signer, sk)
            })
            .boxed()
    }
}

/// Generate single keypair
pub fn gen_ed25519_keys() -> (PublicKeyEd25519, SecretKeyEd25519) {
    use rand::RngCore;

    let mut seed = [0; 32];
    rand::thread_rng().fill_bytes(&mut seed);

    let seed = tezos_crypto_rs::hash::SeedEd25519::try_from_bytes(&seed).unwrap();

    seed.keypair().unwrap()
}
