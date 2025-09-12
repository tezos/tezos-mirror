// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::hash::{
    BlsSignature, ContractTz1Hash, ContractTz2Hash, ContractTz3Hash, ContractTz4Hash,
    Ed25519Signature, P256Signature, PublicKey, PublicKeyBls, PublicKeyEd25519, PublicKeyHash,
    PublicKeyP256, PublicKeySecp256k1, Secp256k1Signature, Signature, UnknownSignature,
};
use crate::Error;
use paste::paste;
use tezos_crypto_rs::{signature, CryptoError, PublicKeySignatureVerifier, PublicKeyWithHash};

macro_rules! bind_pk_to_pkh {
    ($pk:ident, $pkh:ident) => {
        #[uniffi::export]
        impl $pk {
            #[doc = "Hashes the public key."]
            pub fn pk_hash(&self) -> $pkh {
                $pkh(self.0.pk_hash())
            }
        }
    };
}

bind_pk_to_pkh!(PublicKeyEd25519, ContractTz1Hash);
bind_pk_to_pkh!(PublicKeySecp256k1, ContractTz2Hash);
bind_pk_to_pkh!(PublicKeyP256, ContractTz3Hash);
bind_pk_to_pkh!(PublicKeyBls, ContractTz4Hash);
bind_pk_to_pkh!(PublicKey, PublicKeyHash);

macro_rules! generic_sig_conv {
    ($sig:ident) => {
        paste! {
            #[uniffi::export]
            impl $sig {
                #[doc = "Converts the `" $sig "` into a `Signature`."]
                pub fn into_generic(&self) -> Signature {
                    Signature(signature::Signature::from(self.0.clone()))
                }

                #[doc = "Converts a `Signature` into a `" $sig "`."]
                #[uniffi::constructor]
                pub fn try_from_generic(sig: &Signature) -> Result<Self, Error> {
                    sig.0
                        .clone()
                        .try_into()
                        .map(Self)
                        .map_err(|err| Error::Crypto(CryptoError::SignatureType(err)))
                }
            }

            #[uniffi::export]
            impl Signature {
                #[doc = "Converts a `" $sig "` into a `Signature`."]
                #[uniffi::constructor]
                pub fn [<from_ $sig:snake>](sig: &$sig) -> Self {
                    sig.into_generic()
                }

                #[doc = "Converts the `Signature` into a `" $sig "`."]
                pub fn [<try_into_ $sig:snake>](&self) -> Result<$sig, Error> {
                    <$sig>::try_from_generic(self)
                }
            }
        }
    };
}

generic_sig_conv!(Ed25519Signature);
generic_sig_conv!(Secp256k1Signature);
generic_sig_conv!(P256Signature);
generic_sig_conv!(BlsSignature);
generic_sig_conv!(UnknownSignature);

macro_rules! bind_verify_sig {
    ($pk:ident, $sig:ident) => {
        #[uniffi::export]
        impl $pk {
            #[doc = "Verifies that the signature has been signed by the public key."]
            pub fn verify_signature(&self, signature: &$sig, msg: &[u8]) -> Result<bool, Error> {
                self.0
                    .verify_signature(&signature.0, msg)
                    .map_err(Error::Crypto)
            }
        }
    };
}

bind_verify_sig!(PublicKeyEd25519, Ed25519Signature);
bind_verify_sig!(PublicKeySecp256k1, Secp256k1Signature);
bind_verify_sig!(PublicKeyP256, P256Signature);
bind_verify_sig!(PublicKeyBls, BlsSignature);
bind_verify_sig!(PublicKey, Signature);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::forge::forge_message;
    use tezos_crypto_rs::CryptoError;

    // All keys were generated using `octez-client`

    macro_rules! test_pk_to_pkh {
        ($name:ident, $pk_ty:ty, $($b58_pk:expr, $b58_pkh:expr),+ $(,)?) => {
            #[test]
            fn $name() {
                $(
                    let pk = <$pk_ty>::from_b58check($b58_pk).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_pk,
                        stringify!($pk_ty)
                    ));
                    let pkh = pk.pk_hash();
                    let b58_pkh = pkh.to_b58check();
                    assert_eq!(
                        b58_pkh, $b58_pkh,
                        "Derived public key hash must match the expected one"
                    );
                )+
            }
        };
    }

    test_pk_to_pkh!(
        edpk_to_tz1,
        PublicKeyEd25519,
        // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
        "edpkuyqQ99dcn3eGYPkuhj7Tm4pmXH1EiEKSitb451JoTt4a4hr8dP",
        "tz1XFq85mnnXhyhzpNEpxFvrkcuNtFBsSsVu"
    );

    test_pk_to_pkh!(
        sppk_to_tz2,
        PublicKeySecp256k1,
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        "sppk7aBZCsBJTTDTV1Lwo4eZBnqmSTSnChYF1GxHUsgeWumUCHcZyxv",
        "tz2RcdU4n2PvJHUNYkS8FPuvcnFmBqEccxb4"
    );

    test_pk_to_pkh!(
        p2pk_to_tz3,
        PublicKeyP256,
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G",
        "tz3S6P2LccJNrejt27KvJRb3BcuS5vGghkP8"
    );

    test_pk_to_pkh!(
        blpk_to_tz4,
        PublicKeyBls,
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp",
        "tz4F2fxv7sKQx9wyoRMteoJwZEZnV9WFU2wL"
    );

    test_pk_to_pkh!(
        pk_to_pkh,
        PublicKey,
        // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
        "edpkvRy8feM9jRQaPNgtQYURAjWj3qrJtXkXp6hwfkDTtXDcfB2i5F",
        "tz1SUWNMC3hUdBRzzrbTbiuGPH1KFVifTQw7",
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ",
        "tz2PbzLDYrPAZS38BteBY7gqtnZfsTqHF2xu",
        // sk: p2sk3heCRmbfiArx8so4SBevK8t7mPGRqBN8eAYTzZJPWnu6LadRbM
        "p2pk66FyiYn3WDkJ5DEQptvaPy3gBEXGr7TTMFh94pZ5p3KALfzamqi",
        "tz3hqqamVC1G22LACFoMgcJeFKZgoGMFSfSn",
        // sk: BLsk2SdiXbRuYrWkfkSDbN1tCBGjGV7tTHxjVrokaiJsv17rDd8scd
        "BLpk1wG4V4ZterYpzvidZsyrerGDWhrbkPBTUfYaT7A9KsAzapSbgynLGdCZX4VoWHKT6S33Aumf",
        "tz4Uzyxg26DJyM4pc1V2pUvLpdsR5jdyzYsZ",
    );

    macro_rules! test_into_generic_sig {
        ($name:ident, $sig_ty:ty, $($b58_sig:expr),+ $(,)?) => {
            #[test]
            fn $name() {
                $(
                    let sig = <$sig_ty>::from_b58check($b58_sig).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_sig,
                        stringify!($sig_ty)
                    ));
                    let generic_sig = sig.into_generic();
                    assert_eq!(generic_sig.to_b58check(), $b58_sig, "Value must not have changed");
                    paste! {
                        let generic_sig = Signature::[<from_ $sig_ty:snake>](&sig);
                        assert_eq!(generic_sig.to_b58check(), $b58_sig, "Value must not have changed");
                    }
                )+
            }
        };
    }

    test_into_generic_sig!(
        edsig_into_generic_sig,
        Ed25519Signature,
        // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
        // msg: "a"
        "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1"
    );

    test_into_generic_sig!(
        spsig_into_generic_sig,
        Secp256k1Signature,
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        // msg: "a"
        "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E"
    );

    test_into_generic_sig!(
        p2sig_into_generic_sig,
        P256Signature,
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
    );

    test_into_generic_sig!(
        blsig_into_generic_sig,
        BlsSignature,
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        // msg: "a"
        "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx"
    );

    test_into_generic_sig!(
        unknown_sig_into_generic_sig,
        UnknownSignature,
        // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
        // msg: "a"
        "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp"
    );

    macro_rules! test_from_generic_sig {
        ($name:ident, $sig_ty:ty, $({ $b58_sig:expr, $converted_b58_sig:expr }),+ $(,)?) => {
            #[test]
            fn $name() {
                $(
                    let generic_sig = Signature::from_b58check($b58_sig).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_sig,
                        stringify!(Signature)
                    ));
                    let sig = <$sig_ty>::try_from_generic(&generic_sig).expect(&format!(
                        "Specify signature {} into {} should succeed",
                        $b58_sig,
                        stringify!($sig_ty)
                    ));
                    assert_eq!(
                        sig.to_b58check(),
                        $converted_b58_sig,
                        "Signature must match the expected value"
                    );
                    paste! {
                        let sig = Signature::[<try_into_ $sig_ty:snake>](&generic_sig).expect(&format!(
                            "Specify signature {} into {} should succeed",
                            $b58_sig,
                            stringify!($sig_ty)
                        ));
                        assert_eq!(
                            sig.to_b58check(),
                            $converted_b58_sig,
                            "Signature must match the expected value"
                        );
                    }
                )+
            }
        };
    }

    test_from_generic_sig!(
        edsig_from_generic_sig,
        Ed25519Signature,
        {
            // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
            // msg: "a"
            "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1",
            "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1"
        },
        {
            "sigbxi1wrkVdCJSfkpfx4EwGFLmsX6zyqJhxN1YwSe6zTpQNV3zBGv1DxTVNGJcKcAWtqnqmUa4kcDjy37M2yXzHxpmqXh1V",
            "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1"
        },
        {
            // sig: [0; 64]
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq",
            "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"
        }
    );

    test_from_generic_sig!(
        spsig_from_generic_sig,
        Secp256k1Signature,
        {
            // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
            // msg: "a"
            "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E",
            "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E"
        },
        {
            // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
            // msg: "a"
            "sigmstyCA7dwXvY7UXo8mkUG47WPCybhngBYvHokpnReuZdKZwPoXK8aKp4sEZgAhxeJsnNyJ68Q4zdJyJuSw25JTjxgtVpA",
            "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E"
        },
        {
            // sig: [0; 64]
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq",
            "spsig15oyPL6RPsCmQbjdHRDQgBpnqfF1PGCaNk9eV5ksEABhY6BVRfPxGHhrV4fC84UYGJe7g1pFiyccSaBfg97KnBWCXYZh9c"
        }
    );

    test_from_generic_sig!(
        p2sig_from_generic_sig,
        P256Signature,
        {
            // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
            // msg: "a"
            "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD",
            "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        },
        {
            // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
            // msg: "a"
            "sigUkkYXApp64hzZ2CrmsSTw8Unn8GF5zSaJu15qE29hT4aEAhVCdbiCmnY3DAZMP1aAZBHjdSUfXcx5oFs84k8S4FhnDrUk",
            "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD"
        },
        {
            // sig: [0; 64]
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq",
            "p2sigMJWuMaj1zAfVMzdZzFnoncCKE7faHzJ7coB6h3ziUiGeZoTZUNfYSQR5t2dJ6cFWCvUx8CZdLRCigAUtrt2JEfRzvbDnL"
        }
    );

    test_from_generic_sig!(
        blsig_from_generic_sig,
        BlsSignature,
        {
            // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
            // msg: "a"
            "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx",
            "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx"
        }
    );

    test_from_generic_sig!(
        sig_from_generic_sig,
        UnknownSignature,
        {
            // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
            // msg: "a"
            "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp",
            "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp"
        },
        {
            // sig: [0; 64]
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq",
            "sigMzJ4GVAvXEd2RjsKGfG2H9QvqTSKCZsuB2KiHbZRGFz72XgF6KaKADznh674fQgBatxw3xdHqTtMHUZAGRprxy64wg1aq"
        },
        {
            // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
            // msg: "a"
            "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1",
            "sigbxi1wrkVdCJSfkpfx4EwGFLmsX6zyqJhxN1YwSe6zTpQNV3zBGv1DxTVNGJcKcAWtqnqmUa4kcDjy37M2yXzHxpmqXh1V"
        },
        {
            // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
            // msg: "a"
            "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E",
            "sigmstyCA7dwXvY7UXo8mkUG47WPCybhngBYvHokpnReuZdKZwPoXK8aKp4sEZgAhxeJsnNyJ68Q4zdJyJuSw25JTjxgtVpA"
        },
        {
            // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
            // msg: "a"
            "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD",
            "sigUkkYXApp64hzZ2CrmsSTw8Unn8GF5zSaJu15qE29hT4aEAhVCdbiCmnY3DAZMP1aAZBHjdSUfXcx5oFs84k8S4FhnDrUk"
        }
    );

    macro_rules! test_from_wrong_generic_sig {
        ($name:ident, $b58_sig:expr, $($sig_ty:ty),+ $(,)?) => {
            #[test]
            fn $name() {
                $(
                    let sig = Signature::from_b58check($b58_sig).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_sig,
                        stringify!(Signature)
                    ));
                    assert!(
                        matches!(
                            <$sig_ty>::try_from_generic(&sig),
                            Err(Error::Crypto(CryptoError::SignatureType(
                                signature::TryFromSignatureError::InvalidKind(_)
                            )))
                        ),
                        "Converting generic signature into the wrong kind of signature must fail with Crypto(SignatureType(InvalidKind(_)))"
                    );
                )+
            }
        };
    }

    test_from_wrong_generic_sig!(
        edsig_from_wrong_generic_sig,
        // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
        // msg: "a"
        "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1",
        Secp256k1Signature,
        P256Signature,
        BlsSignature,
    );

    test_from_wrong_generic_sig!(
        spsig_from_wrong_generic_sig,
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        // msg: "a"
        "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E",
        Ed25519Signature,
        P256Signature,
        BlsSignature,
    );

    test_from_wrong_generic_sig!(
        p2sig_from_wrong_generic_sig,
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        // msg: "a"
        "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD",
        Ed25519Signature,
        Secp256k1Signature,
        BlsSignature,
    );

    test_from_wrong_generic_sig!(
        blsig_from_wrong_generic_sig,
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        // msg: "a"
        "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx",
        Ed25519Signature,
        Secp256k1Signature,
        P256Signature,
        UnknownSignature,
    );

    test_from_wrong_generic_sig!(
        sig_from_wrong_generic_sig,
        // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
        // msg: "a"
        "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp",
        BlsSignature,
    );

    macro_rules! test_verify_sig {
        ($name:ident, $pk_ty:ty, $sig_ty:ty, $($b58_pk:expr, $b58_sig:expr, $msg:expr),+ $(,)?) => {
            #[test]
            fn $name() {
                $(
                    let pk = <$pk_ty>::from_b58check($b58_pk).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_pk,
                        stringify!($pk_ty)
                    ));
                    let sig = <$sig_ty>::from_b58check($b58_sig).expect(&format!(
                        "Deriving {} from {} should succeed",
                        $b58_sig,
                        stringify!($sig_ty)
                    ));
                    let raw_msg = forge_message($msg);
                    let res = &pk.verify_signature(&sig, &raw_msg).expect(&format!(
                        "Verifying signature {} with public key {} for message {} should succeed",
                        $b58_pk, $b58_sig, $msg
                    ));
                    assert!(res, "Signature verification must succeed");
                )+
            }
        };
    }

    test_verify_sig!(
        verify_edsig,
        PublicKeyEd25519,
        Ed25519Signature,
        // sk: edsk2stcDLHYC5N5AFowvKTvdQ86zuqQCn7QsFBoFJUgYyHL4xSaYB
        "edpkuyqQ99dcn3eGYPkuhj7Tm4pmXH1EiEKSitb451JoTt4a4hr8dP",
        "edsigtmnB915emZPLVrk7oyuRtZXrYhc2ychu5e7kHHSd7LUmiCReV5C16HCNXxt6hnnWxSKQmHFvuNZzUm5K1BKWBM2vejS4T1",
        "a"
    );

    test_verify_sig!(
        verify_spsig,
        PublicKeySecp256k1,
        Secp256k1Signature,
        // sk: spsk2YJesPtHH4swmdVdJpGXU1NLnpKiq2nicQFEtR5Eyb6i8Lju4z
        "sppk7aBZCsBJTTDTV1Lwo4eZBnqmSTSnChYF1GxHUsgeWumUCHcZyxv",
        "spsig1VhaJFmN7HW4vHUHmHKu8AjVRCzYfmRNf83caYz6EYqH4PDkaNbh5hofmEoejZmpj2cw7w9iZYDiibgRRKcWzWzrW7GA1E",
        "a"
    );

    test_verify_sig!(
        verify_p2sig,
        PublicKeyP256,
        P256Signature,
        // sk: p2sk37fQnziQeeWmZFuTDpf5Kn42BncafWsJ1wZ29yNUzNppV4eN8n
        "p2pk68MV9UsLUvtAyWjSZne2VpxFxhur9a8fUXUPY2RFkzixXDmnY5G",
        "p2sigU4yPcGNuYzkTVGy7VTyFSbGBAnVWBQrnkfrUEgTSuuM7mSUoaggwUxCqE9gnnaatnahJosNpAUwKUVBbiWwZhkbgBXncD",
        "a"
    );

    test_verify_sig!(
        verify_blsig,
        PublicKeyBls,
        BlsSignature,
        // sk: BLsk2rrqeqLp7ujt4NSCG8W6xDMFDBm6QHoTabeSQ4HjPDewaX4F6k
        "BLpk1wdBzZKshyhkdge3cXvWdTWhCWDsih8X1pbEdvjTapd1PvsESzTjMTwNWpephX8wyhshSFCp",
        "BLsigBcuqVyAuyYiMhPmcf16B8BmvhujB8DPUAmgYb94ixJ9wkraLfxzJpt2eyWMePtzuRNhRWSF4LukEv39LxAi7nGiH83ihKc9jnyhjbLc76QKTs4h1sTzQQEKR15yF9tSyU39iEsyTx",
        "a"
    );

    test_verify_sig!(
        verify_sig,
        PublicKey,
        Signature,
        // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
        "edpkvRy8feM9jRQaPNgtQYURAjWj3qrJtXkXp6hwfkDTtXDcfB2i5F",
        "edsigtxuEw21vZaZYaamHwkdCb2XXVAG45hmaHE6QjEMGdNst3WJ1UYFK5R1HY7UtBJ7ZuU1ud52LPE7YPWPYSNbRDBArr39oAv",
        "a",
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ",
        "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF",
        "a",
        // sk: p2sk3heCRmbfiArx8so4SBevK8t7mPGRqBN8eAYTzZJPWnu6LadRbM
        "p2pk66FyiYn3WDkJ5DEQptvaPy3gBEXGr7TTMFh94pZ5p3KALfzamqi",
        "p2sigrpjz56TPL47L8mxcpdE9UpK2wZxuP2ZwkqezDXDNCMdLK4TGd9gTvbe2a9M9mj5nMWV7ZwxmwYe9S4mNc8JYRwNag39cB",
        "a",
        // sk: BLsk2SdiXbRuYrWkfkSDbN1tCBGjGV7tTHxjVrokaiJsv17rDd8scd
        "BLpk1wG4V4ZterYpzvidZsyrerGDWhrbkPBTUfYaT7A9KsAzapSbgynLGdCZX4VoWHKT6S33Aumf",
        "BLsigBVeaxRnYEFZppZbbu3H7n1DWKjwjKiH693RnbnNwDw3xaF1Yk9MuLs5RWGXGtuRtpugvc2rfbvS624Qnq8vD7JV9F1pZaK3mtqvJhZZYaVAUhywoFpvzGNpYvyxuoRw3f4Es8rToJ",
        "a",
        // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
        "edpktvpansLmKrvHCS1aWkFHS6gJdSd5haH1Z74MJFAAeNDSuSgHBH",
        "sigWrzQCbre6B7VLP4kGntoQGrEBLLvc8cFPySNiDj5m2cTd4DfJG2feBLhgyjtHcTiLenwrActDgp9y6pxp3MS5m5sqVCY2",
        "a",
        // sk: spsk36wmmgfs88ffW7ujgw9wm511zhZUkM4GnsYxhHMD9ZDy1AsKRa
        "sppk7bo7kcRyjajZaAqEfqdtCNx3wgizhJPFqaEuisncbDFMgn6v4iP",
        "sigaeDzBgzkdvQRhYV2YUFpzYGE19bRe62YdZnnodZMDSXD9P97jBro2v8W2o8a1wRxfJEWJpLEbv2W1TM8jKqBdFCCuKcDp",
        "a",
        // sk: p2sk3EC67qL8iPQv6o2j14bgqKR6JaPBahXHpZFypVCGkbeMvv5H5G
        "p2pk66iANdYfcK2wd3rvvPACnHSHjN3PDFSjCkTmGY54rayykNs9Mdd",
        "sigTCLMigPjVA1ogWQP16sp6DpcAb3Wf4wkxUWDBHUa8wuixro5AVQLkWCGtBH91iYdcDLwPqW2yFs2XppXfBdbCSdbD3v9f",
        "a",
    );

    #[test]
    fn verify_wrong_ed25519_sig() {
        // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
        let b58_pk = "edpkvRy8feM9jRQaPNgtQYURAjWj3qrJtXkXp6hwfkDTtXDcfB2i5F";
        // sk: edsk4QUBg4kqJD5u5mvkwWe6qnmimoL3sAy8v2vDWEnWPbJeEcMMZp
        // msg: "a"
        let b58_sig = "edsigtggTXFpkv2N9YXNMtJe5khZN11RGuF1Cd5jB74Di673ZmSzpACB7khRFkHbXyknp9t8QsMwyiqeatm92cxPKd8qBno25fx";
        let msg = "a";
        let pk = PublicKeyEd25519::from_b58check(b58_pk)
            .expect(&format!("Converting public key {} should succeed", b58_pk));
        let sig = Ed25519Signature::from_b58check(b58_sig)
            .expect(&format!("Converting signature {} should succeed", b58_sig));
        let raw_msg = forge_message(msg);
        assert!(
            matches!(
                &pk.verify_signature(&sig, &raw_msg),
                Err(Error::Crypto(CryptoError::Ed25519(_))),
            ),
            "Verifying wrong ed25519 signature must fail with Crypto(Ed25519(_))"
        );
    }

    #[test]
    fn verify_wrong_sig() {
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        // msg: "a"
        let b58_pk = "sppk7anmrSFCPfSKbm6GsARo1JRpethThozcxipErX4QtT8CBDojnaJ";
        let b58_sig = "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF";
        let msg = "b";
        let pk = PublicKeySecp256k1::from_b58check(b58_pk)
            .expect(&format!("Converting public key {} should succeed", b58_pk));
        let sig = Secp256k1Signature::from_b58check(b58_sig)
            .expect(&format!("Converting signature {} should succeed", b58_sig));
        let raw_msg = forge_message(msg);
        let res = &pk.verify_signature(&sig, &raw_msg).expect(&format!(
            "Verifying signature {} with public key {} for message {} should succeed",
            b58_sig, b58_pk, msg
        ));
        assert!(!res, "Wrong signature must be not valid");
    }

    #[test]
    fn verify_wrong_sig_kind() {
        // sk: edsk2qWwwqoVa2XCiXeaihYVg6BfLcicR1TwC6vf63dCDYPh3qjR2g
        let b58_pk = "edpkvRy8feM9jRQaPNgtQYURAjWj3qrJtXkXp6hwfkDTtXDcfB2i5F";
        // sk: spsk1nDmRj6hETy89DfJzHnmyFicx853ShpiLHLJAbg2Qu9gYdx35n
        // msg: "a"
        let b58_sig = "spsig1DBG3ZMB5a7rwKMD4bXsxt7mD6QndfvZ6xATBAgdbajrnbohonsqYzLVQFWescq2JFF9PztcUbDaKeX89nxcXR7EYrHedF";
        let msg = "a";
        let pk = PublicKey::from_b58check(b58_pk)
            .expect(&format!("Converting public key {} should succeed", b58_pk));
        let sig = Signature::from_b58check(b58_sig)
            .expect(&format!("Converting signature {} should succeed", b58_sig));
        let raw_msg = forge_message(msg);
        assert!(
            matches!(
                &pk.verify_signature(&sig, &raw_msg),
                Err(Error::Crypto(CryptoError::SignatureType(
                    signature::TryFromSignatureError::InvalidKind(_)
                )))
            ),
            "Verifying signature of the wrong kind must fail with Crypto(SignatureType(InvalidKind(_)))"
        );
    }
}
