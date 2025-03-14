// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use tezos_crypto_rs::{
    hash::{self, HashTrait},
    public_key, public_key_hash, signature, PublicKeyWithHash,
};

macro_rules! bind_hash {
    ($bind_name:ident, $name:ty) => {
        #[derive(uniffi::Object)]
        pub struct $bind_name(pub(crate) $name);

        #[uniffi::export]
        impl $bind_name {
            #[uniffi::constructor]
            pub fn from_b58check(data: &str) -> Result<Self, Error> {
                <$name>::from_b58check(data)
                    .map(Self)
                    .map_err(Error::Base58)
            }

            pub fn to_b58check(&self) -> String {
                <$name>::to_b58check(&self.0)
            }
        }
    };
}

bind_hash!(ContractTz1Hash, hash::ContractTz1Hash);
bind_hash!(ContractTz2Hash, hash::ContractTz2Hash);
bind_hash!(ContractTz3Hash, hash::ContractTz3Hash);
bind_hash!(ContractTz4Hash, hash::ContractTz4Hash);
bind_hash!(PublicKeyHash, public_key_hash::PublicKeyHash);

bind_hash!(PublicKeyEd25519, hash::PublicKeyEd25519);
bind_hash!(PublicKeySecp256k1, hash::PublicKeySecp256k1);
bind_hash!(PublicKeyP256, hash::PublicKeyP256);
bind_hash!(PublicKeyBls, hash::PublicKeyBls);
bind_hash!(PublicKey, public_key::PublicKey);

bind_hash!(UnknownSignature, hash::UnknownSignature);
bind_hash!(Ed25519Signature, hash::Ed25519Signature);
bind_hash!(Secp256k1Signature, hash::Secp256k1Signature);
bind_hash!(P256Signature, hash::P256Signature);
bind_hash!(BlsSignature, hash::BlsSignature);

// TODO: https://linear.app/tezos/issue/SDK-73.
// Unable use the `bind_hash!` macro because the `signature::Signature` struct does not implement `from_b58check` and `to_b58check` but `from_base58_check` and `to_base58_check`
/// Generic signature structure gathering the four types of signature hash and the unknown signature hash.
#[derive(uniffi::Object)]
pub struct Signature(pub(crate) signature::Signature);

#[uniffi::export]
impl Signature {
    /// Decodes any Base58Check-encoded signature string into a `Signature`.
    #[uniffi::constructor]
    pub fn from_b58check(data: &str) -> Result<Self, Error> {
        signature::Signature::from_base58_check(data)
            .map(Self)
            .map_err(Error::Base58)
    }

    /// Encodes the `Signature` into a Base58Check-encoded string.
    pub fn to_b58check(&self) -> String {
        signature::Signature::to_base58_check(&self.0)
    }
}

macro_rules! bind_pk_to_pkh {
    ($pk:ident, $pkh:ident) => {
        #[uniffi::export]
        impl $pk {
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

#[cfg(test)]
mod tests {
    use super::*;

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

    // All keys were generated using `octez-client`

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
}
