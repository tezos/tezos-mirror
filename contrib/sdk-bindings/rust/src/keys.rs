// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use tezos_crypto_rs::{
    hash::{self, HashTrait},
    public_key, public_key_hash, signature, PublicKeySignatureVerifier, PublicKeyWithHash,
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

macro_rules! bind_verify_sig {
    ($pk:ident, $sig:ident) => {
        #[uniffi::export]
        impl $pk {
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
                    let raw_msg = forge_message($msg)
                        .expect(&format!("Forging message {} should succeed", $msg));
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
        let raw_msg = forge_message(msg).expect(&format!("Forging message {} should succeed", msg));
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
        let raw_msg = forge_message(msg).expect(&format!("Forging message {} should succeed", msg));
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
        let raw_msg = forge_message(msg).expect(&format!("Forging message {} should succeed", msg));
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
