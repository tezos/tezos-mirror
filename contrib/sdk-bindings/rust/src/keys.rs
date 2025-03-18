// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::{
    base58,
    hash::{self, HashTrait},
    public_key, public_key_hash,
};

#[derive(Debug, uniffi::Error, thiserror::Error)]
#[uniffi(flat_error)]
pub enum Error {
    #[error("Base58check conversion failure: {0:?}")]
    Base58(#[from] base58::FromBase58CheckError),
}

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
