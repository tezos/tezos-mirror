// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::Error;
use paste::paste;
use tezos_crypto_rs::{
    hash::{self, HashTrait},
    public_key, public_key_hash, signature,
};
use tezos_protocol::contract;

macro_rules! bind_hash {
    ($bind_name:ident, $name:ty) => {
        paste! {
            #[derive(uniffi::Object, Debug, Clone, PartialEq, Eq)]
            #[uniffi::export(Debug, Display, Eq)]
            pub struct $bind_name(pub(crate) $name);

            #[uniffi::export]
            impl $bind_name {
                #[doc = "Decodes a Base58Check-encoded string into a `" $bind_name "`."]
                #[uniffi::constructor]
                pub fn from_b58check(data: &str) -> Result<Self, Error> {
                    <$name>::from_b58check(data)
                        .map(Self)
                        .map_err(Error::Base58)
                }

                #[doc = "Encodes the `" $bind_name "` into a Base58Check-encoded string."]
                pub fn to_b58check(&self) -> String {
                    <$name>::to_b58check(&self.0)
                }
            }

            impl ::std::fmt::Display for $bind_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.0)
                }
            }
        }
    };
}

bind_hash!(BlockHash, hash::BlockHash);
bind_hash!(ContractKt1Hash, hash::ContractKt1Hash);
bind_hash!(ContractTz1Hash, hash::ContractTz1Hash);
bind_hash!(ContractTz2Hash, hash::ContractTz2Hash);
bind_hash!(ContractTz3Hash, hash::ContractTz3Hash);
bind_hash!(ContractTz4Hash, hash::ContractTz4Hash);
bind_hash!(PublicKeyEd25519, hash::PublicKeyEd25519);
bind_hash!(PublicKeySecp256k1, hash::PublicKeySecp256k1);
bind_hash!(PublicKeyP256, hash::PublicKeyP256);
bind_hash!(PublicKeyBls, hash::PublicKeyBls);
bind_hash!(UnknownSignature, hash::UnknownSignature);
bind_hash!(Ed25519Signature, hash::Ed25519Signature);
bind_hash!(Secp256k1Signature, hash::Secp256k1Signature);
bind_hash!(P256Signature, hash::P256Signature);
bind_hash!(BlsSignature, hash::BlsSignature);

bind_hash!(Contract, contract::Contract);
bind_hash!(PublicKey, public_key::PublicKey);
bind_hash!(PublicKeyHash, public_key_hash::PublicKeyHash);
bind_hash!(Signature, signature::Signature);
