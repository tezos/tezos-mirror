// Copyright (c) SimpleStaking, Viable Systems, Nomadic Labs and Tezedge Contributors
// SPDX-CopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::*;
use tezos_data_encoding::enc::{BinResult, BinWriter};
use tezos_data_encoding::encoding::{Encoding, HasEncoding};
use tezos_data_encoding::nom::{NomReader, NomResult};

macro_rules! encode_hash {
    ($hash_name:ty) => {
        impl BinWriter for $hash_name {
            fn bin_write(&self, out: &mut Vec<u8>) -> BinResult {
                use tezos_data_encoding::enc::*;

                put_bytes(self.as_ref(), out);
                Ok(())
            }
        }
    };
}

encode_hash!(ChainId);
encode_hash!(BlockHash);
encode_hash!(BlockMetadataHash);
encode_hash!(BlockPayloadHash);
encode_hash!(OperationHash);
encode_hash!(OperationListListHash);
encode_hash!(OperationMetadataHash);
encode_hash!(OperationMetadataListListHash);
encode_hash!(ContextHash);
encode_hash!(ProtocolHash);
encode_hash!(ContractKt1Hash);
encode_hash!(ContractTz1Hash);
encode_hash!(ContractTz2Hash);
encode_hash!(ContractTz3Hash);
encode_hash!(ContractTz4Hash);
encode_hash!(CryptoboxPublicKeyHash);
encode_hash!(PublicKeyEd25519);
encode_hash!(PublicKeySecp256k1);
encode_hash!(PublicKeyP256);
encode_hash!(PublicKeyBls);
encode_hash!(SecretKeyEd25519);
encode_hash!(SecretKeyBls);
encode_hash!(UnknownSignature);
encode_hash!(Ed25519Signature);
encode_hash!(Secp256k1Signature);
encode_hash!(P256Signature);
encode_hash!(BlsSignature);
encode_hash!(NonceHash);
encode_hash!(SmartRollupHash);
encode_hash!(SmartRollupCommitmentHash);
encode_hash!(SmartRollupStateHash);
encode_hash!(ScriptExprHash);

macro_rules! hash_nom_reader {
    ($hash_name:ident) => {
        impl<'a> NomReader<'a> for $hash_name {
            #[inline(always)]
            fn nom_read(input: &[u8]) -> NomResult<Self> {
                use nom::{bytes::complete::take, combinator::map};

                map(take(Self::SIZE), |bytes| {
                    Self::try_from_bytes(bytes).unwrap()
                })(input)
            }
        }
    };
}

hash_nom_reader!(ChainId);
hash_nom_reader!(BlockHash);
hash_nom_reader!(BlockMetadataHash);
hash_nom_reader!(BlockPayloadHash);
hash_nom_reader!(OperationHash);
hash_nom_reader!(OperationListListHash);
hash_nom_reader!(OperationMetadataHash);
hash_nom_reader!(OperationMetadataListListHash);
hash_nom_reader!(ContextHash);
hash_nom_reader!(ProtocolHash);
hash_nom_reader!(ContractKt1Hash);
hash_nom_reader!(ContractTz1Hash);
hash_nom_reader!(ContractTz2Hash);
hash_nom_reader!(ContractTz3Hash);
hash_nom_reader!(ContractTz4Hash);
hash_nom_reader!(CryptoboxPublicKeyHash);
hash_nom_reader!(PublicKeyEd25519);
hash_nom_reader!(PublicKeySecp256k1);
hash_nom_reader!(PublicKeyP256);
hash_nom_reader!(PublicKeyBls);
hash_nom_reader!(SecretKeyEd25519);
hash_nom_reader!(SecretKeyBls);
hash_nom_reader!(UnknownSignature);
hash_nom_reader!(Ed25519Signature);
hash_nom_reader!(Secp256k1Signature);
hash_nom_reader!(P256Signature);
hash_nom_reader!(BlsSignature);
hash_nom_reader!(NonceHash);
hash_nom_reader!(SmartRollupHash);
hash_nom_reader!(SmartRollupCommitmentHash);
hash_nom_reader!(SmartRollupStateHash);
hash_nom_reader!(ScriptExprHash);

macro_rules! hash_has_encoding {
    ($hash_name:ident, $enc_ref_name:ident) => {
        impl HasEncoding for $hash_name {
            fn encoding() -> Encoding {
                Encoding::Hash($hash_name::hash_type().into())
            }
        }
    };
}

hash_has_encoding!(ChainId, CHAIN_ID);
hash_has_encoding!(BlockHash, BLOCK_HASH);
hash_has_encoding!(BlockMetadataHash, BLOCK_METADATA_HASH);
hash_has_encoding!(BlockPayloadHash, BLOCK_PAYLOAD_HASH);
hash_has_encoding!(OperationHash, OPERATION_HASH);
hash_has_encoding!(OperationListListHash, OPERATION_LIST_LIST_HASH);
hash_has_encoding!(OperationMetadataHash, OPERATION_METADATA_HASH);
hash_has_encoding!(
    OperationMetadataListListHash,
    OPERATION_METADATA_LIST_LIST_HASH
);
hash_has_encoding!(ContextHash, CONTEXT_HASH);
hash_has_encoding!(ProtocolHash, PROTOCOL_HASH);
hash_has_encoding!(ContractKt1Hash, CONTRACT_KT1HASH);
hash_has_encoding!(ContractTz1Hash, CONTRACT_TZ1HASH);
hash_has_encoding!(ContractTz2Hash, CONTRACT_TZ2HASH);
hash_has_encoding!(ContractTz3Hash, CONTRACT_TZ3HASH);
hash_has_encoding!(ContractTz4Hash, CONTRACT_TZ4HASH);
hash_has_encoding!(CryptoboxPublicKeyHash, CRYPTOBOX_PUBLIC_KEY_HASH);
hash_has_encoding!(PublicKeyEd25519, PUBLIC_KEY_ED25519);
hash_has_encoding!(PublicKeySecp256k1, PUBLIC_KEY_SECP256K1);
hash_has_encoding!(PublicKeyP256, PUBLIC_KEY_P256);
hash_has_encoding!(PublicKeyBls, PUBLIC_KEY_BLS);
hash_has_encoding!(SecretKeyEd25519, SECRET_KEY_ED25519);
hash_has_encoding!(SecretKeyBls, SECRET_KEY_BLS);
hash_has_encoding!(UnknownSignature, UNKNOWN_SIGNATURE);
hash_has_encoding!(Ed25519Signature, ED25519_SIGNATURE_HASH);
hash_has_encoding!(Secp256k1Signature, SECP256K1_SIGNATURE_HASH);
hash_has_encoding!(P256Signature, P256_SIGNATURE_HASH);
hash_has_encoding!(BlsSignature, BLS_SIGNATURE_HASH);
hash_has_encoding!(NonceHash, NONCE_HASH);
hash_has_encoding!(SmartRollupHash, SMART_ROLLUP_HASH);
hash_has_encoding!(SmartRollupCommitmentHash, SMART_ROLLUP_COMMITMENT_HASH);
hash_has_encoding!(SmartRollupStateHash, SMART_ROLLUP_STATE_HASH);
hash_has_encoding!(ScriptExprHash, SCRIPT_EXPR_HASH);
