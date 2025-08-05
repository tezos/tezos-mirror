// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::hash::{ContractKt1Hash, HashType};
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_tezlink::operation_result::OriginationError;

/// Generate the KT1 address for a contract originated by the public key hash given in
/// parameter. For now the implementation is incorrect.
pub fn generate_kt1(pkh: &PublicKeyHash) -> Result<ContractKt1Hash, OriginationError> {
    let mut output = vec![];
    pkh.bin_write(&mut output)
        .map_err(|err| OriginationError::FailToGenerateKT1(err.to_string()))?;
    let hash_type = HashType::ContractKt1Hash;
    let kt1_str = hash_type
        .hash_to_b58check(&output[..hash_type.size()])
        .map_err(|err| OriginationError::FailToGenerateKT1(err.to_string()))?;
    ContractKt1Hash::from_base58_check(&kt1_str)
        .map_err(|err| OriginationError::FailToGenerateKT1(err.to_string()))
}
