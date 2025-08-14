// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::hash::{ContractKt1Hash, HashType};
use tezos_data_encoding::enc::BinWriter;
use tezos_smart_rollup::types::PublicKeyHash;
use tezos_tezlink::{enc_wrappers::OperationHash, operation_result::OriginationError};

// Inspired by src/proto_alpha/lib_protocol/origination_nonce.ml
#[allow(dead_code)]
#[derive(BinWriter, Clone)]
pub struct OriginationNonce {
    pub operation: OperationHash,
    pub index: u32,
}

impl OriginationNonce {
    pub fn _initial(operation: OperationHash) -> OriginationNonce {
        OriginationNonce {
            operation,
            index: 0,
        }
    }

    /// Generate the KT1 address for a contract originated by the public key hash given in
    /// parameter
    pub fn _generate_kt1(&mut self) -> Result<ContractKt1Hash, OriginationError> {
        // First we increment the nonce
        self.index += 1;
        let address =
            mir::interpreter::compute_contract_address(&self.operation.0 .0, self.index);
        match address.hash {
            mir::ast::AddressHash::Kt1(contract_kt1_hash) => Ok(contract_kt1_hash),
            _ => Err(OriginationError::FailToGenerateKT1(
                "Expected KT1 address hash".to_string(),
            )),
        }
    }
}

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
