// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_data_encoding::enc::BinWriter;
use tezos_tezlink::enc_wrappers::OperationHash;

// Inspired by src/proto_alpha/lib_protocol/origination_nonce.ml
#[derive(BinWriter, Clone)]
pub struct OriginationNonce {
    pub operation: OperationHash,
    pub index: u32,
}

impl OriginationNonce {
    pub fn initial(operation: OperationHash) -> OriginationNonce {
        OriginationNonce {
            operation,
            index: 0,
        }
    }

    /// Generate the KT1 address for a contract originated by the public key hash given in
    /// parameter
    pub fn generate_kt1(&mut self) -> ContractKt1Hash {
        // First we increment the nonce
        self.index += 1;
        mir::interpreter::compute_contract_address(&self.operation.0 .0, self.index)
    }
}
