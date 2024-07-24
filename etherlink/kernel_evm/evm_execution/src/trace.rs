// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, H256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
#[cfg(test)]
use tezos_ethereum::rlp_helpers::{
    decode_field_u16_le, decode_field_u64_le, decode_list, decode_option_canonical,
};
use tezos_ethereum::{
    rlp_helpers::{
        append_option_canonical, append_u16_le, append_u64_le, check_list, decode_field,
        decode_option, next,
    },
    transaction::TransactionHash,
};

// For the following constant, we add +1 for the transaction hash in the input.
const STRUCT_LOGGER_CONFIG_SIZE: usize = 5;
const CALL_TRACER_CONFIG_SIZE: usize = 3;

pub const CALL_TRACER_CONFIG_PREFIX: u8 = 0x01;

#[derive(Debug, Clone, Copy)]
pub struct StructLoggerConfig {
    pub enable_memory: bool,
    pub enable_return_data: bool,
    pub disable_stack: bool,
    pub disable_storage: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct CallTracerConfig {
    pub only_top_call: bool,
    pub with_logs: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum TracerConfig {
    StructLogger(StructLoggerConfig),
    CallTracer(CallTracerConfig),
}

pub fn get_tracer_config(
    current_transaction_hash: Option<TransactionHash>,
    trace_input: &Option<TracerInput>,
) -> Option<TracerConfig> {
    if let Some(trace_input) = trace_input {
        let (transaction_hash, config) = (*trace_input).into();
        match (current_transaction_hash, transaction_hash) {
            (Some(current_transaction_hash), Some(transaction_hash))
                if transaction_hash.0 == current_transaction_hash =>
            {
                Some(config)
            }
            (None, None) | (Some(_), None) => Some(config),
            _ => None,
        }
    } else {
        None
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StructLoggerInput {
    pub transaction_hash: Option<H256>,
    pub config: StructLoggerConfig,
}

#[derive(Debug, Clone, Copy)]
pub struct CallTracerInput {
    pub transaction_hash: Option<H256>,
    pub config: CallTracerConfig,
}

#[derive(Debug, Clone, Copy)]
pub enum TracerInput {
    StructLogger(StructLoggerInput),
    CallTracer(CallTracerInput),
}

impl Decodable for StructLoggerInput {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        let mut it = decoder.iter();
        check_list(decoder, STRUCT_LOGGER_CONFIG_SIZE)?;

        let transaction_hash = decode_option(&next(&mut it)?, "transaction_hash")?;
        let enable_memory = decode_field(&next(&mut it)?, "enable_memory")?;
        let enable_return_data = decode_field(&next(&mut it)?, "enable_return_data")?;
        let disable_stack = decode_field(&next(&mut it)?, "disable_stack")?;
        let disable_storage = decode_field(&next(&mut it)?, "disable_storage")?;

        Ok(StructLoggerInput {
            transaction_hash,
            config: StructLoggerConfig {
                enable_return_data,
                enable_memory,
                disable_stack,
                disable_storage,
            },
        })
    }
}

impl Decodable for CallTracerInput {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        let mut it = decoder.iter();
        check_list(decoder, CALL_TRACER_CONFIG_SIZE)?;

        let transaction_hash = decode_option(&next(&mut it)?, "transaction_hash")?;
        let only_top_call = decode_field(&next(&mut it)?, "only_top_call")?;
        let with_logs = decode_field(&next(&mut it)?, "with_logs")?;

        Ok(CallTracerInput {
            transaction_hash,
            config: CallTracerConfig {
                only_top_call,
                with_logs,
            },
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StorageMapItem {
    pub address: H160,
    pub index: H256,
    pub value: H256,
}

impl Encodable for StorageMapItem {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(3);
        stream.append(&self.address);
        stream.append(&self.index);
        stream.append(&self.value);
    }
}

#[cfg(test)]
impl Decodable for StorageMapItem {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(3) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let address: H160 = decode_field(&next(&mut it)?, "address")?;
        let index: H256 = decode_field(&next(&mut it)?, "index")?;
        let value: H256 = decode_field(&next(&mut it)?, "value")?;

        Ok(Self {
            address,
            index,
            value,
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct StructLog {
    pub pc: u64,
    pub opcode: u8,
    pub gas: u64,
    pub gas_cost: u64,
    pub depth: u16,
    pub error: Option<Vec<u8>>,
    pub stack: Option<Vec<H256>>,
    pub return_data: Option<Vec<u8>>,
    pub memory: Option<Vec<u8>>,
    pub storage: Option<Vec<StorageMapItem>>,
}

impl Encodable for StructLog {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(10);
        append_u64_le(stream, &self.pc);
        stream.append(&self.opcode);
        append_u64_le(stream, &self.gas);
        append_u64_le(stream, &self.gas_cost);
        append_u16_le(stream, &self.depth);
        stream.append(&self.error);
        append_option_canonical(stream, &self.stack, |s, l| s.append_list(l));
        stream.append(&self.return_data);
        stream.append(&self.memory);
        append_option_canonical(stream, &self.storage, |s, l| s.append_list(l));
    }
}

#[cfg(test)]
impl Decodable for StructLog {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(10) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let pc: u64 = decode_field_u64_le(&next(&mut it)?, "pc")?;
        let opcode: u8 = decode_field(&next(&mut it)?, "opcode")?;
        let gas: u64 = decode_field_u64_le(&next(&mut it)?, "gas")?;
        let gas_cost: u64 = decode_field_u64_le(&next(&mut it)?, "gas")?;
        let depth: u16 = decode_field_u16_le(&next(&mut it)?, "depth")?;
        let error: Option<Vec<u8>> = decode_field(&next(&mut it)?, "error")?;
        let stack: Option<Vec<H256>> =
            decode_option_canonical(&next(&mut it)?, "stack", decode_list)?;
        let return_data: Option<Vec<u8>> = decode_field(&next(&mut it)?, "return_data")?;
        let memory: Option<Vec<u8>> = decode_field(&next(&mut it)?, "memory")?;
        let storage: Option<Vec<StorageMapItem>> =
            decode_option_canonical(&next(&mut it)?, "storage", decode_list)?;

        Ok(Self {
            pc,
            opcode,
            gas,
            gas_cost,
            depth,
            error,
            stack,
            return_data,
            memory,
            storage,
        })
    }
}

#[cfg(test)]
pub mod tests {
    use primitive_types::{H160, H256};

    use super::{StorageMapItem, StructLog};

    #[test]
    fn rlp_encode_decode_storage_map_item() {
        let storage_map_item = StorageMapItem {
            address: H160::from([25; 20]),
            index: H256::from([11; 32]),
            value: H256::from([97; 32]),
        };

        let encoded = rlp::encode(&storage_map_item);
        let decoded: StorageMapItem =
            rlp::decode(&encoded).expect("RLP decoding should succeed.");

        assert_eq!(storage_map_item, decoded)
    }

    #[test]
    fn rlp_encode_decode_struct_log() {
        let storage_map_item = StorageMapItem {
            address: H160::from([25; 20]),
            index: H256::from([11; 32]),
            value: H256::from([97; 32]),
        };

        let struct_log = StructLog {
            pc: 25,
            opcode: 11,
            gas: 97,
            gas_cost: 100,
            depth: 3,
            error: Some(vec![25, 11, 97]),
            stack: Some(vec![H256::from([33; 32]), H256::from([35; 32])]),
            return_data: Some(vec![25, 11, 97]),
            memory: Some(vec![25, 11, 97]),
            storage: Some(vec![storage_map_item]),
        };

        let encoded = rlp::encode(&struct_log);
        let decoded: StructLog =
            rlp::decode(&encoded).expect("RLP decoding should succeed.");

        assert_eq!(struct_log, decoded)
    }
}
