// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
#[cfg(test)]
use tezos_ethereum::rlp_helpers::{
    decode_field_u16_le, decode_field_u256_le, decode_field_u64_le, decode_list,
    decode_option_canonical,
};

use tezos_ethereum::{
    rlp_helpers::{
        append_option_canonical, append_u16_le, append_u256_le, append_u64_le,
        check_list, decode_field, decode_option, next,
    },
    Log,
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

impl TracerInput {
    pub fn tx_hash(&self) -> Option<H256> {
        match self {
            TracerInput::StructLogger(input) => input.transaction_hash,
            TracerInput::CallTracer(input) => input.transaction_hash,
        }
    }
}

pub fn get_tracer_configuration(
    tx_hash_target: H256,
    tracer_input: Option<TracerInput>,
) -> Option<TracerInput> {
    match tracer_input {
        Some(tracer_input) => match tracer_input.tx_hash() {
            None => {
                // If there is no transaction hash, we still provide
                // the configuration to trace all transactions
                Some(tracer_input)
            }
            Some(input_hash) => {
                // If there is a transaction hash in the input
                // we only trace if the current transaction hash
                // matches the transaction hash from the input
                if input_hash == tx_hash_target {
                    Some(tracer_input)
                } else {
                    None
                }
            }
        },
        None => None,
    }
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

#[derive(PartialEq, Debug, Clone)]
pub struct CallTrace {
    pub type_: Vec<u8>,
    pub from: H160,
    pub to: Option<H160>, // None if type is CREATE / CREATE2
    pub value: U256,
    pub gas: Option<u64>, // None if no gas limit was provided
    pub gas_used: u64,
    pub input: Vec<u8>,
    pub output: Option<Vec<u8>>, // this output will also be used in revert reason, if there's any
    pub error: Option<Vec<u8>>,
    pub logs: Option<Vec<Log>>,
    pub depth: u16, // will be helpful to reconstruct the tree of call on the node's side
}

impl CallTrace {
    pub fn new_minimal_trace(
        type_: Vec<u8>,
        from: H160,
        value: U256,
        gas_used: u64,
        input: Vec<u8>,
        depth: u16,
    ) -> Self {
        Self {
            type_,
            from,
            to: None,
            value,
            gas: None,
            gas_used,
            input,
            output: None,
            error: None,
            logs: None,
            depth,
        }
    }

    pub fn add_to(&mut self, to: Option<H160>) {
        *self = Self { to, ..self.clone() };
    }

    pub fn add_gas(&mut self, gas: Option<u64>) {
        *self = Self {
            gas,
            ..self.clone()
        };
    }

    pub fn add_output(&mut self, output: Option<Vec<u8>>) {
        *self = Self {
            output,
            ..self.clone()
        };
    }

    pub fn add_error(&mut self, error: Option<Vec<u8>>) {
        *self = Self {
            error,
            ..self.clone()
        };
    }

    pub fn add_logs(&mut self, logs: Option<Vec<Log>>) {
        *self = Self {
            logs,
            ..self.clone()
        };
    }
}

impl Encodable for CallTrace {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(11);
        stream.append(&self.type_);
        stream.append(&self.from);
        stream.append(&self.to);
        append_u256_le(stream, &self.value);
        append_option_canonical(stream, &self.gas, append_u64_le);
        append_u64_le(stream, &self.gas_used);
        stream.append(&self.input);
        stream.append(&self.output);
        stream.append(&self.error);
        append_option_canonical(stream, &self.logs, |s, logs| s.append_list(logs));
        append_u16_le(stream, &self.depth);
    }
}

#[cfg(test)]
impl Decodable for CallTrace {
    fn decode(decoder: &Rlp<'_>) -> Result<Self, DecoderError> {
        if !decoder.is_list() {
            return Err(DecoderError::RlpExpectedToBeList);
        }
        if Ok(11) != decoder.item_count() {
            return Err(DecoderError::RlpIncorrectListLen);
        }

        let mut it = decoder.iter();
        let type_ = decode_field(&next(&mut it)?, "type_")?;
        let from = decode_field(&next(&mut it)?, "from")?;
        let to = decode_field(&next(&mut it)?, "to")?;
        let value = decode_field_u256_le(&next(&mut it)?, "value")?;
        let gas = decode_option_canonical(&next(&mut it)?, "gas", decode_field_u64_le)?;
        let gas_used = decode_field_u64_le(&next(&mut it)?, "gas_used")?;
        let input = decode_field(&next(&mut it)?, "input")?;
        let output = decode_field(&next(&mut it)?, "output")?;
        let error = decode_field(&next(&mut it)?, "error")?;
        let logs = decode_option_canonical(&next(&mut it)?, "logs", decode_list)?;
        let depth = decode_field_u16_le(&next(&mut it)?, "depth")?;

        Ok(Self {
            type_,
            from,
            to,
            value,
            gas,
            gas_used,
            input,
            output,
            error,
            logs,
            depth,
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

impl StructLog {
    #[allow(clippy::too_many_arguments)]
    pub fn prepare(
        pc: u64,
        opcode: u8,
        gas: u64,
        depth: u16,
        stack: Option<Vec<H256>>,
        return_data: Option<Vec<u8>>,
        memory: Option<Vec<u8>>,
        storage: Option<Vec<StorageMapItem>>,
    ) -> Self {
        StructLog {
            pc,
            opcode,
            gas,
            gas_cost: 0,
            depth,
            error: None,
            stack,
            return_data,
            memory,
            storage,
        }
    }

    pub fn finish(self, gas_cost: u64, error: Option<Vec<u8>>) -> Self {
        StructLog {
            gas_cost,
            error,
            ..self
        }
    }
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
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, H256, U256};
    use tezos_ethereum::Log;

    use super::{CallTrace, StorageMapItem, StructLog};

    #[test]
    fn rlp_encode_decode_call_trace() {
        let logs = Log {
            address: H160::from([25; 20]),
            topics: vec![H256::from([25; 32]), H256::from([13; 32])],
            data: vec![0x00, 0x01, 0x02],
        };

        let call_trace = CallTrace {
            type_: "CALL".into(),
            from: H160::from([25; 20]),
            to: Some(H160::from([25; 20])),
            value: U256::from(251197),
            gas: Some(5000),
            gas_used: 5000,
            input: vec![0x00, 0x01, 0x02],
            output: Some(vec![0x00, 0x01, 0x02]),
            error: Some(vec![0x00, 0x01, 0x02]),
            logs: Some(vec![logs]),
            depth: 2,
        };

        let encoded = rlp::encode(&call_trace);
        let decoded: CallTrace =
            rlp::decode(&encoded).expect("RLP decoding should succeed.");

        assert_eq!(call_trace, decoded);

        let call_trace_none = CallTrace {
            type_: "CALL".into(),
            from: H160::from([25; 20]),
            to: None,
            value: U256::from(251197),
            gas: None,
            gas_used: 5000,
            input: vec![0x00, 0x01, 0x02],
            output: None,
            error: None,
            logs: None,
            depth: 2,
        };

        let encoded = rlp::encode(&call_trace_none);
        let decoded: CallTrace =
            rlp::decode(&encoded).expect("RLP decoding should succeed.");

        assert_eq!(call_trace_none, decoded)
    }

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
