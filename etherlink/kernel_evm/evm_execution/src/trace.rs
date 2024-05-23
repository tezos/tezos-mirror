// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::{H160, H256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use tezos_ethereum::rlp_helpers::{
    append_u16_le, append_u64_le, check_list, decode_field, next,
};

#[derive(Debug, Clone, Copy)]
pub struct TracerConfig {
    pub enable_memory: bool,
    pub enable_return_data: bool,
    pub disable_stack: bool,
    pub disable_storage: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct TracerInput {
    pub transaction_hash: H256,
    pub config: TracerConfig,
}

impl Decodable for TracerInput {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        check_list(decoder, 5)?;

        let mut it = decoder.iter();
        let transaction_hash = decode_field(&next(&mut it)?, "transaction_hash")?;
        let enable_memory = decode_field(&next(&mut it)?, "enable_memory")?;
        let enable_return_data = decode_field(&next(&mut it)?, "enable_return_data")?;
        let disable_stack = decode_field(&next(&mut it)?, "disable_stack")?;
        let disable_storage = decode_field(&next(&mut it)?, "disable_storage")?;

        Ok(TracerInput {
            transaction_hash,
            config: TracerConfig {
                enable_return_data,
                enable_memory,
                disable_stack,
                disable_storage,
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

#[derive(PartialEq, Debug)]
pub struct StructLog {
    pub pc: u64,
    pub opcode: u8,
    pub gas: u64,
    pub gas_cost: u64,
    pub depth: u16,
    pub error: Vec<u8>,
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
        match &self.stack {
            Some(stack) => stream.append_list(stack),
            None => stream.append_empty_data(),
        };
        match &self.return_data {
            Some(return_data) => stream.append(return_data),
            None => stream.append_empty_data(),
        };
        match &self.memory {
            Some(memory) => stream.append(memory),
            None => stream.append_empty_data(),
        };
        match &self.storage {
            Some(storage) => stream.append_list(storage),
            None => stream.append_empty_data(),
        };
    }
}
