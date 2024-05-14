// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::H256;
use rlp::{Decodable, DecoderError, Rlp};
use tezos_ethereum::rlp_helpers::{check_list, decode_field, next};

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
