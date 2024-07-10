// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_ethereum::rlp_helpers::{self, append_u32_le, decode_field_u32_le};

#[derive(PartialEq, Debug, Clone)]
pub struct UnsignedDalSlotImportSignal {
    pub slot_index: u8,
    pub published_level: u32,
}

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotImportSignal {
    pub signal: UnsignedDalSlotImportSignal,
    pub signature: UnknownSignature,
}

impl From<&DalSlotImportSignal> for UnsignedDalSlotImportSignal {
    fn from(val: &DalSlotImportSignal) -> UnsignedDalSlotImportSignal {
        val.signal.clone()
    }
}

impl Encodable for UnsignedDalSlotImportSignal {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        let slot_index = self.slot_index;
        let published_level = self.published_level;
        stream.append(&slot_index);
        append_u32_le(stream, &published_level);
    }
}

impl Encodable for DalSlotImportSignal {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(3);
        let slot_index = self.signal.slot_index;
        let published_level = self.signal.published_level;
        stream.append(&slot_index);
        append_u32_le(stream, &published_level);
        stream.append(&self.signature.as_ref());
    }
}

impl Decodable for DalSlotImportSignal {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 3)?;
        let mut it = decoder.iter();
        let slot_index =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "slot_index")?;
        let published_level =
            decode_field_u32_le(&rlp_helpers::next(&mut it)?, "published_level")?;
        let bytes: Vec<u8> =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
        let signature = UnknownSignature::try_from(bytes.as_slice())
            .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
        let signal = UnsignedDalSlotImportSignal {
            slot_index,
            published_level,
        };
        Ok(Self { signal, signature })
    }
}
