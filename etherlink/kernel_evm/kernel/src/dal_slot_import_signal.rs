// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use rlp::{Decodable, DecoderError, Encodable};
use tezos_crypto_rs::hash::UnknownSignature;
use tezos_ethereum::rlp_helpers::{self, append_u32_le, decode_field_u32_le};

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotIndicesList(pub Vec<u8>);

impl Encodable for DalSlotIndicesList {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(self.0.len());
        for slot in &self.0 {
            stream.append(slot);
        }
    }
}

impl Decodable for DalSlotIndicesList {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_is_list(decoder)?;
        let slot_indices: Vec<u8> = rlp_helpers::decode_list(decoder, "slot_indices")?;
        Ok(DalSlotIndicesList(slot_indices))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotIndicesOfLevel {
    pub published_level: u32,
    pub slot_indices: DalSlotIndicesList,
}

impl Encodable for DalSlotIndicesOfLevel {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        append_u32_le(stream, &self.published_level);
        stream.append(&self.slot_indices);
    }
}

impl Decodable for DalSlotIndicesOfLevel {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 2)?;
        let mut it = decoder.iter();
        let published_level: u32 =
            decode_field_u32_le(&rlp_helpers::next(&mut it)?, "published_level")?;
        let slot_indices =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "slot_indices")?;
        Ok(DalSlotIndicesOfLevel {
            published_level,
            slot_indices,
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnsignedDalSlotSignals(pub Vec<DalSlotIndicesOfLevel>);

impl Encodable for UnsignedDalSlotSignals {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(self.0.len());
        for slots_with_level in &self.0 {
            stream.append(slots_with_level);
        }
    }
}

impl Decodable for UnsignedDalSlotSignals {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_is_list(decoder)?;
        let levels_with_slots: Vec<DalSlotIndicesOfLevel> =
            rlp_helpers::decode_list(decoder, "unsigned_dal_slots_signals")?;
        Ok(UnsignedDalSlotSignals(levels_with_slots))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct DalSlotImportSignals {
    pub signals: UnsignedDalSlotSignals,
    pub signature: UnknownSignature,
}

impl Encodable for DalSlotImportSignals {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        stream.begin_list(2);
        stream.append(&self.signals);
        stream.append(&self.signature.as_ref());
    }
}

impl Decodable for DalSlotImportSignals {
    fn decode(decoder: &rlp::Rlp) -> Result<Self, DecoderError> {
        rlp_helpers::check_list(decoder, 2)?;
        let mut it = decoder.iter();
        let signals =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "unsigned_signals")?;
        let signature_bytes: Vec<u8> =
            rlp_helpers::decode_field(&rlp_helpers::next(&mut it)?, "signature")?;
        let signature = UnknownSignature::try_from(signature_bytes.as_slice())
            .map_err(|_| DecoderError::Custom("Invalid signature encoding"))?;
        Ok(DalSlotImportSignals { signals, signature })
    }
}
