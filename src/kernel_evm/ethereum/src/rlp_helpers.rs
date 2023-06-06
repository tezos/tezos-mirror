// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Module containing helper functions for RLP encoding/decoding.

use crate::transaction::{TransactionHash, TransactionStatus, TransactionType};
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};

pub fn next<'a, 'v>(decoder: &mut RlpIterator<'a, 'v>) -> Result<Rlp<'a>, DecoderError> {
    decoder.next().ok_or(DecoderError::RlpIncorrectListLen)
}

pub fn decode_field<T: Decodable>(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<T, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decoder.as_val().map_err(custom_err)
}

pub fn decode_h256(decoder: &Rlp<'_>) -> Result<H256, DecoderError> {
    let length = decoder.data()?.len();
    if length == 32 {
        Ok(H256::from_slice(decoder.data()?))
    } else if length < 32 && length > 0 {
        // there were missing 0 that encoding deleted
        let missing = 32 - length;
        let mut full = [0u8; 32];
        full[missing..].copy_from_slice(decoder.data()?);
        Ok(H256::from(full))
    } else if decoder.data()?.is_empty() {
        // considering the case empty allows to decode unsigned transactions
        Ok(H256::zero())
    } else {
        Err(DecoderError::RlpInvalidLength)
    }
}

pub fn decode_field_h256(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<H256, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decode_h256(decoder).map_err(custom_err)
}

pub fn decode_option<T: Decodable>(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<Option<T>, DecoderError> {
    if decoder.is_empty() {
        Ok(None)
    } else {
        let addr: T = decode_field(decoder, field_name)?;
        Ok(Some(addr))
    }
}

pub fn append_option<T: Encodable>(
    stream: &mut RlpStream,
    data: Option<T>,
) -> &mut RlpStream {
    if let Some(value) = data {
        stream.append(&value)
    } else {
        stream.append_empty_data()
    }
}

pub fn append_vec(stream: &mut RlpStream, data: Vec<u8>) -> &mut RlpStream {
    if data.is_empty() {
        stream.append_empty_data()
    } else {
        stream.append_iter(data)
    }
}

pub fn append_h256(s: &mut rlp::RlpStream, h256: H256) -> &mut RlpStream {
    if H256::zero() != h256 {
        s.append(&h256)
    } else {
        // we could make the distinction between 0 and null
        // but we don't, null is encoded as 0
        // which is not such a big deal as H256 is used for hashed values
        s.append_empty_data()
    }
}

pub fn decode_field_u256_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<U256, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, field_name)?;
    Ok(U256::from_little_endian(&bytes))
}

pub fn append_u256_le(stream: &mut RlpStream, v: U256) {
    let mut bytes: [u8; 32] = v.into();
    v.to_little_endian(&mut bytes);
    stream.append(&bytes.to_vec());
}

pub fn decode_transaction_hash(
    decoder: &Rlp<'_>,
) -> Result<TransactionHash, DecoderError> {
    let hash: H256 = decode_field_h256(decoder, "transaction_hash")?;
    Ok(hash.into())
}

pub fn decode_transaction_type(
    decoder: &Rlp<'_>,
) -> Result<TransactionType, DecoderError> {
    let tag: u8 = decode_field(decoder, "transaction_type")?;
    TransactionType::try_from(&tag)
        .map_err(|_| (DecoderError::Custom("Transaction type cannot be decoded")))
}

pub fn decode_transaction_status(
    decoder: &Rlp<'_>,
) -> Result<TransactionStatus, DecoderError> {
    let tag: u8 = decode_field(decoder, "transaction_status")?;
    TransactionStatus::try_from(&tag)
        .map_err(|_| (DecoderError::Custom("Transaction status cannot be decoded")))
}
