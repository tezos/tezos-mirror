// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Module containing helper functions for RLP encoding/decoding.

use crate::transaction::{
    TransactionHash, TransactionStatus, TransactionType, TRANSACTION_HASH_SIZE,
};
use primitive_types::{H256, U256};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpIterator, RlpStream};
use tezos_smart_rollup_encoding::timestamp::Timestamp;

pub fn next<'a, 'v>(decoder: &mut RlpIterator<'a, 'v>) -> Result<Rlp<'a>, DecoderError> {
    decoder.next().ok_or(DecoderError::RlpIncorrectListLen)
}

pub fn check_list(decoder: &Rlp<'_>, length: usize) -> Result<(), DecoderError> {
    if !decoder.is_list() {
        Err(DecoderError::RlpExpectedToBeList)
    } else if decoder.item_count() != Ok(length) {
        Err(DecoderError::RlpIncorrectListLen)
    } else {
        Ok(())
    }
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
    decode_option_explicit(decoder, field_name, decode_field)
}

// Combinator for decoding an optional field using an explicit
// decoding function, instead of the implemented in the Decodable trait.
pub fn decode_option_explicit<T, Dec>(
    decoder: &Rlp<'_>,
    field_name: &'static str,
    dec_field: Dec,
) -> Result<Option<T>, DecoderError>
where
    Dec: Fn(&Rlp<'_>, &'static str) -> Result<T, DecoderError>,
{
    if decoder.is_empty() {
        Ok(None)
    } else {
        let inner: T = dec_field(decoder, field_name)?;
        Ok(Some(inner))
    }
}

pub fn decode_list<T: Decodable>(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<Vec<T>, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decoder.as_list().map_err(custom_err)
}

pub fn decode_array(
    item: rlp::Rlp<'_>,
    size: usize,
    vec: &mut [u8],
) -> Result<(), DecoderError> {
    let list = item.data()?;
    if list.len() != size {
        return Err(DecoderError::RlpIncorrectListLen);
    }
    vec.copy_from_slice(list);
    Ok(())
}

pub fn append_option<'a, T: Encodable>(
    stream: &'a mut RlpStream,
    data: &Option<T>,
) -> &'a mut RlpStream {
    append_option_explicit(stream, data, |s, v| s.append(v))
}

// Combinator for encoding an optional value using an explicit
// encoding function, instead of the implemented in the Encodable trait.
pub fn append_option_explicit<'a, T, Enc>(
    stream: &'a mut RlpStream,
    data: &Option<T>,
    encoder: Enc,
) -> &'a mut RlpStream
where
    Enc: Fn(&'a mut RlpStream, &T) -> &'a mut RlpStream,
{
    if let Some(value) = data {
        encoder(stream, value)
    } else {
        stream.append_empty_data()
    }
}

pub fn append_vec<'a>(stream: &'a mut RlpStream, data: &Vec<u8>) -> &'a mut RlpStream {
    if data.is_empty() {
        stream.append_empty_data()
    } else {
        stream.append_iter((*data).clone())
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

pub fn decode_tx_hash(item: rlp::Rlp<'_>) -> Result<TransactionHash, DecoderError> {
    let list = item.data()?;
    if list.len() != TRANSACTION_HASH_SIZE {
        return Err(DecoderError::RlpIncorrectListLen);
    }
    let mut tx = [0u8; TRANSACTION_HASH_SIZE];
    tx.copy_from_slice(list);
    Ok(tx)
}

pub fn decode_field_u256_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<U256, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, field_name)?;
    Ok(U256::from_little_endian(&bytes))
}

pub fn append_u256_le<'a>(stream: &'a mut RlpStream, v: &U256) -> &'a mut RlpStream {
    let mut bytes = Into::<[u8; 32]>::into(*v);
    v.to_little_endian(&mut bytes);
    stream.append(&bytes.to_vec())
}

pub fn decode_field_u64_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<u64, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, field_name)?;
    let bytes_array: [u8; 8] = bytes.try_into().map_err(|_| {
        DecoderError::Custom("Invalid conversion from vector of bytes to bytes.")
    })?;
    Ok(u64::from_le_bytes(bytes_array))
}

pub fn append_u64_le<'a>(stream: &'a mut RlpStream, v: &u64) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes().to_vec())
}

pub fn decode_field_u32_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<u32, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, field_name)?;
    let bytes_array: [u8; 4] = bytes.try_into().map_err(|_| {
        DecoderError::Custom("Invalid conversion from vector of bytes to bytes.")
    })?;
    Ok(u32::from_le_bytes(bytes_array))
}

pub fn append_u32_le<'a>(stream: &'a mut RlpStream, v: &u32) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes().to_vec())
}

pub fn decode_field_u16_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<u16, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, field_name)?;
    let bytes_array: [u8; 2] = bytes
        .try_into()
        .map_err(|_| DecoderError::Custom("Field is not 2 bytes"))?;
    Ok(u16::from_le_bytes(bytes_array))
}

pub fn append_u16_le<'a>(stream: &'a mut RlpStream, v: &u16) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes().to_vec())
}

pub fn decode_transaction_hash(
    decoder: &Rlp<'_>,
) -> Result<TransactionHash, DecoderError> {
    let hash: H256 = decode_field_h256(decoder, "transaction_hash")?;
    Ok(hash.into())
}

pub fn decode_transaction_hash_list(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<Vec<TransactionHash>, DecoderError> {
    let custom_err = |_: DecoderError| (DecoderError::Custom(field_name));
    decoder
        .iter()
        .map(|rlp| decode_h256(&rlp).map(|h| h.into()))
        .collect::<Result<Vec<TransactionHash>, DecoderError>>()
        .map_err(custom_err)
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

pub trait FromRlpBytes: Decodable {
    fn from_rlp_bytes(bytes: &[u8]) -> Result<Self, DecoderError>;
}

impl<T: Decodable> FromRlpBytes for T {
    fn from_rlp_bytes(bytes: &[u8]) -> Result<Self, DecoderError> {
        let decoder = Rlp::new(bytes);
        Self::decode(&decoder)
    }
}

pub fn append_timestamp(stream: &mut RlpStream, timestamp: Timestamp) {
    stream.append(&timestamp.i64().to_le_bytes().to_vec());
}

pub fn decode_timestamp(decoder: &Rlp<'_>) -> Result<Timestamp, DecoderError> {
    let bytes: Vec<u8> = decode_field(decoder, "timestamp")?;

    let timestamp = i64::from_le_bytes(bytes.try_into().map_err(|_| {
        DecoderError::Custom(
            "Invalid conversion from timestamp vector of bytes to bytes.",
        )
    })?)
    .into();
    Ok(timestamp)
}

/// Hardcoding the option RLP encoding for u64 in little endian. This is
/// unfortunately necessary as we cannot redefine the u64 encoding.
pub fn append_option_u64_le(v: &Option<u64>, stream: &mut rlp::RlpStream) {
    match v {
        None => {
            stream.begin_list(0);
        }
        Some(value) => {
            stream.begin_list(1);
            append_u64_le(stream, value);
        }
    }
}

/// See [append_option_u64_le]
pub fn decode_option_u64_le(
    decoder: &Rlp<'_>,
    field_name: &'static str,
) -> Result<Option<u64>, DecoderError> {
    let items = decoder.item_count()?;
    match items {
        1 => {
            let mut it = decoder.iter();
            Ok(Some(decode_field_u64_le(&next(&mut it)?, field_name)?))
        }
        0 => Ok(None),
        _ => Err(DecoderError::RlpIncorrectListLen),
    }
}
