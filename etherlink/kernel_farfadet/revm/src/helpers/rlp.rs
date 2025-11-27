// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::primitives::{Address, U256};
use rlp::RlpStream;

pub fn append_u16_le<'a>(stream: &'a mut RlpStream, v: &u16) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes().to_vec())
}

pub fn append_u64_le<'a>(stream: &'a mut RlpStream, v: &u64) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes().to_vec())
}

pub fn append_address<'a>(
    stream: &'a mut RlpStream,
    address: &Address,
) -> &'a mut RlpStream {
    stream.append(&address.to_vec())
}

pub fn append_option_canonical<'a, T, Enc>(
    stream: &'a mut RlpStream,
    v: &Option<T>,
    append: Enc,
) where
    Enc: Fn(&'a mut RlpStream, &T) -> &'a mut RlpStream,
{
    match v {
        None => {
            stream.begin_list(0);
        }
        Some(value) => {
            stream.begin_list(1);
            append(stream, value);
        }
    }
}

pub fn append_option_u64_le(stream: &mut RlpStream, v: &Option<u64>) {
    append_option_canonical(stream, v, append_u64_le)
}

pub fn append_option_address(stream: &mut RlpStream, address: &Option<Address>) {
    append_option_canonical(stream, address, append_address)
}

pub fn append_u256_le<'a>(stream: &'a mut RlpStream, v: &U256) -> &'a mut RlpStream {
    stream.append(&v.to_le_bytes::<32>().to_vec())
}
