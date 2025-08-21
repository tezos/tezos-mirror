// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use primitive_types::U256;

pub fn hex_of_option<T>(o: &Option<T>) -> String
where
    T: AsRef<[u8]>,
{
    match o {
        Some(v) => hex::encode(v),
        None => hex::encode(""),
    }
}

pub fn bytes_of_u256(v: U256) -> Vec<u8> {
    let mut bytes = Into::<[u8; 32]>::into(v);
    v.to_little_endian(&mut bytes);
    bytes.to_vec()
}
