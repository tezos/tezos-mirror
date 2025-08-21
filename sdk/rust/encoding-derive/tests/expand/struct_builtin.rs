// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructBuiltins {
    a : i8,
    b : u8,
    c : i16,
    d : u16,
    e : i32,
    f : u32,
    g : i64,
    h : f64,
    i : bool,
}
