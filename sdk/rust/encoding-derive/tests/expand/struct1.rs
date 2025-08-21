// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

#[derive(HasEncoding, NomReader, BinWriter)]
struct Struct1 { a : A }
