// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// We can specify a max size for strings

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructString {
    a : String,
    #[encoding(string = "32")]
    b : String,
}
