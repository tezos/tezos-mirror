// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

#[derive(HasEncoding, NomReader, BinWriter)]
#[encoding(tags = "u8")]
enum EnumTags {
    #[encoding(tag = 0)]
    A(A),
    #[encoding(tag = 4)]
    B
}
