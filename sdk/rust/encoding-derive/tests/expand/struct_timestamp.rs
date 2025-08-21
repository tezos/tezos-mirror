// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// We can mark i64 fields as being timestamps, which is visible in the
// derived implementation of HasEncoding but changes nothing for
// NomReader and BinWriter.

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructTimestamp {
    a : i64,
    #[encoding(timestamp)]
    b : i64,
}
