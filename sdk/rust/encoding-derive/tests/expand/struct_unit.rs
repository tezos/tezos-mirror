// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// All three derivations fail when a field has type ()

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructUnit { a : () }
