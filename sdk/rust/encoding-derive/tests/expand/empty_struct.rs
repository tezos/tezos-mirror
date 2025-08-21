// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// Derivation of NomReader fails for the Empty struct

#[derive(HasEncoding, BinWriter, NomReader)]
struct EmptyStruct {}
