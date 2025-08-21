// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// No constraint is generated in the where clause about the unused D generic type parameter.

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructWithGenericTypes<A, D> { a : A, b : B, c : C }
