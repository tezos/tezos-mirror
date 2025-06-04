// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// The "A : Trait" constraint is duplicated in the where clause but this is not an issue.

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructWithDuplicatedConstraint { a1 : A, a2 : A }
