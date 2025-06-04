// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_data_encoding_derive::{BinWriter, NomReader, HasEncoding};

// We can force the use of a dynamic and bounded encodings.

#[derive(HasEncoding, NomReader, BinWriter)]
struct StructComposite<A: Clone> {
    a: A,
    #[encoding(dynamic)]
    b: A,
    #[encoding(dynamic, list)]
    c: Vec<A>,
    #[encoding(dynamic, bytes)]
    d: Vec<u8>,
    #[encoding(short_dynamic, bytes)]
    e: Vec<u8>,
    #[encoding(dynamic, list, dynamic, list)]
    f: Vec<Vec<A>>,
    #[encoding(bounded = "10")]
    g: A,
    #[encoding(list = "10")]
    h: Vec<A>,
    #[encoding(dynamic = "10")]
    i: A,
    #[encoding(skip)]
    j: A,
    #[encoding(string = "10")]
    k: String,
    #[encoding(reserve = "10")]
    l: A,
}
