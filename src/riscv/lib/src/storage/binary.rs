// SPDX-FileCopyrightText: 2024 Trilitech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::io::Read;
use std::io::Write;

use bincode::DefaultOptions;
use bincode::Options;

/// Constructs the default options for bincode serialisation and deserialisation.
#[inline(always)]
pub(crate) fn bincode_default() -> impl Options {
    DefaultOptions::new()
        .with_no_limit()
        .with_little_endian()
        .with_fixint_encoding()
        .reject_trailing_bytes()
}

/// Deserialise a slice of bytes into a value of type `T`.
pub(crate) fn deserialise<'de, T: serde::Deserialize<'de>>(data: &'de [u8]) -> bincode::Result<T> {
    bincode_default().deserialize(data)
}

/// Deserialise a value of type `T` from a byte source.
pub(crate) fn deserialise_from<T: serde::de::DeserializeOwned, R: Read>(
    source: R,
) -> bincode::Result<T> {
    bincode_default().deserialize_from(source)
}

/// Serialize `T` into a vector of bytes.
pub fn serialise<T: serde::Serialize>(value: &T) -> bincode::Result<Vec<u8>> {
    bincode_default().serialize(value)
}

/// Serialize `T` into a sink.
pub(crate) fn serialise_into<T: serde::Serialize, W: Write>(
    value: &T,
    sink: W,
) -> bincode::Result<()> {
    bincode_default().serialize_into(sink, value)
}
