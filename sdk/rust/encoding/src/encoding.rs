// Copyright (c) SimpleStaking, Viable Systems, Nomadic Labs and Tezedge Contributors
// SPDX-CopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-License-Identifier: MIT

//! Schema used for serialization and deserialization.

use std::collections::HashMap;

pub use tezos_data_encoding_derive::HasEncoding;

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    encoding: Encoding,
}

impl Field {
    pub fn new(name: &str, encoding: Encoding) -> Field {
        Field {
            name: String::from(name),
            encoding,
        }
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_encoding(&self) -> &Encoding {
        &self.encoding
    }
}

pub type Schema = Vec<Field>;

#[derive(Debug, Clone)]
pub struct Tag {
    id: u16,
    variant: String,
    encoding: Encoding,
}

impl Tag {
    pub fn new(id: u16, variant: &str, encoding: Encoding) -> Tag {
        Tag {
            id,
            variant: String::from(variant),
            encoding,
        }
    }

    pub fn get_id(&self) -> u16 {
        self.id
    }

    pub fn get_encoding(&self) -> &Encoding {
        &self.encoding
    }

    pub fn get_variant(&self) -> &String {
        &self.variant
    }
}

#[derive(Debug, Clone)]
pub struct TagMap {
    id_to_tag: HashMap<u16, Tag>,
    variant_to_id: HashMap<String, u16>,
}

impl TagMap {
    pub fn new(tags: Vec<Tag>) -> TagMap {
        let mut id_to_tag = HashMap::new();
        let mut variant_to_id = HashMap::new();

        for tag in tags {
            let tag_id = tag.get_id();
            let variant = tag.get_variant().to_string();
            let prev_item = id_to_tag.insert(tag_id, tag);
            debug_assert!(
                prev_item.is_none(),
                "Tag id: 0x{:X} is already present in TagMap",
                tag_id
            );
            variant_to_id.insert(variant, tag_id);
        }

        TagMap {
            id_to_tag,
            variant_to_id,
        }
    }

    pub fn find_by_id(&self, id: u16) -> Option<&Tag> {
        self.id_to_tag.get(&id)
    }

    pub fn find_by_variant(&self, variant: &str) -> Option<&Tag> {
        self.variant_to_id
            .get(variant)
            .and_then(|tag_id| self.id_to_tag.get(tag_id))
    }

    pub fn tags(&self) -> impl Iterator<Item = &Tag> {
        self.id_to_tag.values()
    }
}

/// Represents schema used for encoding a data into a json or a binary form.
#[derive(Debug, Clone)]
pub enum Encoding {
    /// Encoded as nothing in binary or null in json
    Unit,
    /// Signed 8 bit integer (data is encoded as a byte in binary and an integer in JSON).
    Int8,
    /// Unsigned 8 bit integer (data is encoded as a byte in binary and an integer in JSON).
    Uint8,
    /// Signed 16 bit integer (data is encoded as a short in binary and an integer in JSON).
    Int16,
    /// Unsigned 16 bit integer (data is encoded as a short in binary and an integer in JSON).
    Uint16,
    /// Signed 31 bit integer, which corresponds to type int on 32-bit OCaml systems (data is encoded as a 32 bit int in binary and an integer in JSON).
    Int31,
    /// Signed 32 bit integer (data is encoded as a 32-bit int in binary and an integer in JSON).
    Int32,
    /// Unsigned 32 bit integer (data is encoded as a 32-bit int in binary and an integer in JSON).
    Uint32,
    /// Signed 64 bit integer (data is encoded as a 64-bit int in binary and a decimal string in JSON).
    Int64,
    /// Integer with bounds in a given range. Both bounds are inclusive.
    RangedInt,
    ///  Big number
    ///  In JSON, data is encoded as a decimal string.
    ///  In binary, data is encoded as a variable length sequence of
    ///  bytes, with a running unary size bit: the most significant bit of
    ///  each byte tells is this is the last byte in the sequence (0) or if
    /// there is more to read (1). The second most significant bit of the
    /// first byte is reserved for the sign (positive if zero). Binary_size and
    /// sign bits ignored, data is then the binary representation of the
    /// absolute value of the number in little-endian order.
    Z,
    /// Big number
    /// Almost identical to [Encoding::Z], but does not contain the sign bit in the second most
    /// significant bit of the first byte
    N,
    /// Encoding of floating point number (encoded as a floating point number in JSON and a double in binary).
    Float,
    /// Float with bounds in a given range. Both bounds are inclusive.
    RangedFloat,
    /// Encoding of a boolean (data is encoded as a byte in binary and a boolean in JSON).
    Bool,
    /// Encoding of a string
    /// - encoded as a byte sequence in binary prefixed by the length of the string
    /// - encoded as a string in JSON.
    String,
    /// Encoding of a string
    /// - encoded as a byte sequence in binary prefixed by the length of the string
    /// - encoded as a string in JSON.
    BoundedString(usize),
    /// Encoding of arbitrary sized bytes (encoded via hex in JSON and directly as a sequence byte in binary).
    Bytes,
    /// Tag is prefixed by tag id and followed by encoded bytes
    /// First argument represents size of the tag marker in bytes.
    Tags(usize, TagMap),
    /// List combinator. It's behavior is similar to [Encoding::Greedy] encoding. Main distinction
    /// is that we are expecting list of items instead of a single item to be contained in binary data.
    /// - encoded as an array in JSON
    /// - encoded as the concatenation of all the element in binary
    List(Box<Encoding>),
    /// Encode enumeration via association list
    ///  - represented as a string in JSON and
    ///  - represented as an integer representing the element's position in the list in binary. The integer size depends on the list size.
    BoundedList(usize, Box<Encoding>),
    /// Encode enumeration via association list
    ///  - represented as a string in JSON and
    ///  - represented as an integer representing the element's position in the list in binary. The integer size depends on the list size.
    Enum,
    /// Combinator to make an optional value
    /// (represented as a 1-byte tag followed by the data (or nothing) in binary
    ///  and either the raw value or an empty object in JSON).
    ///
    /// Compatible with ocaml usage: (req "arg" (Data_encoding.option string))
    Option(Box<Encoding>),
    /// TE-172 - combinator to make an optional field, this is not the same as Encoding::Option
    /// (req "arg" (Data_encoding.option string)) is not the same as (opt "arg" string))
    ///
    /// Compatible with ocaml usage: (opt "arg" string)
    OptionalField(Box<Encoding>),
    /// Is the collection of fields.
    /// not prefixed by anything in binary, encoded as the concatenation of all the element in binary
    Obj(&'static str, Schema),
    /// Heterogeneous collection of values.
    /// Similar to [Encoding::Obj], but schema can be any types, not just fields.
    /// Encoded as continuous binary representation.
    Tup(Vec<Encoding>),
    /// Is the collection of fields.
    /// prefixed its length in bytes (1 Bytes), encoded as the concatenation of all the element in binary
    ShortDynamic(Box<Encoding>),
    /// Is the collection of fields.
    /// prefixed its length in bytes (4 Byte), encoded as the concatenation of all the element in binary
    Dynamic(Box<Encoding>),
    /// Is the collection of fields.
    /// prefixed its length in bytes (4 Bytes), encoded as the concatenation of all the element in binary
    BoundedDynamic(usize, Box<Encoding>),
    /// Represents fixed size block in binary encoding.
    Sized(usize, Box<Encoding>),
    /// Represents bounded block in binary encoding
    /// (one with a length that cannot exceed the upper value).
    Bounded(usize, Box<Encoding>),
    /// Almost same as [Encoding::Dynamic] but without bytes size information prefix.
    /// It assumes that encoding passed as argument will process rest of the available data.
    Greedy(Box<Encoding>),
    /// Decode various types of hashes. Hash has it's own predefined length and prefix.
    /// This is controller by a hash implementation.
    Hash(&'static str),
    /// Timestamp encoding.
    /// - encoded as RFC 3339 in json
    /// - encoded as [Encoding::Int64] in binary
    Timestamp,
    /// This is used to perform encoding using custom function
    /// rather than basing on schema. Used to get rid of recursion
    /// while encoding/decoding recursive types.
    Custom,
}

impl Encoding {
    /// Utility function to construct [Encoding::List] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn list(encoding: Encoding) -> Encoding {
        Encoding::List(Box::new(encoding))
    }

    /// Utility function to construct [Encoding::BoundedList] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn bounded_list(max: usize, encoding: Encoding) -> Encoding {
        Encoding::BoundedList(max, Box::new(encoding))
    }

    /// Utility function to construct [Encoding::Sized] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn sized(bytes_sz: usize, encoding: Encoding) -> Encoding {
        Encoding::Sized(bytes_sz, Box::new(encoding))
    }

    /// Utility function to construct [Encoding::Bounded] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn bounded(max: usize, encoding: Encoding) -> Encoding {
        Encoding::Bounded(max, Box::new(encoding))
    }

    /// Utility function to construct [Encoding::Greedy] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn greedy(encoding: Encoding) -> Encoding {
        Encoding::Greedy(Box::new(encoding))
    }

    /// Utility function to construct [Encoding::ShortDynamic] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn short_dynamic(encoding: Encoding) -> Encoding {
        Encoding::ShortDynamic(Box::new(encoding))
    }

    /// Utility function to construct [Encoding::Dynamic] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn dynamic(encoding: Encoding) -> Encoding {
        Encoding::Dynamic(Box::new(encoding))
    }

    /// Utility function to construct [Encoding::BoundedDynamic] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn bounded_dynamic(max: usize, encoding: Encoding) -> Encoding {
        Encoding::BoundedDynamic(max, Box::new(encoding))
    }

    /// Utility function to construct [Encoding::Option] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn option(encoding: Encoding) -> Encoding {
        Encoding::Option(Box::new(encoding))
    }

    /// Utility function to construct [Encoding::OptionalField] without the need
    /// to manually create new [Box].
    #[inline]
    pub fn option_field(encoding: Encoding) -> Encoding {
        Encoding::OptionalField(Box::new(encoding))
    }
}

/// Indicates that type has its own ser/de schema.
pub trait HasEncoding {
    fn encoding() -> Encoding;
}

/// Creates impl HasEncoding for given struct backed by lazy_static ref instance with encoding.
#[macro_export]
macro_rules! has_encoding {
    ($struct_name:ident, $enc_ref_name:ident, $code:block) => {
        impl HasEncoding for $struct_name {
            fn encoding() -> Encoding {
                $code
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use crate as tezos_data_encoding;
    use tezos_data_encoding_derive::{BinWriter, NomReader};

    #[derive(Debug, PartialEq, BinWriter, NomReader)]
    struct Bytes {
        #[encoding(short_dynamic, list)]
        data: Vec<u8>,
    }

    #[test]
    fn short_dynamic() {
        let test = Bytes {
            data: vec![1_u8, 2_u8, 3_u8],
        };
        let mut output = Vec::new();
        crate::enc::BinWriter::bin_write(&test, &mut output)
            .expect("BinWriting should have succeed");

        // Verify that the size of the encoded data is a short (one byte)
        assert_eq!(output, vec![3_u8, 1_u8, 2_u8, 3_u8]);

        let (rem, decoded_test) =
            crate::nom::NomReader::nom_read(&output).expect("NomReading should have succeed");
        assert!(rem.is_empty());

        assert_eq!(test, decoded_test);
    }
}
