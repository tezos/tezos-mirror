// Copyright (c) SimpleStaking, Viable Systems, Trili Tech and Tezedge Contributors
// SPDX-License-Identifier: MIT

//! Defines types of the intermediate data format.

use std::str::FromStr;

use crate::enc::BinWriter;
use crate::encoding::Encoding;
use crate::encoding::HasEncoding;
use crate::has_encoding;
use crate::nom::NomReader;

use hex::FromHexError;
use serde::{Deserialize, Serialize};

/// This is a wrapper for [num_bigint::BigInt] type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BigInt(pub num_bigint::BigInt);

impl From<num_bigint::BigInt> for BigInt {
    fn from(from: num_bigint::BigInt) -> Self {
        BigInt(from)
    }
}

impl From<BigInt> for num_bigint::BigInt {
    fn from(from: BigInt) -> Self {
        from.0
    }
}

impl From<&num_bigint::BigInt> for BigInt {
    fn from(from: &num_bigint::BigInt) -> Self {
        BigInt(from.clone())
    }
}

impl From<&BigInt> for num_bigint::BigInt {
    fn from(from: &BigInt) -> Self {
        from.0.clone()
    }
}

/// Zarith number
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
pub struct Zarith(pub num_bigint::BigInt);

impl From<num_bigint::BigInt> for Zarith {
    fn from(from: num_bigint::BigInt) -> Self {
        Zarith(from)
    }
}

impl From<Zarith> for num_bigint::BigInt {
    fn from(from: Zarith) -> Self {
        from.0
    }
}

impl From<&num_bigint::BigInt> for Zarith {
    fn from(from: &num_bigint::BigInt) -> Self {
        Zarith(from.clone())
    }
}

impl From<&Zarith> for num_bigint::BigInt {
    fn from(from: &Zarith) -> Self {
        from.0.clone()
    }
}

impl From<Zarith> for BigInt {
    fn from(source: Zarith) -> Self {
        Self(source.0)
    }
}

impl From<&Zarith> for BigInt {
    fn from(source: &Zarith) -> Self {
        Self(source.0.clone())
    }
}

impl From<i32> for Zarith {
    fn from(value: i32) -> Self {
        Zarith(value.into())
    }
}

impl From<u64> for Zarith {
    fn from(value: u64) -> Self {
        Zarith(value.into())
    }
}

impl From<num_bigint::BigUint> for Zarith {
    fn from(from: num_bigint::BigUint) -> Self {
        Zarith(num_bigint::BigInt::from(from))
    }
}

impl From<Narith> for Zarith {
    fn from(from: Narith) -> Self {
        Zarith(num_bigint::BigInt::from(from.0))
    }
}

has_encoding!(Zarith, ZARITH_ENCODING, { Encoding::Z });

/// Error returned by [`Zarith::from_str`] when a string is not a valid
/// integer literal under OCaml Zarith's `Z.of_string` grammar.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseZarithError;

impl std::fmt::Display for ParseZarithError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid Zarith integer literal")
    }
}

impl std::error::Error for ParseZarithError {}

impl FromStr for Zarith {
    type Err = ParseZarithError;

    /// Parse an arbitrary-precision integer with the exact syntactic rules of
    /// OCaml Zarith's `Z.of_string`.
    ///
    /// This is a faithful port of Zarith's C parser `ml_z_of_substring_base`
    /// (`caml_z.c`, called with base 0); the Rust ecosystem has no binding to
    /// the OCaml Zarith runtime, so the grammar is reproduced byte-for-byte.
    /// The order of the steps matters for exact parity:
    ///
    ///   1. an optional `-` (sets the sign) **followed by** an optional `+` —
    ///      two independent checks, so `-+42` is `-42`, but `+-42`, `++42` and
    ///      `--42` are rejected (only the very first `-` and first `+` are
    ///      consumed);
    ///   2. an optional radix prefix `0x`/`0X`, `0o`/`0O`, `0b`/`0B`
    ///      (default 10);
    ///   3. a leading `_` at the start of the digit body is rejected;
    ///   4. each remaining byte must be `_` (ignored, a digit separator) or a
    ///      digit valid in the radix — anything else (including a stray sign)
    ///      is rejected.
    ///
    /// Mirroring Zarith, a body that reduces to no digits parses to `0`: this
    /// includes `"+"`, `"-"`, `"0x"`, the empty string, and e.g. `"0_"`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use num_bigint::Sign;
        let bytes = s.as_bytes();
        let n = bytes.len();
        let mut i = 0;

        // Step 1: optional `-` then optional `+` (Zarith's two independent
        // `if`s).
        let mut sign = Sign::Plus;
        if i < n && bytes[i] == b'-' {
            sign = Sign::Minus;
            i += 1;
        }
        if i < n && bytes[i] == b'+' {
            i += 1;
        }

        // Step 2: optional radix prefix. A lone `0` not followed by
        // `o`/`x`/`b` is left in place as an ordinary (leading-zero) digit,
        // matching Zarith's `d--` rollback.
        let mut radix = 10u32;
        if i < n && bytes[i] == b'0' && i + 1 < n {
            match bytes[i + 1] {
                b'o' | b'O' => {
                    radix = 8;
                    i += 2;
                }
                b'x' | b'X' => {
                    radix = 16;
                    i += 2;
                }
                b'b' | b'B' => {
                    radix = 2;
                    i += 2;
                }
                _ => {}
            }
        }

        // Step 3: Zarith rejects a leading underscore at the start of the body.
        if i < n && bytes[i] == b'_' {
            return Err(ParseZarithError);
        }

        // Step 4: validate every remaining byte against the radix, dropping the
        // interior `_` separators. A stray sign or out-of-range digit is
        // rejected here, exactly as Zarith's per-character `invalid_argument`
        // check.
        let mut cleaned = String::with_capacity(n - i);
        for &b in &bytes[i..] {
            if b == b'_' {
                continue;
            }
            let digit = match b {
                b'0'..=b'9' => u32::from(b - b'0'),
                b'a'..=b'f' => u32::from(b - b'a') + 10,
                b'A'..=b'F' => u32::from(b - b'A') + 10,
                _ => return Err(ParseZarithError),
            };
            if digit >= radix {
                return Err(ParseZarithError);
            }
            cleaned.push(char::from(b));
        }

        // Empty digit body parses to 0 in Zarith (e.g. `Z.of_string "+"`,
        // `"0x"`, `""`, `"0_"`).
        if cleaned.is_empty() {
            return Ok(Zarith(num_bigint::BigInt::from(0)));
        }
        // `cleaned` now holds only digits valid in `radix`, so `parse_bytes`
        // succeeds; the error arm is defensive.
        let magnitude =
            num_bigint::BigUint::parse_bytes(cleaned.as_bytes(), radix).ok_or(ParseZarithError)?;
        Ok(Zarith(num_bigint::BigInt::from_biguint(sign, magnitude)))
    }
}

/// Mutez number
#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Narith(pub num_bigint::BigUint);

#[deprecated = "Mutez has been replaced by Narith, which has identical semantics for encoding & decoding"]
pub type Mutez = Narith;

impl<'de> Deserialize<'de> for Narith {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            let string: String = serde::Deserialize::deserialize(deserializer)?;
            let big_uint: num_bigint::BigUint = string
                .parse()
                .map_err(|err| serde::de::Error::custom(format!("cannot parse big int: {err}")))?;
            Ok(Self(big_uint))
        } else {
            Ok(Self(serde::Deserialize::deserialize(deserializer)?))
        }
    }
}

impl Serialize for Narith {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            let string = self.0.to_string();
            string.serialize(serializer)
        } else {
            self.0.serialize(serializer)
        }
    }
}

impl From<num_bigint::BigUint> for Narith {
    fn from(from: num_bigint::BigUint) -> Self {
        Narith(from)
    }
}

impl From<Narith> for num_bigint::BigUint {
    fn from(from: Narith) -> Self {
        from.0
    }
}

impl From<&num_bigint::BigUint> for Narith {
    fn from(from: &num_bigint::BigUint) -> Self {
        Narith(from.clone())
    }
}

impl From<&Narith> for num_bigint::BigUint {
    fn from(from: &Narith) -> Self {
        from.0.clone()
    }
}

impl From<u64> for Narith {
    fn from(value: u64) -> Self {
        Narith(value.into())
    }
}

has_encoding!(Narith, NARITH_ENCODING, { Encoding::N });

#[derive(Clone, PartialEq, Eq)]
pub struct SizedBytes<const SIZE: usize>(pub [u8; SIZE]);

impl<const SIZE: usize> std::fmt::Display for SizedBytes<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode(self.0))
    }
}

impl<const SIZE: usize> std::fmt::Debug for SizedBytes<SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bytes: {}", hex::encode(self.0))
    }
}

impl<const SIZE: usize> AsRef<[u8]> for SizedBytes<SIZE> {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl<const SIZE: usize> AsMut<[u8]> for SizedBytes<SIZE> {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl<const SIZE: usize> From<[u8; SIZE]> for SizedBytes<SIZE> {
    fn from(bytes: [u8; SIZE]) -> Self {
        Self(bytes)
    }
}

impl<const SIZE: usize> From<SizedBytes<SIZE>> for [u8; SIZE] {
    fn from(bytes: SizedBytes<SIZE>) -> Self {
        bytes.0
    }
}

impl<const SIZE: usize> Serialize for SizedBytes<SIZE> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            serializer.serialize_str(&hex::encode(self.0.as_ref()))
        } else {
            serializer.serialize_newtype_struct(stringify!(Bytes<SIZE>), self.0.as_ref())
        }
    }
}

struct BytesVisitor<const SIZE: usize>;

impl<'de, const SIZE: usize> serde::de::Visitor<'de> for BytesVisitor<SIZE> {
    type Value = SizedBytes<{ SIZE }>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("eigher sequence of bytes or hex encoded data expected")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let mut bytes = [0_u8; SIZE];
        hex::decode_to_slice(v, &mut bytes)
            .map_err(|e| E::custom(format!("error constructing bytes from hex: {e}")))?;
        Ok(bytes.into())
    }

    fn visit_newtype_struct<E>(self, e: E) -> Result<Self::Value, E::Error>
    where
        E: serde::Deserializer<'de>,
    {
        let mut bytes = [0_u8; SIZE];
        match <Vec<u8> as serde::Deserialize>::deserialize(e) {
            Ok(val) if val.len() == SIZE => bytes.copy_from_slice(&val),
            Ok(val) => {
                return Err(serde::de::Error::custom(format!(
                    "invalid lenght, expected {SIZE}, got {len}",
                    len = val.len()
                )))
            }
            Err(err) => {
                return Err(err);
            }
        };
        Ok(bytes.into())
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        let mut bytes = [0_u8; SIZE];
        match seq.next_element::<Vec<u8>>() {
            Ok(Some(val)) if val.len() == SIZE => bytes.copy_from_slice(&val),
            Ok(Some(val)) => {
                return Err(serde::de::Error::custom(format!(
                    "invalid lenght, expected {SIZE}, got {len}",
                    len = val.len()
                )))
            }
            Ok(None) => return Err(serde::de::Error::custom("no bytes".to_string())),
            Err(err) => return Err(err),
        };
        Ok(bytes.into())
    }
}

impl<'de, const SIZE: usize> Deserialize<'de> for SizedBytes<SIZE> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            deserializer.deserialize_str(BytesVisitor)
        } else {
            deserializer.deserialize_newtype_struct("Bytes<SIZE>", BytesVisitor)
        }
    }
}

impl<const SIZE: usize> NomReader<'_> for SizedBytes<SIZE> {
    fn nom_read(input: &[u8]) -> crate::nom::NomResult<Self> {
        use crate::nom;
        let (input, slice) = nom::sized(SIZE, nom::bytes)(input)?;
        let mut bytes = [0; SIZE];
        bytes.copy_from_slice(&slice);
        Ok((input, bytes.into()))
    }
}

impl<const SIZE: usize> BinWriter for SizedBytes<SIZE> {
    fn bin_write(&self, bytes: &mut Vec<u8>) -> crate::enc::BinResult {
        use crate::enc;
        enc::put_bytes(&self.0, bytes);
        Ok(())
    }
}

impl<const SIZE: usize> HasEncoding for SizedBytes<SIZE> {
    fn encoding() -> Encoding {
        Encoding::sized(SIZE, Encoding::Bytes)
    }
}

/// Sequence of bytes bounded by maximum size
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Bytes(Vec<u8>);

#[derive(Debug, thiserror::Error)]
pub enum BytesDecodeError {
    #[error(transparent)]
    Hex(#[from] FromHexError),
}

impl Bytes {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl std::fmt::Debug for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Bytes").field(&self.to_string()).finish()
    }
}

impl std::fmt::Display for Bytes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        hex::encode(&self.0).fmt(f)
    }
}

impl From<Vec<u8>> for Bytes {
    fn from(source: Vec<u8>) -> Self {
        Self(source)
    }
}

impl From<Bytes> for Vec<u8> {
    fn from(source: Bytes) -> Self {
        source.0
    }
}

impl FromStr for Bytes {
    type Err = BytesDecodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(hex::decode(s)?))
    }
}

impl AsRef<[u8]> for Bytes {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl AsRef<Vec<u8>> for Bytes {
    fn as_ref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl HasEncoding for Bytes {
    fn encoding() -> Encoding {
        Encoding::list(Encoding::Uint8)
    }
}

impl NomReader<'_> for Bytes {
    fn nom_read(input: &[u8]) -> crate::nom::NomResult<Self> {
        use crate::nom::bytes;
        let (input, b) = bytes(input)?;
        Ok((input, Self(b)))
    }
}

impl BinWriter for Bytes {
    fn bin_write(&self, output: &mut Vec<u8>) -> crate::enc::BinResult {
        crate::enc::put_bytes(self.0.as_ref(), output);
        Ok(())
    }
}

impl serde::Serialize for Bytes {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if serializer.is_human_readable() {
            let hex_bytes = hex::encode(&self.0);
            serde::Serialize::serialize(&hex_bytes, serializer)
        } else {
            serde::Serialize::serialize(&self.0, serializer)
        }
    }
}

impl<'de> serde::Deserialize<'de> for Bytes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            let hex_bytes: String = serde::Deserialize::deserialize(deserializer)?;
            let bytes = hex::decode(hex_bytes).map_err(|err| {
                serde::de::Error::custom(format!("error decoding from hex string: {err}"))
            })?;
            Ok(Self(bytes))
        } else {
            let bytes = serde::Deserialize::deserialize(deserializer)?;
            Ok(Self(bytes))
        }
    }
}

/// Represents `true` value in binary format.
pub const BYTE_VAL_TRUE: u8 = 0xFF;
/// Represents `false` value in binary format.
pub const BYTE_VAL_FALSE: u8 = 0;
/// Represents `Some(x)` value in binary format.
pub const BYTE_VAL_SOME: u8 = 1;
/// Represents `None` value in binary format.
pub const BYTE_VAL_NONE: u8 = 0;
/// TE-172 - Represents 'Optional field' in binary format.
pub const BYTE_FIELD_SOME: u8 = 0xFF;
/// TE-172 - Represents `None` for 'Optional field' in binary format.
pub const BYTE_FIELD_NONE: u8 = 0;

/// Represents data in the intermediate form.
///
/// Rust struct is first converted to this intermediate form is then used for produce binary or json output.
/// Also this intermediate form is used when rust type is being created from a binary or json input.
///
/// # How it works:
///
/// Imagine we have struct `MyStruct` we want to serialize.
/// ```rust
/// struct MyStruct {
///   count: i32,
///   diameter: f32
/// }
/// let my_struct = MyStruct { count: 1, diameter: 102.95 };
/// ```
///
/// First we need to convert it to intermediate form represented by the [Value] type.
/// Structure will be converted to:
/// ```rust
/// use crate::tezos_data_encoding::types::Value;
/// let intermediate = Value::Record(vec![
///     ("count".into(), Value::Int32(1)),
///     ("diameter".into(), Value::Float(102.95))
/// ]);
/// ```
///
/// After that the intermediate form can be converted to binary by passing it to [crate::binary_writer::BinaryWriter].
#[derive(PartialEq, Debug)]
pub enum Value {
    /// Nothing: data is omitted from binary.
    Unit,
    /// Signed 8 bit integer (data is encoded as a byte in binary and an integer in JSON).
    Int8(i8),
    /// Unsigned 8 bit integer (data is encoded as a byte in binary and an integer in JSON).
    Uint8(u8),
    /// Signed 16 bit integer (data is encoded as a short in binary and an integer in JSON).
    Int16(i16),
    /// Unsigned 16 bit integer (data is encoded as a short in binary and an integer in JSON).
    Uint16(u16),
    /// Signed 31 bit integer, which corresponds to type int on 32-bit OCaml systems (data is encoded as a 32 bit int in binary and an integer in JSON).
    Int31(i32),
    /// Signed 32 bit integer (data is encoded as a 32-bit int in binary and an integer in JSON).
    Int32(i32),
    /// Signed 64 bit integer (data is encoded as a 64-bit int in binary and a decimal string in JSON).
    Int64(i64),
    /// Integer with bounds in a given range. Both bounds are inclusive.
    RangedInt(i32),
    /// Encoding of floating point number (encoded as a floating point number in JSON and a double in binary).
    Float(f64),
    /// Float with bounds in a given range. Both bounds are inclusive.
    RangedFloat(f64),
    /// Encoding of a boolean (data is encoded as a byte in binary and a boolean in JSON).
    Bool(bool),
    /// Encoding of a string
    /// - encoded as a byte sequence in binary prefixed by the length of the string
    /// - encoded as a string in JSON.
    String(String),
    /// Encoding of arbitrary bytes (encoded via hex in JSON and directly as a sequence byte in binary).
    Bytes(Vec<u8>),
    /// Combinator to make an optional value
    /// (represented as a 1-byte tag followed by the data (or nothing) in binary
    ///  and either the raw value or an empty object in JSON).
    Option(Option<Box<Value>>),
    /// List combinator.
    /// - encoded as an array in JSON
    /// - encoded as the concatenation of all the element in binary in binary prefixed by its length in bytes
    List(Vec<Value>),
    /// Enum value with name and/or ordinal number
    Enum(Option<String>, Option<u32>),
    /// Tag value with variant id and tag inner value
    Tag(String, Box<Value>),
    /// A Record is represented by a vector of (`<record name>`, `value`).
    /// This allows schema-less encoding.
    ///
    /// See [Record](types.Record) for a more user-friendly support.
    Record(Vec<(String, Value)>),
    /// Tuple is heterogeneous collection of values, it should have fixed amount of elements
    Tuple(Vec<Value>),
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn bytes_to_string() {
        let bytes = Bytes(vec![0xde, 0xad, 0xbe, 0xef]);
        let bytes_hex = bytes.to_string();
        assert_eq!(&bytes_hex, "deadbeef");
    }

    #[test]
    fn bytes_parse() {
        let bytes: Bytes = "deadbeef".parse().unwrap();
        assert_eq!(bytes, Bytes(vec![0xde, 0xad, 0xbe, 0xef]));
    }

    #[test]
    fn bytes_parse_error() {
        let _ = "deadbeefe".parse::<Bytes>().expect_err("");
        let _ = "morebeef".parse::<Bytes>().expect_err("");
    }

    #[test]
    fn bytes_to_json() {
        let bytes = Bytes(vec![0xde, 0xad, 0xbe, 0xef]);
        let json = serde_json::to_value(bytes).unwrap();
        assert!(matches!(json.as_str(), Some("deadbeef")))
    }

    #[test]
    fn bytes_from_json() {
        let json = serde_json::json!("deadbeef");
        let bytes: Bytes = serde_json::from_value(json).unwrap();
        assert_eq!(bytes, Bytes(vec![0xde, 0xad, 0xbe, 0xef]));
    }

    /// [`Zarith`]'s `FromStr` must match OCaml Zarith's `Z.of_string`
    /// byte-for-byte. Each case is annotated with the corresponding
    /// `Z.of_string` result (verified against the OCaml library).
    #[test]
    fn zarith_from_str_mirrors_zarith() {
        fn ok(s: &str, v: i64) {
            assert_eq!(s.parse::<Zarith>(), Ok(Zarith(v.into())), "input {s:?}");
        }
        fn err(s: &str) {
            assert_eq!(s.parse::<Zarith>(), Err(ParseZarithError), "input {s:?}");
        }

        // Plain decimal, with optional sign.
        ok("0", 0);
        ok("42", 42);
        ok("-42", -42);
        ok("+42", 42);

        // Radix prefixes (both cases).
        ok("0xff", 255);
        ok("0XFF", 255);
        ok("0o17", 15);
        ok("0b1010", 10);
        ok("-0x10", -16);

        // Interior underscores are digit separators; a leading one is invalid.
        ok("1_000_000", 1_000_000);
        ok("0_0_1", 1);
        err("0x_ff"); // leading `_` after prefix
        err("_42"); // leading `_`

        // Sign and/or prefix with no digit body parse to 0.
        ok("+", 0);
        ok("-", 0);
        ok("0x", 0);
        ok("", 0);
        ok("0_", 0);

        // Sign sequencing: optional `-` then optional `+`.
        ok("-+42", -42);
        ok("-+0x10", -16);
        ok("-+", 0);
        err("++42");
        err("+-42");
        err("--42");
        // A `+`/`-` inside the digit body (after the prefix) is an invalid
        // digit.
        err("0x+10");
        err("+-0x10");

        // Truly invalid inputs.
        err("3.5");
        err("ABCD");
        err("0xZ");
        err("_");

        // Big value past i64 round-trips through the magnitude path.
        assert_eq!(
            "99999999999999999999".parse::<Zarith>().map(|z| z.0),
            num_bigint::BigInt::parse_bytes(b"99999999999999999999", 10).ok_or(ParseZarithError)
        );
    }
}
