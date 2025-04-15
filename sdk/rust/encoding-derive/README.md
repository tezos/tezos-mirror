Tezos data encoding derive
=====================

Procedural macro to allow automatic derivation of `HasEncoding`,
`BinWriter` & `NomReader` traits - enabling (de)serialization of rust
structures to the `Data_encoding` binary format, with less
boilerplate.

Getting started
---------------

From a struct or enum definition such as

```rust
struct S {
  a : i32,
  b : Option<Vec<bool>>,
}
```

the derive macros of this module can automatically produce
implementation of the BinWriter (serialization) and NomReader
(deserialization) traits. These derived implementations are inverse of
each other, which we can test with a roundtrip check:

```rust
// The derive macros are brought in scope when these traits are used
use tezos_data_encoding::{enc::BinWriter, nom::NomReader};

#[derive(Debug, PartialEq, BinWriter, NomReader)]
struct S {
    a: i32,
    b: Option<Vec<bool>>,
}

#[test]
fn test_roundtrip() {
    let s = S { a: -42, b: Some(vec![true, false, false]) };
    let mut serialized = vec![];
    s.bin_write(&mut serialized)
        .expect("serialization should succeed");
    let (remaining_input, deserialized) =
        S::nom_read(&serialized).expect("deserialization should succeed");
    assert_eq!(remaining_input.len(), 0);
    assert_eq!(s, deserialized)
}
```

Builtin types
-------------

The following types are built into the derivation macros and can thus
be used freely to type the fields of the structs: `bool`, `i8`, `u8`,
`i16`, `u16`, `i32`, `u32`, `i64`, `f64`, `Vec<_>`, `Option<_>`,
`String`.

Enums and tags
--------------

The BinWriter and NomReader traits can also be derived for
enums. Consider for example the following enum definition:

```rust
#[derive(BinWriter, NomReader)]
enum E { A, B, C }
```

The derived serialization format uses by default a single-byte tag. In
the previous example, the constructors `A`, `B`, and `C` are mapped
respectively to the tags `0`, `1`, and `2`. This can however be
customized using the `tag` attribute as follows:

```rust
#[derive(BinWriter, NomReader)]
enum E {
  #[encoding(tag = 2)
  A,
  #[encoding(tag = 4)]
  B,
  #[encoding(tag = 6)]
  C
}
```

It is also possible to use tags spreading over several bytes as follows:

```rust
#[derive(BinWriter, NomReader)]
#[encoding(tags = "u16")]
enum E {
    #[encoding(tag = 256)]
    A,
    #[encoding(tag = 257)]
    B,
    #[encoding(tag = 10)]
    C,
}
```

The tags are encoded in big endian.


Dynamic encodings
-----------------

By default, the serialization of a struct with several fields is the
concatenation of the serialization of the fields. It works as expected
in simple cases such as the `S` struct above because the field `a`
which is read first is of fixed size (4 bytes in the case of
`i32`). But the roundtrip property is broken if any field but the last
one has an unknown length. Consider for example a struct with two
fields of type `Vec<u8>`:

```rust
#[derive(BinWriter, NomReader)]
struct BytesTwice {
    bytes_1: Vec<u8>,
    bytes_2: Vec<u8>,
}
```

Serializing and then deserializing the value `BytesTwice { bytes_1:
vec![1, 2, 3], bytes_2: vec![2, 3, 4] }` results in the incorrect
value `BytesTwice { bytes_1: vec![1, 2, 3, 2, 3, 4], bytes_2: vec![]
}`.

The solution to this problem is to mark the first field with the
`dynamic` attribute to prepend the length of the field (using four
bytes).

```rust
#[derive(BinWriter, NomReader)]
struct BytesTwice {
    #[encoding(dynamic, bytes)]
    bytes_1: Vec<u8>,
    bytes_2: Vec<u8>,
}
```

Note that we had to specify that the `dynamic` attribute applies to
the whole `Vec<u8>` type by also using the `bytes` attribute; using
only `#[encoding(dynamic)]` would have resulted in each byte of
`bytes_1` to be prefixed with its length.

Bounded encodings
-----------------





Advanced features
-----------------

- Dynamic and bounded encodings
  - #[encoding(dynamic)] to mark a field as dynamically encoded (prefixed with the length)
  - #[encoding(bytes)] forces the field to be a collection of u8
  - #[encoding(bounded = N)] to constraint a field to a N max length 
  - #[encoding(reserve = N)] -> use the nom::reserve combinator to reserve trailing bytes


Unsupported features
--------------------

- Unions

- Fields of type unit

- Constructor with several arguments

- Empty struct (no NomReader)


HasEncoding trait
-----------------
