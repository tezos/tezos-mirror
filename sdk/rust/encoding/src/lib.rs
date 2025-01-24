// Copyright (c) SimpleStaking, Viable Systems and Tezedge Contributors
// SPDX-License-Identifier: MIT
#![forbid(unsafe_code)]

//! This crate provides serialization and deserialization functionality for the data types used by the Tezos shell.
//!
//! You can either implement [`NomReader`] and [`BinWriter`] manually, or derive them.
//!
//! # Examples
//!
//! Let's create encodings for a struct containing two arrays - one of fixed size, and one dynamic.
//!
//! Derivation is supported across generic structs.
//!
//! ```rust
//! use tezos_data_encoding::nom::NomReader;
//! use tezos_data_encoding::enc::BinWriter;
//! use tezos_data_encoding::encoding::HasEncoding;
//! # use core::fmt::Debug;
//!
//! const INNER_SIZE: usize = 10;
//!
//! #[derive(Debug, PartialEq, HasEncoding, NomReader, BinWriter)]
//! struct Inner {
//!   #[encoding(sized = "INNER_SIZE", bytes)]
//!   fixed_size: Vec<u8>
//! }
//!
//! #[derive(Debug, PartialEq, HasEncoding, NomReader, BinWriter)]
//! struct Outer<T>
//! where T: Debug + PartialEq + HasEncoding + for<'a> NomReader<'a> + BinWriter {
//!   #[encoding(dynamic)]
//!   dynamic_size: Vec<T>
//! }
//! #
//! # let inner_1 = Inner { fixed_size: vec![1; INNER_SIZE] };
//! # let inner_2 = Inner { fixed_size: vec![2; INNER_SIZE] };
//! #
//! # let outer = Outer { dynamic_size: vec![inner_1, inner_2] };
//! #
//! # let mut encoded = Vec::new();
//! # outer.bin_write(&mut encoded).expect("encoding works");
//! #
//! # let (_remaining_input, result) = Outer::<Inner>::nom_read(&encoded)
//! #     .expect("decoding works");
//! #
//! # assert!(_remaining_input.is_empty());
//! # assert_eq!(outer, result);
//! ```

mod bit_utils;
pub mod types;

pub mod binary_reader;
pub mod binary_writer;

pub mod enc;
pub mod encoding;
pub mod nom;
