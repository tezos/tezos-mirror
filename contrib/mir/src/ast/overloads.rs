// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Various overloads for different instructions. The name of the enum
//! corresponds to the name of the instruction, while the variant name
//! corresponds to the stack types used for the particular overload.

#![allow(missing_docs)]

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Add {
    IntInt,
    NatNat,
    IntNat,
    NatInt,
    MutezMutez,
    #[cfg(feature = "bls")]
    Bls12381G1,
    #[cfg(feature = "bls")]
    Bls12381G2,
    #[cfg(feature = "bls")]
    Bls12381Fr,
    TimestampInt,
    IntTimestamp,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Sub {
    NatNat,
    NatInt,
    IntNat,
    IntInt,
    TimestampInt,
    TimestampTimestamp,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum And {
    Bool,
    NatNat,
    IntNat,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Or {
    Bool,
    Nat,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Xor {
    Bool,
    Nat,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Not {
    Bool,
    Nat,
    Int,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mem {
    Set,
    Map,
    BigMap,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mul {
    NatNat,
    NatInt,
    IntNat,
    IntInt,
    MutezNat,
    NatMutez,
    #[cfg(feature = "bls")]
    Bls12381G1Bls12381Fr,
    #[cfg(feature = "bls")]
    Bls12381G2Bls12381Fr,
    #[cfg(feature = "bls")]
    Bls12381FrBls12381Fr,
    #[cfg(feature = "bls")]
    NatBls12381Fr,
    #[cfg(feature = "bls")]
    IntBls12381Fr,
    #[cfg(feature = "bls")]
    Bls12381FrNat,
    #[cfg(feature = "bls")]
    Bls12381FrInt,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EDiv {
    NatNat,
    NatInt,
    IntNat,
    IntInt,
    MutezNat,
    MutezMutez,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Neg {
    Nat,
    Int,
    #[cfg(feature = "bls")]
    Bls12381G1,
    #[cfg(feature = "bls")]
    Bls12381G2,
    #[cfg(feature = "bls")]
    Bls12381Fr,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Lsl {
    Nat,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Lsr {
    Nat,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Get {
    Map,
    BigMap,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Update {
    Set,
    Map,
    BigMap,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum GetAndUpdate {
    Map,
    BigMap,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Size {
    String,
    Bytes,
    List,
    Set,
    Map,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Iter {
    List,
    Set,
    Map,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Slice {
    String,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Concat {
    TwoStrings,
    TwoBytes,
    ListOfStrings,
    ListOfBytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Int {
    Nat,
    #[cfg(feature = "bls")]
    Bls12381Fr,
    Bytes,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Bytes {
    Int,
    Nat,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Map {
    List,
    Option,
    Map,
}
