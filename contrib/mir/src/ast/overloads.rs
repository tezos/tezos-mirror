/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Add {
    IntInt,
    NatNat,
    IntNat,
    NatInt,
    MutezMutez,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mem {
    Set,
    Map,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Get {
    Map,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Update {
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
