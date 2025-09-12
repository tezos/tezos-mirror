// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations

use crate::enc_wrappers::ScriptExprHash;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::Zarith;

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
pub struct Update {
    pub key_hash: ScriptExprHash,
    pub key: Vec<u8>,
    pub value: Option<Vec<u8>>,
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
pub struct Copy {
    pub source: Zarith,
    #[encoding(dynamic, list)]
    pub updates: Vec<Update>,
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
pub struct Alloc {
    #[encoding(dynamic, list)]
    pub updates: Vec<Update>,
    pub key_type: Vec<u8>,
    pub value_type: Vec<u8>,
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
#[encoding(tags = "u8")]
pub enum StorageDiff {
    #[encoding(dynamic, list)]
    Update(Vec<Update>),
    Remove,
    Copy(Copy),
    Alloc(Alloc),
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
pub struct BigMapDiff {
    pub id: Zarith,
    pub storage_diff: StorageDiff,
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
#[encoding(tags = "u8")]
pub enum LazyStorageDiff {
    BigMap(BigMapDiff),
}

#[derive(PartialEq, Debug, BinWriter, NomReader, Clone)]
pub struct LazyStorageDiffList {
    #[encoding(dynamic, list)]
    pub diff: Vec<LazyStorageDiff>,
}
