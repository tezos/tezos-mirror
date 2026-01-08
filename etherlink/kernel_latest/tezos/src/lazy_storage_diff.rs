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

impl From<LazyStorageDiff> for LazyStorageDiffList {
    fn from(value: LazyStorageDiff) -> Self {
        LazyStorageDiffList { diff: vec![value] }
    }
}

impl StorageDiff {
    /// Push a big_map update in an already existing diff
    /// If the already existing diff is a remove does nothing
    pub fn push_update(&mut self, update: Update) {
        match self {
            StorageDiff::Update(updates) => updates.push(update),
            StorageDiff::Remove => (),
            StorageDiff::Copy(copy) => copy.updates.push(update),
            StorageDiff::Alloc(alloc) => alloc.updates.push(update),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        enc_wrappers::ScriptExprHash,
        encoding_test_data_helper::test_helpers::fetch_generated_data,
        lazy_storage_diff::{
            Alloc, BigMapDiff, Copy, LazyStorageDiff, LazyStorageDiffList, StorageDiff,
            Update,
        },
        protocol::TARGET_TEZOS_PROTOCOL,
    };
    use mir::ast::BinWriter;
    use primitive_types::H256;

    #[test]
    pub fn big_map_alloc_compatibility() {
        let key_type = mir::ast::Micheline::prim0(mir::lexer::Prim::nat).encode();
        let value_type = mir::ast::Micheline::prim0(mir::lexer::Prim::unit).encode();
        let diff: LazyStorageDiffList = LazyStorageDiff::BigMap(BigMapDiff {
            id: 0u64.into(),
            storage_diff: StorageDiff::Alloc(Alloc {
                updates: vec![],
                key_type,
                value_type,
            }),
        })
        .into();
        let expected = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "lazy_storage_diff",
            "big_map_diff-alloc",
        );
        assert_eq!(
            diff.to_bytes()
                .expect("Lazy storage diff encoding should have succeed"),
            expected,
            "Lazy storage diff encoding should have been compatible"
        );
    }

    #[test]
    pub fn big_map_copy_compatibility() {
        let diff: LazyStorageDiffList = LazyStorageDiff::BigMap(BigMapDiff {
            id: 0u64.into(),
            storage_diff: StorageDiff::Copy(Copy {
                source: 0u64.into(),
                updates: vec![],
            }),
        })
        .into();
        let expected = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "lazy_storage_diff",
            "big_map_diff-copy",
        );
        assert_eq!(
            diff.to_bytes()
                .expect("Lazy storage diff encoding should have succeed"),
            expected,
            "Lazy storage diff encoding should have been compatible"
        );
    }

    #[test]
    pub fn big_map_remove_compatibility() {
        let diff: LazyStorageDiffList = LazyStorageDiff::BigMap(BigMapDiff {
            id: 0u64.into(),
            storage_diff: StorageDiff::Remove,
        })
        .into();
        let expected = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "lazy_storage_diff",
            "big_map_diff-remove",
        );
        assert_eq!(
            diff.to_bytes()
                .expect("Lazy storage diff encoding should have succeed"),
            expected,
            "Lazy storage diff encoding should have been compatible"
        );
    }

    #[test]
    pub fn big_map_update_compatibility() {
        let key = mir::ast::Micheline::Int(1u64.into()).encode();
        let value = Some(mir::ast::Micheline::prim0(mir::lexer::Prim::UNIT).encode());
        let diff: LazyStorageDiffList = LazyStorageDiff::BigMap(BigMapDiff {
            id: 0u64.into(),
            storage_diff: StorageDiff::Update(vec![Update {
                key_hash: ScriptExprHash(H256::from_str("cffedbaf00cb581448a5683abdefe0d5cd4d4ba4923f1a489791810c3fec3325").unwrap()),
                key,
                value,
            }]),
        })
        .into();
        let expected = fetch_generated_data(
            TARGET_TEZOS_PROTOCOL,
            "lazy_storage_diff",
            "big_map_diff-update",
        );
        assert_eq!(
            diff.to_bytes()
                .expect("Lazy storage diff encoding should have succeed"),
            expected,
            "Lazy storage diff encoding should have been compatible"
        );
    }
}
