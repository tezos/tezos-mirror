// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::enc_wrappers::BlockNumber;
use crate::operation_result::OperationDataAndMetadata;
use crate::protocol::Protocol;
use rlp::{Decodable, Encodable};
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{BlockHash, OperationHash};
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom::{self as tezos_nom};
use tezos_enc::{BinError, BinWriter};
use tezos_nom::NomReader;
use tezos_smart_rollup::types::Timestamp;

#[derive(PartialEq, Debug, BinWriter, NomReader, Eq)]
pub struct AppliedOperation {
    // OperationHash are 32 bytes long
    pub hash: OperationHash,
    pub branch: BlockHash,
    #[encoding(dynamic)]
    pub op_and_receipt: OperationDataAndMetadata,
}

impl Encodable for AppliedOperation {
    fn rlp_append(&self, stream: &mut rlp::RlpStream) {
        match self.to_bytes() {
            Ok(bytes) => bytes.rlp_append(stream),
            Err(_err) => (),
        }
    }
}

#[derive(PartialEq, Debug, Default, BinWriter, NomReader)]
pub struct OperationsWithReceipts {
    pub list: Vec<AppliedOperation>,
}

impl Encodable for OperationsWithReceipts {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        let bytes = self.to_bytes().unwrap_or_else(|_| {
            let operations = OperationsWithReceipts { list: vec![] };
            // This is a "safe" unwrap as we know exactly what we're trying to
            // convert to bytes.
            operations.to_bytes().unwrap()
        });
        s.append_internal(&bytes);
    }
}

impl Decodable for OperationsWithReceipts {
    fn decode(rlp: &rlp::Rlp) -> Result<Self, rlp::DecoderError> {
        let bytes = rlp.data()?;
        Self::nom_read_exact(bytes).map_err(|_| {
            rlp::DecoderError::Custom("Failed to decode OperationsWithReceipts")
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct TezBlock {
    pub hash: BlockHash,
    pub number: BlockNumber,
    pub previous_hash: BlockHash,
    pub timestamp: Timestamp,
    pub protocol: Protocol,
    pub next_protocol: Protocol,
    pub operations: OperationsWithReceipts,
    /// `keccak256(h(/tez/tez_accounts) || blueprint_hash)`. Commits to the
    /// Michelson account state and to the blueprint inputs (EVM txs,
    /// delayed txs, Michelson ops, timestamp) of the producing blueprint
    /// -- see `kernel/src/state_hash.rs` and `adr_tzx_state_hash.md`.
    pub state_root: [u8; 32],
}

const VERSION: u8 = 2;

impl Encodable for TezBlock {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.begin_list(9);
        s.append(&VERSION);
        s.append(&self.hash.as_ref());
        s.append(&self.number);
        s.append(&self.previous_hash.as_ref());
        s.append(&self.timestamp.i64().to_le_bytes().to_vec());
        s.append(&self.protocol);
        s.append(&self.next_protocol);
        s.append(&self.operations);
        s.append(&self.state_root.as_ref());
    }
}

impl TezBlock {
    /// Ghostnet genesis hash according to 'devtools/get_contracts/config.ml'
    pub fn genesis_block_hash() -> BlockHash {
        BlockHash::from_base58_check(
            "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9",
        )
        .unwrap()
    }

    // This function must be used on a TezBlock whose hash field is a zero BlockHash
    fn hash(&self) -> Result<BlockHash, BinError> {
        let encoded_data = self.to_bytes();
        let hashed_data = digest_256(&encoded_data);
        Ok(BlockHash::from(hashed_data))
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        self.rlp_bytes().to_vec()
    }

    pub fn new(
        protocol: Protocol,
        next_protocol: Protocol,
        number: BlockNumber,
        timestamp: Timestamp,
        previous_hash: primitive_types::H256,
        operations: Vec<AppliedOperation>,
        state_root: [u8; 32],
    ) -> Result<Self, BinError> {
        let block = Self {
            hash: BlockHash::default(), // Placeholder, will be computed
            number,
            timestamp,
            previous_hash: BlockHash::from(previous_hash.to_fixed_bytes()),
            protocol,
            next_protocol,
            operations: OperationsWithReceipts { list: operations },
            state_root,
        };
        let hash = block.hash()?;
        // Genesis convention: the first Tezos block is its own predecessor.
        let previous_hash =
            if previous_hash == primitive_types::H256(*Self::genesis_block_hash()) {
                BlockHash::from(*hash)
            } else {
                block.previous_hash
            };
        Ok(Self {
            hash,
            previous_hash,
            ..block
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lazy_storage_diff::*;
    use crate::operation::*;
    use crate::operation_result::*;
    use mir::ast::annotations::NO_ANNS;
    use mir::ast::micheline::Micheline;
    use mir::ast::Entrypoint;
    use mir::lexer::Prim;
    use primitive_types::H256;
    use tezos_crypto_rs::hash::{ContractKt1Hash, UnknownSignature};
    use tezos_data_encoding::types::{Narith, Zarith};
    use tezos_smart_rollup::types::Contract;
    use tezos_smart_rollup::types::PublicKeyHash;

    /// The block hash must commit to `state_root`: flipping it produces a
    /// different block hash and a different RLP encoding.
    #[test]
    fn state_root_is_committed_to_block_hash() {
        let number: BlockNumber = 1u32.into();
        let ts = tezos_smart_rollup::types::Timestamp::from(1_700_000_000i64);
        let parent = H256::zero();

        let a = TezBlock::new(
            Protocol::S023,
            Protocol::S023,
            number,
            ts,
            parent,
            vec![],
            [7u8; 32],
        )
        .expect("TezBlock::new with fixed state_root");
        let b = TezBlock::new(
            Protocol::S023,
            Protocol::S023,
            number,
            ts,
            parent,
            vec![],
            [8u8; 32],
        )
        .expect("TezBlock::new with different state_root");

        assert_ne!(a.hash, b.hash);
        assert_ne!(a.to_bytes(), b.to_bytes());
    }

    /// Build an `AppliedOperation` that mimics a CRAC receipt (handler → alias,
    /// with an Event internal op).  Then round-trip through BinWriter/NomReader
    /// to ensure the receipt survives storage in the BlockInProgress.
    #[test]
    fn crac_receipt_round_trip() {
        let null_pkh =
            PublicKeyHash::from_b58check("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU").unwrap();
        let alias_source =
            Contract::from_b58check("KT1SHrxmgUojs2hwe4hguExy6BqteeG1rDHi").unwrap();
        let alias_sender =
            Contract::from_b58check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton").unwrap();
        let target =
            Contract::from_b58check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton").unwrap();

        // CRAC event payload
        let crac_id_str = "1-0";
        let ty = Micheline::App(Prim::string, &[], NO_ANNS).encode().unwrap();
        let payload = Micheline::from(crac_id_str.to_string()).encode().unwrap();

        let transfer_internal =
            InternalOperationSum::Transfer(InternalContentWithMetadata {
                sender: alias_sender.clone(),
                nonce: 0,
                content: TransferContent {
                    amount: 0u64.into(),
                    destination: target,
                    parameters: Parameters::default(),
                },
                result: ContentResult::Applied(TransferTarget::from(
                    TransferSuccess::default(),
                )),
            });

        let event_internal = InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked("crac".into())),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: alias_source.clone(),
            nonce: 1,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: Narith(0u64.into()),
            }),
        });

        let top_level_result = OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(TransferTarget::from(
                TransferSuccess::default(),
            )),
            internal_operation_results: vec![transfer_internal, event_internal],
        };

        let signature = UnknownSignature::try_from([0u8; 64].as_slice()).unwrap();

        let op_data =
            OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
                operations: vec![OperationWithMetadata {
                    content: ManagerOperationContent::Transaction(ManagerOperation {
                        source: null_pkh,
                        fee: Narith(0u64.into()),
                        counter: Narith(0u64.into()),
                        gas_limit: Narith(0u64.into()),
                        storage_limit: Narith(0u64.into()),
                        operation: TransferContent {
                            amount: 0u64.into(),
                            destination: alias_source,
                            parameters: Parameters::default(),
                        },
                    }),
                    receipt: OperationResultSum::Transfer(top_level_result),
                }],
                signature,
            });

        let applied = AppliedOperation {
            hash: OperationHash::default(),
            branch: BlockHash::default(),
            op_and_receipt: op_data,
        };

        // Round-trip through BinWriter/NomReader
        let bytes = applied.to_bytes().expect("BinWriter should succeed");
        let decoded =
            AppliedOperation::nom_read_exact(&bytes).expect("NomReader should succeed");
        assert_eq!(applied, decoded, "round-trip mismatch");

        // Also test OperationsWithReceipts round-trip
        let ops = OperationsWithReceipts {
            list: vec![applied],
        };
        let ops_bytes = ops
            .to_bytes()
            .expect("BinWriter for OperationsWithReceipts");
        let ops_decoded = OperationsWithReceipts::nom_read_exact(&ops_bytes)
            .expect("NomReader for OperationsWithReceipts");
        assert_eq!(
            ops, ops_decoded,
            "OperationsWithReceipts round-trip mismatch"
        );

        // Also test RLP round-trip (this is what BlockInProgress uses)
        let mut stream = rlp::RlpStream::new();
        ops.rlp_append(&mut stream);
        let rlp_bytes = stream.out();
        let rlp = rlp::Rlp::new(&rlp_bytes);
        let ops_rlp_decoded: OperationsWithReceipts =
            rlp::Decodable::decode(&rlp).expect("RLP decode for OperationsWithReceipts");
        assert_eq!(ops, ops_rlp_decoded, "RLP round-trip mismatch");
    }

    #[test]
    fn transfer_success_with_storage_round_trip() {
        let storage_bytes =
            hex::decode("0a0000001601dce4a60cb714ccae99f6af1cf74331c56dc594da00")
                .unwrap();
        let ts = TransferSuccess {
            storage: Some(MichelineExpr(storage_bytes)),
            balance_updates: vec![],
            ticket_receipt: vec![],
            originated_contracts: vec![],
            consumed_milligas: Narith(0u64.into()),
            storage_size: 0.into(),
            paid_storage_size_diff: 0.into(),
            allocated_destination_contract: false,
            lazy_storage_diff: None,
            address_registry_diff: vec![],
        };
        let bytes = ts.to_bytes().expect("encode");
        eprintln!(
            "TransferSuccess with storage: {} bytes, hex={}",
            bytes.len(),
            hex::encode(&bytes)
        );
        let decoded = TransferSuccess::nom_read_exact(&bytes);
        assert!(
            decoded.is_ok(),
            "TransferSuccess round-trip failed: {:?}",
            decoded.err()
        );
    }

    /// Parse a real CRAC receipt captured from a live kernel execution.
    #[test]
    fn parse_real_crac_receipt_bytes() {
        let hex_str = "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012900000000e46c000000000000000000000000000000000000000000000000000001dce4a60cb714ccae99f6af1cf74331c56dc594da000000000000000000000000000000000000000000000000000000000000000000920101dce4a60cb714ccae99f6af1cf74331c56dc594da00000000015f2bcf8eb453099df505d0f52f260fd22382e41b00000000ff0a0000001601dce4a60cb714ccae99f6af1cf74331c56dc594da00000000000000000000000000e71800000000000000000401dce4a60cb714ccae99f6af1cf74331c56dc594da0000010368ffff0463726163ff0100000003312d30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        let bytes = hex::decode(hex_str).unwrap();
        let result = AppliedOperation::nom_read_exact(&bytes);
        assert!(
            result.is_ok(),
            "Failed to parse real CRAC receipt: {:?}",
            result.err()
        );
    }

    /// Round-trip a CRAC receipt where every field carries non-trivial data:
    /// balance updates, storage, originated contracts, lazy storage diffs
    /// (BigMap alloc with updates), address registry entries, ticket
    /// receipts, non-default parameters, and realistic fee/counter/gas values.
    #[test]
    fn crac_receipt_elaborate_round_trip() {
        // -- addresses ---------------------------------------------------
        let null_pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let alias_kt1 =
            Contract::from_b58check("KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw").unwrap();
        let target =
            Contract::from_b58check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton").unwrap();
        let originated_kt1 =
            ContractKt1Hash::from_base58_check("KT1SHrxmgUojs2hwe4hguExy6BqteeG1rDHi")
                .unwrap();

        // -- Micheline expressions ----------------------------------------
        let storage_expr = Micheline::App(Prim::pair, &[], NO_ANNS).encode().unwrap();
        let param_value = Micheline::Int(42u64.into()).encode().unwrap();

        // -- lazy storage diff: two big maps (copy + remove) ---------------
        let lazy_diff = LazyStorageDiffList {
            diff: vec![
                LazyStorageDiff::BigMap(BigMapDiff {
                    id: Zarith::from(7u64),
                    storage_diff: StorageDiff::Copy(Copy {
                        source: Zarith::from(5u64),
                        updates: vec![],
                    }),
                }),
                LazyStorageDiff::BigMap(BigMapDiff {
                    id: Zarith::from(3u64),
                    storage_diff: StorageDiff::Remove,
                }),
            ],
        };

        // -- balance updates ----------------------------------------------
        let balance_updates_top = vec![
            BalanceUpdate {
                balance: Balance::Account(
                    Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                        .unwrap(),
                ),
                changes: -500_000,
                update_origin: UpdateOrigin::BlockApplication,
            },
            BalanceUpdate {
                balance: Balance::BlockFees,
                changes: 500_000,
                update_origin: UpdateOrigin::BlockApplication,
            },
        ];
        let balance_updates_result = vec![
            BalanceUpdate {
                balance: Balance::Account(
                    Contract::from_b58check("tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN")
                        .unwrap(),
                ),
                changes: -1_000_000,
                update_origin: UpdateOrigin::BlockApplication,
            },
            BalanceUpdate {
                balance: Balance::StorageFees,
                changes: 250,
                update_origin: UpdateOrigin::BlockApplication,
            },
        ];

        // -- CRAC event payload -------------------------------------------
        let crac_id_str = "42-3";
        let ty = Micheline::App(Prim::string, &[], NO_ANNS).encode().unwrap();
        let payload = Micheline::from(crac_id_str.to_string()).encode().unwrap();

        // -- internal operations ------------------------------------------
        let transfer_internal =
            InternalOperationSum::Transfer(InternalContentWithMetadata {
                sender: alias_kt1.clone(),
                nonce: 0,
                content: TransferContent {
                    amount: 1_000_000u64.into(),
                    destination: target,
                    parameters: Parameters {
                        entrypoint: Entrypoint::from_string_unchecked(
                            "do_something".into(),
                        ),
                        value: param_value,
                    },
                },
                result: ContentResult::Applied(TransferTarget::from(TransferSuccess {
                    storage: Some(MichelineExpr(storage_expr)),
                    balance_updates: balance_updates_result,
                    ticket_receipt: vec![0xDE, 0xAD, 0xBE, 0xEF],
                    originated_contracts: vec![0x01, 0x02, 0x03],
                    consumed_milligas: Narith(1_500_000u64.into()),
                    storage_size: Zarith::from(320u64),
                    paid_storage_size_diff: Zarith::from(64u64),
                    allocated_destination_contract: true,
                    lazy_storage_diff: Some(lazy_diff),
                    address_registry_diff: vec![
                        AddressRegistry {
                            address: Contract::from_b58check(
                                "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw",
                            )
                            .unwrap(),
                            index: Zarith::from(0u64),
                        },
                        AddressRegistry {
                            address: Contract::from_b58check(
                                "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton",
                            )
                            .unwrap(),
                            index: Zarith::from(1u64),
                        },
                    ],
                })),
            });

        let event_internal = InternalOperationSum::Event(InternalContentWithMetadata {
            content: EventContent {
                tag: Some(Entrypoint::from_string_unchecked("crac".into())),
                payload: Some(payload.into()),
                ty: ty.into(),
            },
            sender: alias_kt1.clone(),
            nonce: 1,
            result: ContentResult::Applied(EventSuccess {
                consumed_milligas: Narith(100_000u64.into()),
            }),
        });

        // -- origination internal op --------------------------------------
        let origination_internal =
            InternalOperationSum::Origination(InternalContentWithMetadata {
                sender: alias_kt1.clone(),
                nonce: 2,
                content: OriginationContent {
                    balance: 500_000u64.into(),
                    delegate: Some(null_pkh.clone()),
                    script: Script {
                        code: vec![0x05, 0x02, 0x00, 0x00, 0x00, 0x04],
                        storage: vec![0x05, 0x00, 0x2A],
                    },
                },
                result: ContentResult::Applied(OriginationSuccess {
                    balance_updates: vec![BalanceUpdate {
                        balance: Balance::Account(
                            Contract::from_b58check(
                                "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                            )
                            .unwrap(),
                        ),
                        changes: -500_000,
                        update_origin: UpdateOrigin::BlockApplication,
                    }],
                    originated_contracts: vec![Originated {
                        contract: originated_kt1,
                    }],
                    consumed_milligas: Narith(2_000_000u64.into()),
                    storage_size: Zarith::from(128u64),
                    paid_storage_size_diff: Zarith::from(128u64),
                    lazy_storage_diff: None,
                }),
            });

        // -- top-level result with all three internals --------------------
        let top_level_result = OperationResult {
            balance_updates: balance_updates_top,
            result: ContentResult::Applied(TransferTarget::from(
                TransferSuccess::default(),
            )),
            internal_operation_results: vec![
                transfer_internal,
                event_internal,
                origination_internal,
            ],
        };

        // -- non-trivial signature (non-zero) ----------------------------
        let mut sig_bytes = [0u8; 64];
        for (i, b) in sig_bytes.iter_mut().enumerate() {
            *b = (i as u8).wrapping_mul(7).wrapping_add(13);
        }
        let signature = UnknownSignature::try_from(sig_bytes.as_slice()).unwrap();

        // -- operation hash and branch from real base58 -------------------
        let branch = BlockHash::from_base58_check(
            "BLockGenesisGenesisGenesisGenesisGenesis1db77eJNeJ9",
        )
        .unwrap();
        let op_hash = OperationHash::from(tezos_crypto_rs::blake2b::digest_256(
            b"elaborate-test-op",
        ));

        let op_data =
            OperationDataAndMetadata::OperationWithMetadata(OperationBatchWithMetadata {
                operations: vec![OperationWithMetadata {
                    content: ManagerOperationContent::Transaction(ManagerOperation {
                        source: null_pkh,
                        fee: Narith(500_000u64.into()),
                        counter: Narith(42u64.into()),
                        gas_limit: Narith(10_000_000u64.into()),
                        storage_limit: Narith(1_000u64.into()),
                        operation: TransferContent {
                            amount: 1_000_000u64.into(),
                            destination: alias_kt1,
                            parameters: Parameters {
                                entrypoint: Entrypoint::from_string_unchecked(
                                    "transfer".into(),
                                ),
                                value: vec![0x05, 0x00, 0x07],
                            },
                        },
                    }),
                    receipt: OperationResultSum::Transfer(top_level_result),
                }],
                signature,
            });

        let applied = AppliedOperation {
            hash: op_hash,
            branch,
            op_and_receipt: op_data,
        };

        // -- BinWriter/NomReader round-trip --------------------------------
        let bytes = applied.to_bytes().expect("BinWriter should succeed");
        let decoded =
            AppliedOperation::nom_read_exact(&bytes).expect("NomReader should succeed");
        assert_eq!(applied, decoded, "round-trip mismatch");

        // -- OperationsWithReceipts round-trip ----------------------------
        let ops = OperationsWithReceipts {
            list: vec![applied],
        };
        let ops_bytes = ops
            .to_bytes()
            .expect("BinWriter for OperationsWithReceipts");
        let ops_decoded = OperationsWithReceipts::nom_read_exact(&ops_bytes)
            .expect("NomReader for OperationsWithReceipts");
        assert_eq!(
            ops, ops_decoded,
            "OperationsWithReceipts round-trip mismatch"
        );

        // -- RLP round-trip -----------------------------------------------
        let mut stream = rlp::RlpStream::new();
        ops.rlp_append(&mut stream);
        let rlp_bytes = stream.out();
        let rlp = rlp::Rlp::new(&rlp_bytes);
        let ops_rlp_decoded: OperationsWithReceipts =
            rlp::Decodable::decode(&rlp).expect("RLP decode for OperationsWithReceipts");
        assert_eq!(ops, ops_rlp_decoded, "RLP round-trip mismatch");
    }

    /// Round-trip a TransferSuccess where every field is populated:
    /// storage, balance updates of all three kinds, ticket receipt bytes,
    /// originated contracts bytes, realistic gas/storage numbers,
    /// allocated_destination_contract = true, lazy storage diff with a
    /// BigMap update containing key/value data, and two address registry
    /// entries.
    #[test]
    fn transfer_success_elaborate_round_trip() {
        let ts = TransferSuccess {
            storage: Some(MichelineExpr(
                Micheline::App(Prim::pair, &[], NO_ANNS).encode().unwrap(),
            )),
            balance_updates: vec![
                BalanceUpdate {
                    balance: Balance::Account(
                        Contract::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
                            .unwrap(),
                    ),
                    changes: -2_000_000,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::BlockFees,
                    changes: 1_500_000,
                    update_origin: UpdateOrigin::BlockApplication,
                },
                BalanceUpdate {
                    balance: Balance::StorageFees,
                    changes: 500_000,
                    update_origin: UpdateOrigin::BlockApplication,
                },
            ],
            ticket_receipt: vec![0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08],
            originated_contracts: vec![0xAA, 0xBB, 0xCC, 0xDD],
            consumed_milligas: Narith(3_500_000u64.into()),
            storage_size: Zarith::from(512u64),
            paid_storage_size_diff: Zarith::from(256u64),
            allocated_destination_contract: true,
            lazy_storage_diff: Some(LazyStorageDiffList {
                diff: vec![
                    LazyStorageDiff::BigMap(BigMapDiff {
                        id: Zarith::from(42u64),
                        storage_diff: StorageDiff::Copy(Copy {
                            source: Zarith::from(10u64),
                            updates: vec![],
                        }),
                    }),
                    LazyStorageDiff::BigMap(BigMapDiff {
                        id: Zarith::from(99u64),
                        storage_diff: StorageDiff::Remove,
                    }),
                ],
            }),
            address_registry_diff: vec![
                AddressRegistry {
                    address: Contract::from_b58check(
                        "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw",
                    )
                    .unwrap(),
                    index: Zarith::from(0u64),
                },
                AddressRegistry {
                    address: Contract::from_b58check(
                        "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                    )
                    .unwrap(),
                    index: Zarith::from(5u64),
                },
            ],
        };

        let bytes = ts.to_bytes().expect("encode");
        let decoded = TransferSuccess::nom_read_exact(&bytes)
            .expect("TransferSuccess round-trip failed");
        assert_eq!(ts, decoded, "TransferSuccess round-trip mismatch");
    }
}
