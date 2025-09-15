// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::types::BigInt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, uniffi::Enum)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms, missing_docs)]
pub enum Prim {
    K_parameter,
    K_storage,
    K_code,
    D_False,
    D_Elt,
    D_Left,
    D_None,
    D_Pair,
    D_Right,
    D_Some,
    D_True,
    D_Unit,
    I_PACK,
    I_UNPACK,
    I_BLAKE2B,
    I_SHA256,
    I_SHA512,
    I_ABS,
    I_ADD,
    I_AMOUNT,
    I_AND,
    I_BALANCE,
    I_CAR,
    I_CDR,
    I_CHECK_SIGNATURE,
    I_COMPARE,
    I_CONCAT,
    I_CONS,
    I_CREATE_ACCOUNT,
    I_CREATE_CONTRACT,
    I_IMPLICIT_ACCOUNT,
    I_DIP,
    I_DROP,
    I_DUP,
    I_EDIV,
    I_EMPTY_MAP,
    I_EMPTY_SET,
    I_EQ,
    I_EXEC,
    I_FAILWITH,
    I_GE,
    I_GET,
    I_GT,
    I_HASH_KEY,
    I_IF,
    I_IF_CONS,
    I_IF_LEFT,
    I_IF_NONE,
    I_INT,
    I_LAMBDA,
    I_LE,
    I_LEFT,
    I_LOOP,
    I_LSL,
    I_LSR,
    I_LT,
    I_MAP,
    I_MEM,
    I_MUL,
    I_NEG,
    I_NEQ,
    I_NIL,
    I_NONE,
    I_NOT,
    I_NOW,
    I_OR,
    I_PAIR,
    I_PUSH,
    I_RIGHT,
    I_SIZE,
    I_SOME,
    I_SOURCE,
    I_SENDER,
    I_SELF,
    I_STEPS_TO_QUOTA,
    I_SUB,
    I_SWAP,
    I_TRANSFER_TOKENS,
    I_SET_DELEGATE,
    I_UNIT,
    I_UPDATE,
    I_XOR,
    I_ITER,
    I_LOOP_LEFT,
    I_ADDRESS,
    I_CONTRACT,
    I_ISNAT,
    I_CAST,
    I_RENAME,
    T_bool,
    T_contract,
    T_int,
    T_key,
    T_key_hash,
    T_lambda,
    T_list,
    T_map,
    T_big_map,
    T_nat,
    T_option,
    T_or,
    T_pair,
    T_set,
    T_signature,
    T_string,
    T_bytes,
    T_mutez,
    T_timestamp,
    T_unit,
    T_operation,
    T_address,
    I_SLICE,
    I_DIG,
    I_DUG,
    I_EMPTY_BIG_MAP,
    I_APPLY,
    T_chain_id,
    I_CHAIN_ID,
    I_LEVEL,
    I_SELF_ADDRESS,
    T_never,
    I_NEVER,
    I_UNPAIR,
    I_VOTING_POWER,
    I_TOTAL_VOTING_POWER,
    I_KECCAK,
    I_SHA3,
    I_PAIRING_CHECK,
    T_bls12_381_g1,
    T_bls12_381_g2,
    T_bls12_381_fr,
    T_sapling_state,
    T_sapling_transaction_deprecated,
    I_SAPLING_EMPTY_STATE,
    I_SAPLING_VERIFY_UPDATE,
    T_ticket,
    I_TICKET_DEPRECATED,
    I_READ_TICKET,
    I_SPLIT_TICKET,
    I_JOIN_TICKETS,
    I_GET_AND_UPDATE,
    T_chest,
    T_chest_key,
    I_OPEN_CHEST,
    I_VIEW,
    K_view,
    H_constant,
    I_SUB_MUTEZ,
    T_tx_rollup_l2_address,
    I_MIN_BLOCK_TIME,
    T_sapling_transaction,
    I_EMIT,
    D_Lambda_rec,
    I_LAMBDA_REC,
    I_TICKET,
    I_BYTES,
    I_NAT,
    D_Ticket,
    I_IS_IMPLICIT_ACCOUNT,
    I_INDEX_ADDRESS,
    I_GET_ADDRESS_INDEX,
    // Unstable primitives (they are not part of a released protocol)
    UNSTABLE_Transfer_tokens,
    UNSTABLE_Set_delegate,
    UNSTABLE_Create_contract,
    UNSTABLE_Emit,
}

#[derive(Debug, Clone, PartialEq, Eq, uniffi::Enum)]
pub enum Annotation {
    Special(String),
    Field(String),
    Variable(String),
    Type(String),
}

#[derive(Debug, Clone, Eq, PartialEq, uniffi::Enum)]
pub enum Micheline {
    Int {
        big_int: Arc<BigInt>,
    },
    String {
        string: String,
    },
    Bytes {
        bytes: Vec<u8>,
    },
    App {
        prim: Prim,
        seq: Vec<Micheline>,
        annots: Vec<Annotation>,
    },
    Seq {
        seq: Vec<Micheline>,
    },
}

// Because uniffi does not allow bindings of methods implemented on
// structures tagged as `uniffi::Enum` (this restriction also applies
// to the traits `Debug`, `Display`, and `Eq`), a dedicated manager
// for the `Micheline` enum has been introduced to be able to bind the
// features associated with it.
#[derive(uniffi::Object)]
struct MichelineManager;

#[uniffi::export]
impl MichelineManager {
    #[uniffi::constructor]
    pub fn new() -> Self {
        Self
    }

    /// Check that two micheline are equal.
    pub fn equal_micheline(&self, micheline_1: &Micheline, micheline_2: &Micheline) -> bool {
        micheline_1 == micheline_2
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    parameter unit;
    storage unit;
    code {CDR; NIL operation; PAIR};
    */
    #[test]
    fn build_basic_script_micheline() {
        let micheline = Micheline::Seq {
            seq: vec![
                Micheline::App {
                    prim: Prim::K_parameter,
                    seq: vec![Micheline::App {
                        prim: Prim::T_unit,
                        seq: vec![],
                        annots: vec![],
                    }],
                    annots: vec![],
                },
                Micheline::App {
                    prim: Prim::K_storage,
                    seq: vec![Micheline::App {
                        prim: Prim::T_unit,
                        seq: vec![],
                        annots: vec![],
                    }],
                    annots: vec![],
                },
                Micheline::App {
                    prim: Prim::K_code,
                    seq: vec![Micheline::Seq {
                        seq: vec![
                            Micheline::App {
                                prim: Prim::I_CDR,
                                seq: vec![],
                                annots: vec![],
                            },
                            Micheline::App {
                                prim: Prim::I_NIL,
                                seq: vec![Micheline::App {
                                    prim: Prim::T_operation,
                                    seq: vec![],
                                    annots: vec![],
                                }],
                                annots: vec![],
                            },
                            Micheline::App {
                                prim: Prim::I_PAIR,
                                seq: vec![],
                                annots: vec![],
                            },
                        ],
                    }],
                    annots: vec![],
                },
            ],
        };

        let micheline_manager = MichelineManager::new();

        assert!(
            micheline_manager.equal_micheline(&micheline, &micheline),
            "Micheline must be equal to itself"
        );
    }

    /*
    (Pair :foo "string" 0 0x00)
    */
    #[test]
    fn build_simple_data_micheline() {
        let micheline = Micheline::App {
            prim: Prim::D_Pair,
            seq: vec![
                Micheline::String {
                    string: "string".to_owned(),
                },
                Micheline::Int {
                    big_int: Arc::new(BigInt::from_int(0.into())),
                },
                Micheline::Bytes {
                    bytes: hex::decode("00").unwrap(),
                },
            ],
            annots: vec![Annotation::Type("foo".to_owned())],
        };

        let micheline_manager = MichelineManager::new();

        assert!(
            micheline_manager.equal_micheline(&micheline, &micheline),
            "Micheline must be equal to itself"
        );
    }
}
