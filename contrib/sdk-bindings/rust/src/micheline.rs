// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::types::BigInt;
use crate::Error;
use mir::{
    ast::{annotations::Annotation as MirAnnotation, micheline::Micheline as MirMicheline},
    gas::Gas,
    lexer::Prim as MirPrim,
    parser::Parser,
};
use std::sync::Arc;
use typed_arena::Arena;

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

impl From<&MirPrim> for Prim {
    fn from(prim: &MirPrim) -> Self {
        match prim {
            MirPrim::parameter => Self::K_parameter,
            MirPrim::storage => Self::K_storage,
            MirPrim::code => Self::K_code,
            MirPrim::False => Self::D_False,
            MirPrim::Elt => Self::D_Elt,
            MirPrim::Left => Self::D_Left,
            MirPrim::None => Self::D_None,
            MirPrim::Pair => Self::D_Pair,
            MirPrim::Right => Self::D_Right,
            MirPrim::Some => Self::D_Some,
            MirPrim::True => Self::D_True,
            MirPrim::Unit => Self::D_Unit,
            MirPrim::PACK => Self::I_PACK,
            MirPrim::UNPACK => Self::I_UNPACK,
            MirPrim::BLAKE2B => Self::I_BLAKE2B,
            MirPrim::SHA256 => Self::I_SHA256,
            MirPrim::SHA512 => Self::I_SHA512,
            MirPrim::ABS => Self::I_ABS,
            MirPrim::ADD => Self::I_ADD,
            MirPrim::AMOUNT => Self::I_AMOUNT,
            MirPrim::AND => Self::I_AND,
            MirPrim::BALANCE => Self::I_BALANCE,
            MirPrim::CAR => Self::I_CAR,
            MirPrim::CDR => Self::I_CDR,
            MirPrim::CHECK_SIGNATURE => Self::I_CHECK_SIGNATURE,
            MirPrim::COMPARE => Self::I_COMPARE,
            MirPrim::CONCAT => Self::I_CONCAT,
            MirPrim::CONS => Self::I_CONS,
            MirPrim::CREATE_ACCOUNT => Self::I_CREATE_ACCOUNT,
            MirPrim::CREATE_CONTRACT => Self::I_CREATE_CONTRACT,
            MirPrim::IMPLICIT_ACCOUNT => Self::I_IMPLICIT_ACCOUNT,
            MirPrim::DIP => Self::I_DIP,
            MirPrim::DROP => Self::I_DROP,
            MirPrim::DUP => Self::I_DUP,
            MirPrim::EDIV => Self::I_EDIV,
            MirPrim::EMPTY_MAP => Self::I_EMPTY_MAP,
            MirPrim::EMPTY_SET => Self::I_EMPTY_SET,
            MirPrim::EQ => Self::I_EQ,
            MirPrim::EXEC => Self::I_EXEC,
            MirPrim::FAILWITH => Self::I_FAILWITH,
            MirPrim::GE => Self::I_GE,
            MirPrim::GET => Self::I_GET,
            MirPrim::GT => Self::I_GT,
            MirPrim::HASH_KEY => Self::I_HASH_KEY,
            MirPrim::IF => Self::I_IF,
            MirPrim::IF_CONS => Self::I_IF_CONS,
            MirPrim::IF_LEFT => Self::I_IF_LEFT,
            MirPrim::IF_NONE => Self::I_IF_NONE,
            MirPrim::INT => Self::I_INT,
            MirPrim::LAMBDA => Self::I_LAMBDA,
            MirPrim::LE => Self::I_LE,
            MirPrim::LEFT => Self::I_LEFT,
            MirPrim::LOOP => Self::I_LOOP,
            MirPrim::LSL => Self::I_LSL,
            MirPrim::LSR => Self::I_LSR,
            MirPrim::LT => Self::I_LT,
            MirPrim::MAP => Self::I_MAP,
            MirPrim::MEM => Self::I_MEM,
            MirPrim::MUL => Self::I_MUL,
            MirPrim::NEG => Self::I_NEG,
            MirPrim::NEQ => Self::I_NEQ,
            MirPrim::NIL => Self::I_NIL,
            MirPrim::NONE => Self::I_NONE,
            MirPrim::NOT => Self::I_NOT,
            MirPrim::NOW => Self::I_NOW,
            MirPrim::OR => Self::I_OR,
            MirPrim::PAIR => Self::I_PAIR,
            MirPrim::PUSH => Self::I_PUSH,
            MirPrim::RIGHT => Self::I_RIGHT,
            MirPrim::SIZE => Self::I_SIZE,
            MirPrim::SOME => Self::I_SOME,
            MirPrim::SOURCE => Self::I_SOURCE,
            MirPrim::SENDER => Self::I_SENDER,
            MirPrim::SELF => Self::I_SELF,
            MirPrim::STEPS_TO_QUOTA => Self::I_STEPS_TO_QUOTA,
            MirPrim::SUB => Self::I_SUB,
            MirPrim::SWAP => Self::I_SWAP,
            MirPrim::TRANSFER_TOKENS => Self::I_TRANSFER_TOKENS,
            MirPrim::SET_DELEGATE => Self::I_SET_DELEGATE,
            MirPrim::UNIT => Self::I_UNIT,
            MirPrim::UPDATE => Self::I_UPDATE,
            MirPrim::XOR => Self::I_XOR,
            MirPrim::ITER => Self::I_ITER,
            MirPrim::LOOP_LEFT => Self::I_LOOP_LEFT,
            MirPrim::ADDRESS => Self::I_ADDRESS,
            MirPrim::CONTRACT => Self::I_CONTRACT,
            MirPrim::ISNAT => Self::I_ISNAT,
            MirPrim::CAST => Self::I_CAST,
            MirPrim::RENAME => Self::I_RENAME,
            MirPrim::bool => Self::T_bool,
            MirPrim::contract => Self::T_contract,
            MirPrim::int => Self::T_int,
            MirPrim::key => Self::T_key,
            MirPrim::key_hash => Self::T_key_hash,
            MirPrim::lambda => Self::T_lambda,
            MirPrim::list => Self::T_list,
            MirPrim::map => Self::T_map,
            MirPrim::big_map => Self::T_big_map,
            MirPrim::nat => Self::T_nat,
            MirPrim::option => Self::T_option,
            MirPrim::or => Self::T_or,
            MirPrim::pair => Self::T_pair,
            MirPrim::set => Self::T_set,
            MirPrim::signature => Self::T_signature,
            MirPrim::string => Self::T_string,
            MirPrim::bytes => Self::T_bytes,
            MirPrim::mutez => Self::T_mutez,
            MirPrim::timestamp => Self::T_timestamp,
            MirPrim::unit => Self::T_unit,
            MirPrim::operation => Self::T_operation,
            MirPrim::address => Self::T_address,
            MirPrim::SLICE => Self::I_SLICE,
            MirPrim::DIG => Self::I_DIG,
            MirPrim::DUG => Self::I_DUG,
            MirPrim::EMPTY_BIG_MAP => Self::I_EMPTY_BIG_MAP,
            MirPrim::APPLY => Self::I_APPLY,
            MirPrim::chain_id => Self::T_chain_id,
            MirPrim::CHAIN_ID => Self::I_CHAIN_ID,
            MirPrim::LEVEL => Self::I_LEVEL,
            MirPrim::SELF_ADDRESS => Self::I_SELF_ADDRESS,
            MirPrim::never => Self::T_never,
            MirPrim::NEVER => Self::I_NEVER,
            MirPrim::UNPAIR => Self::I_UNPAIR,
            MirPrim::VOTING_POWER => Self::I_VOTING_POWER,
            MirPrim::TOTAL_VOTING_POWER => Self::I_TOTAL_VOTING_POWER,
            MirPrim::KECCAK => Self::I_KECCAK,
            MirPrim::SHA3 => Self::I_SHA3,
            MirPrim::PAIRING_CHECK => Self::I_PAIRING_CHECK,
            MirPrim::bls12_381_g1 => Self::T_bls12_381_g1,
            MirPrim::bls12_381_g2 => Self::T_bls12_381_g2,
            MirPrim::bls12_381_fr => Self::T_bls12_381_fr,
            MirPrim::sapling_state => Self::T_sapling_state,
            MirPrim::sapling_transaction_deprecated => Self::T_sapling_transaction_deprecated,
            MirPrim::SAPLING_EMPTY_STATE => Self::I_SAPLING_EMPTY_STATE,
            MirPrim::SAPLING_VERIFY_UPDATE => Self::I_SAPLING_VERIFY_UPDATE,
            MirPrim::ticket => Self::T_ticket,
            MirPrim::TICKET_DEPRECATED => Self::I_TICKET_DEPRECATED,
            MirPrim::READ_TICKET => Self::I_READ_TICKET,
            MirPrim::SPLIT_TICKET => Self::I_SPLIT_TICKET,
            MirPrim::JOIN_TICKETS => Self::I_JOIN_TICKETS,
            MirPrim::GET_AND_UPDATE => Self::I_GET_AND_UPDATE,
            MirPrim::chest => Self::T_chest,
            MirPrim::chest_key => Self::T_chest_key,
            MirPrim::OPEN_CHEST => Self::I_OPEN_CHEST,
            MirPrim::VIEW => Self::I_VIEW,
            MirPrim::view => Self::K_view,
            MirPrim::constant => Self::H_constant,
            MirPrim::SUB_MUTEZ => Self::I_SUB_MUTEZ,
            MirPrim::tx_rollup_l2_address => Self::T_tx_rollup_l2_address,
            MirPrim::MIN_BLOCK_TIME => Self::I_MIN_BLOCK_TIME,
            MirPrim::sapling_transaction => Self::T_sapling_transaction,
            MirPrim::EMIT => Self::I_EMIT,
            MirPrim::Lambda_rec => Self::D_Lambda_rec,
            MirPrim::LAMBDA_REC => Self::I_LAMBDA_REC,
            MirPrim::TICKET => Self::I_TICKET,
            MirPrim::BYTES => Self::I_BYTES,
            MirPrim::NAT => Self::I_NAT,
            MirPrim::Ticket => Self::D_Ticket,
            MirPrim::IS_IMPLICIT_ACCOUNT => Self::I_IS_IMPLICIT_ACCOUNT,
            MirPrim::INDEX_ADDRESS => Self::I_INDEX_ADDRESS,
            MirPrim::GET_ADDRESS_INDEX => Self::I_GET_ADDRESS_INDEX,
            // Unstable primitives (they are not part of a released protocol)
            MirPrim::Transfer_tokens => Self::UNSTABLE_Transfer_tokens,
            MirPrim::Set_delegate => Self::UNSTABLE_Set_delegate,
            MirPrim::Create_contract => Self::UNSTABLE_Create_contract,
            MirPrim::Emit => Self::UNSTABLE_Emit,
        }
    }
}

impl From<&Prim> for MirPrim {
    fn from(prim: &Prim) -> Self {
        match prim {
            Prim::K_parameter => Self::parameter,
            Prim::K_storage => Self::storage,
            Prim::K_code => Self::code,
            Prim::D_False => Self::False,
            Prim::D_Elt => Self::Elt,
            Prim::D_Left => Self::Left,
            Prim::D_None => Self::None,
            Prim::D_Pair => Self::Pair,
            Prim::D_Right => Self::Right,
            Prim::D_Some => Self::Some,
            Prim::D_True => Self::True,
            Prim::D_Unit => Self::Unit,
            Prim::I_PACK => Self::PACK,
            Prim::I_UNPACK => Self::UNPACK,
            Prim::I_BLAKE2B => Self::BLAKE2B,
            Prim::I_SHA256 => Self::SHA256,
            Prim::I_SHA512 => Self::SHA512,
            Prim::I_ABS => Self::ABS,
            Prim::I_ADD => Self::ADD,
            Prim::I_AMOUNT => Self::AMOUNT,
            Prim::I_AND => Self::AND,
            Prim::I_BALANCE => Self::BALANCE,
            Prim::I_CAR => Self::CAR,
            Prim::I_CDR => Self::CDR,
            Prim::I_CHECK_SIGNATURE => Self::CHECK_SIGNATURE,
            Prim::I_COMPARE => Self::COMPARE,
            Prim::I_CONCAT => Self::CONCAT,
            Prim::I_CONS => Self::CONS,
            Prim::I_CREATE_ACCOUNT => Self::CREATE_ACCOUNT,
            Prim::I_CREATE_CONTRACT => Self::CREATE_CONTRACT,
            Prim::I_IMPLICIT_ACCOUNT => Self::IMPLICIT_ACCOUNT,
            Prim::I_DIP => Self::DIP,
            Prim::I_DROP => Self::DROP,
            Prim::I_DUP => Self::DUP,
            Prim::I_EDIV => Self::EDIV,
            Prim::I_EMPTY_MAP => Self::EMPTY_MAP,
            Prim::I_EMPTY_SET => Self::EMPTY_SET,
            Prim::I_EQ => Self::EQ,
            Prim::I_EXEC => Self::EXEC,
            Prim::I_FAILWITH => Self::FAILWITH,
            Prim::I_GE => Self::GE,
            Prim::I_GET => Self::GET,
            Prim::I_GT => Self::GT,
            Prim::I_HASH_KEY => Self::HASH_KEY,
            Prim::I_IF => Self::IF,
            Prim::I_IF_CONS => Self::IF_CONS,
            Prim::I_IF_LEFT => Self::IF_LEFT,
            Prim::I_IF_NONE => Self::IF_NONE,
            Prim::I_INT => Self::INT,
            Prim::I_LAMBDA => Self::LAMBDA,
            Prim::I_LE => Self::LE,
            Prim::I_LEFT => Self::LEFT,
            Prim::I_LOOP => Self::LOOP,
            Prim::I_LSL => Self::LSL,
            Prim::I_LSR => Self::LSR,
            Prim::I_LT => Self::LT,
            Prim::I_MAP => Self::MAP,
            Prim::I_MEM => Self::MEM,
            Prim::I_MUL => Self::MUL,
            Prim::I_NEG => Self::NEG,
            Prim::I_NEQ => Self::NEQ,
            Prim::I_NIL => Self::NIL,
            Prim::I_NONE => Self::NONE,
            Prim::I_NOT => Self::NOT,
            Prim::I_NOW => Self::NOW,
            Prim::I_OR => Self::OR,
            Prim::I_PAIR => Self::PAIR,
            Prim::I_PUSH => Self::PUSH,
            Prim::I_RIGHT => Self::RIGHT,
            Prim::I_SIZE => Self::SIZE,
            Prim::I_SOME => Self::SOME,
            Prim::I_SOURCE => Self::SOURCE,
            Prim::I_SENDER => Self::SENDER,
            Prim::I_SELF => Self::SELF,
            Prim::I_STEPS_TO_QUOTA => Self::STEPS_TO_QUOTA,
            Prim::I_SUB => Self::SUB,
            Prim::I_SWAP => Self::SWAP,
            Prim::I_TRANSFER_TOKENS => Self::TRANSFER_TOKENS,
            Prim::I_SET_DELEGATE => Self::SET_DELEGATE,
            Prim::I_UNIT => Self::UNIT,
            Prim::I_UPDATE => Self::UPDATE,
            Prim::I_XOR => Self::XOR,
            Prim::I_ITER => Self::ITER,
            Prim::I_LOOP_LEFT => Self::LOOP_LEFT,
            Prim::I_ADDRESS => Self::ADDRESS,
            Prim::I_CONTRACT => Self::CONTRACT,
            Prim::I_ISNAT => Self::ISNAT,
            Prim::I_CAST => Self::CAST,
            Prim::I_RENAME => Self::RENAME,
            Prim::T_bool => Self::bool,
            Prim::T_contract => Self::contract,
            Prim::T_int => Self::int,
            Prim::T_key => Self::key,
            Prim::T_key_hash => Self::key_hash,
            Prim::T_lambda => Self::lambda,
            Prim::T_list => Self::list,
            Prim::T_map => Self::map,
            Prim::T_big_map => Self::big_map,
            Prim::T_nat => Self::nat,
            Prim::T_option => Self::option,
            Prim::T_or => Self::or,
            Prim::T_pair => Self::pair,
            Prim::T_set => Self::set,
            Prim::T_signature => Self::signature,
            Prim::T_string => Self::string,
            Prim::T_bytes => Self::bytes,
            Prim::T_mutez => Self::mutez,
            Prim::T_timestamp => Self::timestamp,
            Prim::T_unit => Self::unit,
            Prim::T_operation => Self::operation,
            Prim::T_address => Self::address,
            Prim::I_SLICE => Self::SLICE,
            Prim::I_DIG => Self::DIG,
            Prim::I_DUG => Self::DUG,
            Prim::I_EMPTY_BIG_MAP => Self::EMPTY_BIG_MAP,
            Prim::I_APPLY => Self::APPLY,
            Prim::T_chain_id => Self::chain_id,
            Prim::I_CHAIN_ID => Self::CHAIN_ID,
            Prim::I_LEVEL => Self::LEVEL,
            Prim::I_SELF_ADDRESS => Self::SELF_ADDRESS,
            Prim::T_never => Self::never,
            Prim::I_NEVER => Self::NEVER,
            Prim::I_UNPAIR => Self::UNPAIR,
            Prim::I_VOTING_POWER => Self::VOTING_POWER,
            Prim::I_TOTAL_VOTING_POWER => Self::TOTAL_VOTING_POWER,
            Prim::I_KECCAK => Self::KECCAK,
            Prim::I_SHA3 => Self::SHA3,
            Prim::I_PAIRING_CHECK => Self::PAIRING_CHECK,
            Prim::T_bls12_381_g1 => Self::bls12_381_g1,
            Prim::T_bls12_381_g2 => Self::bls12_381_g2,
            Prim::T_bls12_381_fr => Self::bls12_381_fr,
            Prim::T_sapling_state => Self::sapling_state,
            Prim::T_sapling_transaction_deprecated => Self::sapling_transaction_deprecated,
            Prim::I_SAPLING_EMPTY_STATE => Self::SAPLING_EMPTY_STATE,
            Prim::I_SAPLING_VERIFY_UPDATE => Self::SAPLING_VERIFY_UPDATE,
            Prim::T_ticket => Self::ticket,
            Prim::I_TICKET_DEPRECATED => Self::TICKET_DEPRECATED,
            Prim::I_READ_TICKET => Self::READ_TICKET,
            Prim::I_SPLIT_TICKET => Self::SPLIT_TICKET,
            Prim::I_JOIN_TICKETS => Self::JOIN_TICKETS,
            Prim::I_GET_AND_UPDATE => Self::GET_AND_UPDATE,
            Prim::T_chest => Self::chest,
            Prim::T_chest_key => Self::chest_key,
            Prim::I_OPEN_CHEST => Self::OPEN_CHEST,
            Prim::I_VIEW => Self::VIEW,
            Prim::K_view => Self::view,
            Prim::H_constant => Self::constant,
            Prim::I_SUB_MUTEZ => Self::SUB_MUTEZ,
            Prim::T_tx_rollup_l2_address => Self::tx_rollup_l2_address,
            Prim::I_MIN_BLOCK_TIME => Self::MIN_BLOCK_TIME,
            Prim::T_sapling_transaction => Self::sapling_transaction,
            Prim::I_EMIT => Self::EMIT,
            Prim::D_Lambda_rec => Self::Lambda_rec,
            Prim::I_LAMBDA_REC => Self::LAMBDA_REC,
            Prim::I_TICKET => Self::TICKET,
            Prim::I_BYTES => Self::BYTES,
            Prim::I_NAT => Self::NAT,
            Prim::D_Ticket => Self::Ticket,
            Prim::I_IS_IMPLICIT_ACCOUNT => Self::IS_IMPLICIT_ACCOUNT,
            Prim::I_INDEX_ADDRESS => Self::INDEX_ADDRESS,
            Prim::I_GET_ADDRESS_INDEX => Self::GET_ADDRESS_INDEX,
            // Unstable primitives (they are not part of a released protocol)
            Prim::UNSTABLE_Transfer_tokens => Self::Transfer_tokens,
            Prim::UNSTABLE_Set_delegate => Self::Set_delegate,
            Prim::UNSTABLE_Create_contract => Self::Create_contract,
            Prim::UNSTABLE_Emit => Self::Emit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, uniffi::Enum)]
pub enum Annotation {
    Special(String),
    Field(String),
    Variable(String),
    Type(String),
}

impl From<&MirAnnotation<'_>> for Annotation {
    fn from(annotation: &MirAnnotation<'_>) -> Self {
        match annotation {
            MirAnnotation::Special(s) => Self::Special(s.clone().into_owned()),
            MirAnnotation::Field(s) => Self::Field(s.clone().into_owned()),
            MirAnnotation::Variable(s) => Self::Variable(s.clone().into_owned()),
            MirAnnotation::Type(s) => Self::Type(s.clone().into_owned()),
        }
    }
}

impl From<&Annotation> for MirAnnotation<'_> {
    fn from(annotation: &Annotation) -> Self {
        match annotation {
            Annotation::Special(s) => Self::Special(s.clone().into()),
            Annotation::Field(s) => Self::Field(s.clone().into()),
            Annotation::Variable(s) => Self::Variable(s.clone().into()),
            Annotation::Type(s) => Self::Type(s.clone().into()),
        }
    }
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

impl From<&MirMicheline<'_>> for Micheline {
    fn from(micheline: &MirMicheline<'_>) -> Self {
        match micheline {
            MirMicheline::Int(big_int) => Self::Int {
                big_int: Arc::new(BigInt(big_int.clone())),
            },
            MirMicheline::String(string) => Self::String {
                string: string.to_owned(),
            },
            MirMicheline::Bytes(bytes) => Self::Bytes {
                bytes: bytes.to_owned(),
            },
            MirMicheline::App(prim, seq, annots) => Self::App {
                prim: prim.into(),
                seq: seq.iter().map(Self::from).collect(),
                annots: annots.iter().map(Annotation::from).collect(),
            },
            MirMicheline::Seq(seq) => Self::Seq {
                seq: seq.iter().map(Self::from).collect(),
            },
        }
    }
}

impl Micheline {
    fn to_mir_micheline<'a>(&self, arena: &'a Arena<MirMicheline<'a>>) -> MirMicheline<'a> {
        match self {
            Self::Int { big_int } => MirMicheline::Int(big_int.0.clone()),
            Self::String { string } => MirMicheline::String(string.clone()),
            Self::Bytes { bytes } => MirMicheline::Bytes(bytes.clone()),
            Self::App { prim, seq, annots } => {
                let prim = prim.into();
                let seq = seq
                    .iter()
                    .map(|micheline| micheline.to_mir_micheline(arena))
                    .collect::<Vec<MirMicheline>>();
                let annots = annots.iter().map(MirAnnotation::from).collect();
                MirMicheline::App(prim, arena.alloc_extend(seq), annots)
            }
            Self::Seq { seq } => {
                let seq = seq
                    .iter()
                    .map(|micheline| micheline.to_mir_micheline(arena))
                    .collect::<Vec<MirMicheline>>();
                MirMicheline::Seq(arena.alloc_extend(seq))
            }
        }
    }

    pub fn parse(script: &str) -> Result<Self, Error> {
        let parser = Parser::new();
        let micheline = &parser
            .parse(script)
            .map_err(|err| Error::Parsing(err.to_string()))?;
        Ok(micheline.into())
    }

    pub fn encode(&self) -> Result<Vec<u8>, Error> {
        let arena = Arena::new();
        // Encoding is performed off-chain: gas accounting does not apply.
        self.to_mir_micheline(&arena)
            .encode(&mut Gas::unmetered())
            .map_err(|_| Error::Encoding("out of gas".to_string()))?
            .map_err(|err| Error::Encoding(err.to_string()))
    }
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

    /// Build a micheline value.
    pub fn parse(&self, micheline: &str) -> Result<Micheline, Error> {
        Micheline::parse(micheline)
    }

    /// Encode micheline into bytes.
    pub fn encode(&self, micheline: &Micheline) -> Result<Vec<u8>, Error> {
        micheline.encode()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
    octez-client convert data "
    { parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }
    " from Michelson to binary
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

        let parsed_micheline = micheline_manager
            .parse("{ parameter unit ; storage unit ; code { CDR ; NIL operation ; PAIR } }")
            .expect("Micheline parsing");
        assert!(
            micheline_manager.equal_micheline(&micheline, &parsed_micheline),
            "Parsed micheline must be equal to the manually constructed micheline"
        );

        let raw_micheline = micheline_manager.encode(&micheline).unwrap();
        let bytes =
            hex::decode("02000000170500036c0501036c050202000000080317053d036d0342").unwrap();
        assert_eq!(
            bytes, raw_micheline,
            "Micheline must be encoded into the expected bytes"
        );
    }

    /*
    octez-client convert data '(Pair :foo "string" 0 0x00)' from Michelson to binary
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

        let parsed_micheline = micheline_manager
            .parse(r#"(Pair :foo "string" 0 0x00)"#)
            .expect("Micheline parsing");
        assert!(
            micheline_manager.equal_micheline(&micheline, &parsed_micheline),
            "Parsed micheline must be equal to the manually constructed micheline"
        );

        let raw_micheline = micheline_manager.encode(&micheline).unwrap();
        let bytes =
            hex::decode("0907000000130100000006737472696e6700000a0000000100000000043a666f6f")
                .unwrap();
        assert_eq!(
            bytes, raw_micheline,
            "Micheline must be encoded into the expected bytes"
        );
    }
}
