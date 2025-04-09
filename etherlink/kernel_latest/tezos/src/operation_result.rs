// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations

/// The whole module is inspired of `src/proto_alpha/lib_protocol/apply_result.ml` to represent the result of an operation
use std::fmt::Debug;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom as tezos_nom;
use tezos_data_encoding::types::Narith;
use tezos_data_encoding::types::Zarith;
use tezos_enc::BinWriter;
use tezos_nom::NomReader;
use tezos_smart_rollup::types::Contract;
use tezos_smart_rollup::types::{PublicKey, PublicKeyHash};

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum ValidityError {
    InvalidCounter(Narith),
    CantPayFees(Narith),
    EmptyImplicitContract,
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum ManagerError {
    PreviouslyRevealedKey(PublicKey),
    InconsistentHash(PublicKeyHash),
    InconsistentPublicKey(PublicKeyHash),
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum OperationError {
    Validation(ValidityError),
    Manager(ManagerError),
}

impl From<ValidityError> for OperationError {
    fn from(value: ValidityError) -> Self {
        Self::Validation(value)
    }
}

impl From<ManagerError> for OperationError {
    fn from(value: ManagerError) -> Self {
        Self::Manager(value)
    }
}

pub trait ManagerKind {
    type Success: PartialEq + Debug + BinWriter + for<'a> NomReader<'a>;

    fn kind() -> Self;
}

/// Empty struct to implement [ManagerKind] trait for Reveal
#[derive(PartialEq, Debug)]
pub struct Reveal;

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct RevealSuccess {
    pub consumed_gas: Narith,
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct RevealContent {
    pk: PublicKey,
}

impl ManagerKind for Reveal {
    type Success = RevealSuccess;

    fn kind() -> Self {
        Self
    }
}

// Inspired from `src/proto_alpha/lib_protocol/apply_operation_result.ml`
// Still need to implement Backtracked and Skipped
#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub enum OperationStatus<M: ManagerKind> {
    Applied(M::Success),
    Failed(Vec<OperationError>),
}

/// A [Balance] updates can be triggered on different target
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum Balance {
    Account(Contract),
    BlockFees,
}

/// Depending of the sign of [credited], the account is credited or debited
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct BalanceUpdate {
    pub balance: Balance,
    pub credited: Zarith,
}

// Inspired from `src/proto_alpha/lib_protocol/apply_results.ml`
// Still need to implement internal_results
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct ManagerReceipt<M: ManagerKind> {
    pub balance_updates: Vec<BalanceUpdate>,
    pub result: OperationStatus<M>,
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum OperationReceipt {
    RevealReceipt(ManagerReceipt<Reveal>),
}
