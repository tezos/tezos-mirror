// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations

/// The whole module is inspired of `src/proto_alpha/lib_protocol/apply_result.ml` to represent the result of an operation
/// In Tezlink, operation is equivalent to manager operation because there is no other type of operation that interests us.
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
pub enum ApplyOperationError {
    PreviouslyRevealedKey(PublicKey),
    InconsistentHash(PublicKeyHash),
    InconsistentPublicKey(PublicKeyHash),
}

#[derive(Debug, PartialEq, Eq, NomReader, BinWriter)]
pub enum OperationError {
    Validation(ValidityError),
    Apply(ApplyOperationError),
}

impl From<ValidityError> for OperationError {
    fn from(value: ValidityError) -> Self {
        Self::Validation(value)
    }
}

impl From<ApplyOperationError> for OperationError {
    fn from(value: ApplyOperationError) -> Self {
        Self::Apply(value)
    }
}

pub trait OperationKind {
    type Success: PartialEq + Debug + BinWriter + for<'a> NomReader<'a>;

    fn kind() -> Self;
}

/// Empty struct to implement [OperationKind] trait for Reveal
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

#[derive(PartialEq, Debug, Clone)]
pub struct Empty;

impl BinWriter for Empty {
    fn bin_write(&self, _: &mut Vec<u8>) -> tezos_enc::BinResult {
        Ok(())
    }
}

impl NomReader<'_> for Empty {
    fn nom_read(input: &'_ [u8]) -> tezos_nom::NomResult<'_, Self> {
        Ok((input, Self))
    }
}

#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub struct TransferSuccess {
    pub storage: Option<Empty>,
    pub lazy_storage_diff: Option<Empty>,
    pub balance_updates: Vec<BalanceUpdate>,
    pub ticket_receipt: Vec<Empty>,
    pub originated_contracts: Vec<Empty>,
    pub consumed_gas: Narith,
    pub storage_size: Narith,
    pub paid_storage_size_diff: Narith,
    pub allocated_destination_contract: bool,
}

/// Empty struct to implement [OperationKind] trait for Transfer
#[derive(PartialEq, Debug)]
pub struct Transfer;

impl OperationKind for Transfer {
    type Success = TransferSuccess;

    fn kind() -> Self {
        Self
    }
}

impl OperationKind for Reveal {
    type Success = RevealSuccess;

    fn kind() -> Self {
        Self
    }
}

// Inspired from `operation_result` in `src/proto_alpha/lib_protocol/apply_operation_result.ml`
// Still need to implement Backtracked and Skipped
#[derive(PartialEq, Debug, BinWriter, NomReader)]
pub enum ContentResult<M: OperationKind> {
    Applied(M::Success),
    Failed(Vec<OperationError>),
}

/// A [Balance] updates can be triggered on different target
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum Balance {
    Account(Contract),
    Block,
}

/// Depending of the sign of [changes], the balance is credited or debited
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct BalanceUpdate {
    pub balance: Balance,
    pub changes: Zarith,
}

// Inspired from `Manager_operation_result` case in 'kind contents_result type
// from `src/proto_alpha/lib_protocol/apply_results.ml` file.
// Still need to implement internal_results
#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub struct OperationResult<M: OperationKind> {
    pub balance_updates: Vec<BalanceUpdate>,
    pub result: ContentResult<M>,
}

#[derive(PartialEq, Debug, NomReader, BinWriter)]
pub enum OperationResultSum {
    Reveal(OperationResult<Reveal>),
    Transfer(OperationResult<Transfer>),
}

pub fn produce_operation_result<M: OperationKind>(
    result: Result<M::Success, OperationError>,
) -> OperationResult<M> {
    match result {
        Ok(success) => OperationResult {
            balance_updates: vec![],
            result: ContentResult::Applied(success),
        },
        Err(operation_error) => OperationResult {
            balance_updates: vec![],
            result: ContentResult::Failed(vec![operation_error]),
        },
    }
}
