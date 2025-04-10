// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

//! Tezos operations

/// The whole module is inspired of `src/proto_alpha/lib_protocol/apply_result.ml` to represent the result of an operation
use std::fmt::Debug;
use tezos_data_encoding::enc as tezos_enc;
use tezos_data_encoding::nom as tezos_nom;
use tezos_data_encoding::types::Narith;
use tezos_enc::BinWriter;
use tezos_nom::NomReader;
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
