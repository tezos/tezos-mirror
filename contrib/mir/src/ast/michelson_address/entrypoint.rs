// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Structures and utilities for [Tezos
//! entrypoints](https://docs.tezos.com/smart-contracts/entrypoints).

use std::collections::HashMap;

use crate::ast::Type;

use tezos_protocol::entrypoint;

use super::ByteReprError;

impl From<entrypoint::ByteReprError> for ByteReprError {
    fn from(entrypoint::ByteReprError::WrongFormat(err): entrypoint::ByteReprError) -> Self {
        Self::WrongFormat(err)
    }
}

/// Structure representing address entrypoint on a Tezos address, in other
/// words, the part after `%` in `KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo`.
/// Tezos entrypoints are ASCII strings of at most 31 characters long.
pub type Entrypoint = entrypoint::Entrypoint;

/// Enum representing each layer to achieve a given entrypoint
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Direction {
    /// Left
    Left,
    /// Right
    Right,
}

/// A structure mapping from entrypoints to their types. This is simply an alias
/// for a [HashMap].
pub type Entrypoints = HashMap<Entrypoint, Type>;

/// NB: default entrypoint is represented as literal "default", because it
/// affects comparison for addresses.
pub const DEFAULT_EP_NAME: &str = entrypoint::DEFAULT_EP_NAME;
