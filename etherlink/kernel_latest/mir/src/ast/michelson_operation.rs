// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `operation` values.

use std::rc::Rc;

use super::{Address, ContractScript, FieldAnnotation, Micheline, Or, Type, TypedValue};
use tezos_crypto_rs::{hash::ContractKt1Hash, public_key_hash::PublicKeyHash};

/// Representation of token transfer operation, created by `TRANSFER_TOKENS`
/// instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TransferTokens<'a> {
    /// Transfer parameter.
    pub param: TypedValue<'a>,
    /// Transfer destination.
    pub destination_address: Address,
    /// Transfer amount.
    pub amount: i64,
}

/// Representation of set delegate operation, created by `SET_DELEGATE` instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SetDelegate(pub Option<PublicKeyHash>);

/// Representation of emit operation, created by `EMIT` instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Emit<'a> {
    /// Event tag.
    pub tag: Option<FieldAnnotation<'a>>,
    /// Event value.
    pub value: TypedValue<'a>,

    /// Event type.
    ///
    /// Here an `Or` type is used, (instead of a single `Type` or `Micheline`
    /// field), for two reasons:
    ///
    /// 1. Emit type annotation is special when explicit in that it must
    ///    preserve annotations. That is currently only representable as [Micheline].
    /// 2. If the type is implicit, and comes from the stack, we must use the
    ///    [Type] from the typechecker. Converting it to [Micheline] is
    ///    complicated, as [Micheline] requires an arena.
    pub arg_ty: Or<Type, Micheline<'a>>,
}

/// Representation of create contract operation, created by `CREATE_CONTRACT` instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CreateContract<'a> {
    /// Contract's optional delegate.
    pub delegate: Option<PublicKeyHash>,
    /// Contract's inital balance.
    pub amount: i64,
    /// Contract's initial storage.
    pub storage: TypedValue<'a>,
    /// Contract's typechecked code.
    pub code: Rc<ContractScript<'a>>,
    /// Raw [Micheline] representation of the contract's code. The operation
    /// encoding must survive round-trip via `PACK`/`UNPACK`, so raw code has to
    /// be stored.
    pub micheline_code: &'a Micheline<'a>,
    /// The address at which the contract must be originated.
    pub address: ContractKt1Hash,
}

/// Enum corresponding to values of the `operation` Michelson type.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operation<'a> {
    /// Transfer tokens operation.
    TransferTokens(TransferTokens<'a>),
    /// Set delegate operation.
    SetDelegate(SetDelegate),
    /// Emit operation.
    Emit(Emit<'a>),
    /// Create contract operation.
    CreateContract(CreateContract<'a>),
}

/// Operation with a nonce attached.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OperationInfo<'a> {
    /// Operation.
    pub operation: Operation<'a>,
    /// Nonce.
    pub counter: u128,
}
