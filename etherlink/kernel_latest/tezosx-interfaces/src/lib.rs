// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::error::Error;

use primitive_types::U256;
use tezos_evm_runtime::runtime::Runtime;

pub trait RuntimeInterface {
    type AddressType;
    type Error: Error;
    // TODO: Probably need to pass more data to initialize
    // a real operation when creating an alias.
    fn generate_alias(
        &self,
        host: &mut impl Runtime,
        native_address: &[u8],
    ) -> Result<Self::AddressType, Self::Error>;
    // This is a just a placeholder for now to show how the
    //interface would look like.
    // TODO: Probably need to pass and return more data to
    // initialize a real operation when we will implement this.
    // Amounts should have been subtracted from the sender
    // before calling this function.
    fn call(
        &self,
        host: &mut impl Runtime,
        from: &Self::AddressType,
        to: &Self::AddressType,
        amount: U256,
        data: &[u8],
    ) -> Result<Vec<u8>, Self::Error>;

    fn encode_address(&self, address: &Self::AddressType)
        -> Result<Vec<u8>, Self::Error>;

    fn decode_address(&self, data: &[u8]) -> Result<Self::AddressType, Self::Error>;
}

pub enum RuntimeId {
    Tezos = 0,
    Ethereum = 1,
}

impl From<RuntimeId> for u8 {
    fn from(value: RuntimeId) -> Self {
        match value {
            RuntimeId::Tezos => 0,
            RuntimeId::Ethereum => 1,
        }
    }
}

impl TryFrom<u8> for RuntimeId {
    type Error = &'static str;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(RuntimeId::Tezos),
            1 => Ok(RuntimeId::Ethereum),
            _ => Err("Invalid RuntimeId value"),
        }
    }
}
