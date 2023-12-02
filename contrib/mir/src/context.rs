#![allow(clippy::type_complexity)]
use crate::ast::michelson_address::entrypoint::Entrypoints;
use crate::ast::michelson_address::AddressHash;
use crate::gas::Gas;
use num_bigint::BigUint;
use std::collections::HashMap;

pub struct Ctx {
    pub gas: Gas,
    pub amount: i64,
    pub balance: i64,
    pub level: BigUint,
    pub chain_id: tezos_crypto_rs::hash::ChainId,
    pub self_address: AddressHash,
    pub lookup_contract: Box<dyn FnMut(&AddressHash) -> Option<Entrypoints>>,
    operation_counter: u128,
}

impl Ctx {
    pub fn operation_counter(&mut self) -> u128 {
        self.operation_counter += 1;
        self.operation_counter
    }

    pub fn set_operation_counter(&mut self, v: u128) {
        self.operation_counter = v;
    }

    pub fn set_known_contracts(&mut self, v: impl Into<HashMap<AddressHash, Entrypoints>>) {
        let map = v.into();
        self.lookup_contract = Box::new(move |ah| map.get(ah).cloned());
    }
}

impl Default for Ctx {
    fn default() -> Self {
        Ctx {
            gas: Gas::default(),
            balance: 0,
            amount: 0,
            level: 0u32.into(),
            // the default chain id is NetXynUjJNZm7wi, which is also the default chain id of octez-client in mockup mode
            chain_id: tezos_crypto_rs::hash::ChainId(vec![0xf3, 0xd4, 0x85, 0x54]),
            self_address: "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi".try_into().unwrap(),
            lookup_contract: Box::new(|_| None),
            operation_counter: 0,
        }
    }
}
