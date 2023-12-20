#![allow(clippy::type_complexity)]
use crate::ast::big_map::{InMemoryLazyStorage, LazyStorage};
use crate::ast::michelson_address::entrypoint::Entrypoints;
use crate::ast::michelson_address::AddressHash;
use crate::ast::michelson_key_hash::KeyHash;
use crate::gas::Gas;
use num_bigint::{BigInt, BigUint};
use std::collections::HashMap;

pub struct Ctx<'a> {
    pub gas: Gas,
    pub amount: i64,
    pub balance: i64,
    pub level: BigUint,
    pub sender: AddressHash,
    pub source: AddressHash,
    pub min_block_time: BigUint,
    pub chain_id: tezos_crypto_rs::hash::ChainId,
    pub self_address: AddressHash,
    pub lookup_contract: Box<dyn FnMut(&AddressHash) -> Option<Entrypoints>>,
    pub voting_powers: Box<dyn Fn(&KeyHash) -> BigUint>,
    pub now: BigInt,
    pub total_voting_power: BigUint,
    // NB: lifetime is mandatory if we want to use types implementing with
    // references inside for LazyStorage, and we do due to how Runtime is passed
    // as &mut
    pub big_map_storage: Box<dyn LazyStorage<'a> + 'a>,
    operation_counter: u128,
}

impl Ctx<'_> {
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

    pub fn set_voting_powers(&mut self, v: impl Into<HashMap<KeyHash, BigUint>>) {
        let map: HashMap<KeyHash, BigUint> = v.into();
        self.total_voting_power = map.values().sum();
        self.voting_powers = Box::new(move |x| map.get(x).unwrap_or(&0u32.into()).clone());
    }
}

impl Default for Ctx<'_> {
    fn default() -> Self {
        Ctx {
            gas: Gas::default(),
            balance: 0,
            amount: 0,
            level: 0u32.into(),
            now: 0i32.into(),
            min_block_time: 1u32.into(),
            // the default chain id is NetXynUjJNZm7wi, which is also the default chain id of octez-client in mockup mode
            chain_id: tezos_crypto_rs::hash::ChainId(vec![0xf3, 0xd4, 0x85, 0x54]),
            self_address: "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi".try_into().unwrap(),
            sender: "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi".try_into().unwrap(),
            source: "tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP".try_into().unwrap(),
            lookup_contract: Box::new(|_| None),
            voting_powers: Box::new(|_| 0u32.into()),
            total_voting_power: 0u32.into(),
            big_map_storage: Box::new(InMemoryLazyStorage::new()),
            operation_counter: 0,
        }
    }
}
