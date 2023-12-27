/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! The "outer context" required for typechecking and interpreting Michelson.

#![allow(clippy::type_complexity)]
use crate::ast::big_map::{InMemoryLazyStorage, LazyStorage};
use crate::ast::michelson_address::entrypoint::Entrypoints;
use crate::ast::michelson_address::AddressHash;
use crate::ast::michelson_key_hash::KeyHash;
use crate::gas::Gas;
use num_bigint::{BigInt, BigUint};
use std::collections::HashMap;
use tezos_crypto_rs::hash::OperationListHash;

/// [Ctx] includes "outer context" required for typechecking and interpreting
/// Michelson.
pub struct Ctx<'a> {
    /// [Gas] counter. Defaults to [`Gas::default()`]
    pub gas: Gas,
    /// Transfer amount that initiated this execution. Defaults to `0`
    pub amount: i64,
    /// Contract balance. Defaults to `0`.
    pub balance: i64,
    /// Current blockchain level. Defaults to `0`.
    pub level: BigUint,
    /// Transfer sender, i.e. the contract that initiated the current internal
    /// transaction. The result of the `SENDER` instruction. Defaults to
    /// `KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi`.
    pub sender: AddressHash,
    /// Transfer source, i.e. the contract that initiated and signed the current
    /// transaction. The result of the `SOURCE` instruction. Note that in a
    /// regular blockchain, this is necessarily an implicit account. Defaults to
    /// `tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP`.
    pub source: AddressHash,
    /// Minimal block time in seconds. The result of the `MIN_BLOCK_TIME`
    /// instruciton. Defaults to `1`.
    pub min_block_time: BigUint,
    /// Identifier of the chain where the script is being executed. The result
    /// of the `CHAIN_ID` instruction. Defaults to `NetXynUjJNZm7wi`.
    pub chain_id: tezos_crypto_rs::hash::ChainId,
    /// Address of the contract being executed. The result of the `SELF_ADDRESS`
    /// instruction. Defaults to `KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi`.
    pub self_address: AddressHash,
    /// A function that maps contract addresses to their entrypoints. It only
    /// needs to work with smart contract and smart rollup addresses, as
    /// implicit accounts don't really have entrypoints. For a given address,
    /// the function must return either [None], meaning the contract doesn't
    /// exist, or [`Some(entrypoints)`] with the map of its entrypoints. See
    /// also [Self::set_known_contracts]. Defaults to returning [None] for any
    /// address.
    pub lookup_contract: Box<dyn FnMut(&AddressHash) -> Option<Entrypoints>>,
    /// A function that maps public key hashes (i.e. effectively implicit
    /// account addresses) to their corresponding voting powers. Note that if
    /// you provide a custom function here, you also must define
    /// [Self::total_voting_power] to be consistent with your function! See also
    /// [Self::set_voting_powers]. Defaults to returning `0` for any address.
    pub voting_powers: Box<dyn Fn(&KeyHash) -> BigUint>,
    /// The minimal injection time for the current block, as a unix timestamp
    /// (in seconds). Defaults to `0`.
    pub now: BigInt,
    /// Total voting power. Note that if you are setting this manually, you must
    /// also provide a consistent implementation for [Self::voting_powers]. See
    /// also [Self::set_voting_powers]. Defaults to `0`.
    pub total_voting_power: BigUint,
    /// Hash for the current operation group. This will be used to generate
    /// contract addresses for newly-created contracts (via `CREATE_CONTRACT`
    /// instruction). Defaults to
    /// `onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D`.
    pub operation_group_hash: [u8; 32],
    // NB: lifetime is mandatory if we want to use types implementing with
    // references inside for LazyStorage, and we do due to how Runtime is passed
    // as &mut
    /// Storage for `big_map`s. By default uses [InMemoryLazyStorage], but can
    /// admit a custom implementation of [LazyStorage] trait. Defaults to a new,
    /// empty, [InMemoryLazyStorage].
    pub big_map_storage: Box<dyn LazyStorage<'a> + 'a>,
    origination_counter: u32,
    operation_counter: u128,
}

impl Ctx<'_> {
    /// Increment the internal operation counter and return it. Used as a nonce
    /// for operations.
    pub fn operation_counter(&mut self) -> u128 {
        self.operation_counter += 1;
        self.operation_counter
    }

    /// Forcibly set the operation counter. This is mostly useful for testing purposes.
    pub fn set_operation_counter(&mut self, v: u128) {
        self.operation_counter = v;
    }

    /// Set a reasonable implementation for [Self::lookup_contract] by providing
    /// something that can convert to [`HashMap<AddressHash, Entrypoints>`].
    pub fn set_known_contracts(&mut self, v: impl Into<HashMap<AddressHash, Entrypoints>>) {
        let map = v.into();
        self.lookup_contract = Box::new(move |ah| map.get(ah).cloned());
    }

    /// Set a reasonable implementation for [Self::voting_powers] and a
    /// consistent value for [Self::total_voting_power] by providing something
    /// that converts into  [`HashMap<KeyHash, BigUint>`], mapping key hashes to
    /// voting powers. If a given key hash is unspecified, its voting power is
    /// assumed to be `0`. [Self::total_voting_power] is set to the sum of all
    /// values.
    pub fn set_voting_powers(&mut self, v: impl Into<HashMap<KeyHash, BigUint>>) {
        let map: HashMap<KeyHash, BigUint> = v.into();
        self.total_voting_power = map.values().sum();
        self.voting_powers = Box::new(move |x| map.get(x).unwrap_or(&0u32.into()).clone());
    }

    /// Increment origination counter and return its new value. Used as a nonce
    /// to generate unique contract addresses for the `CREATE_CONTRACT`
    /// instruction.
    pub fn origination_counter(&mut self) -> u32 {
        self.origination_counter += 1;
        self.origination_counter
    }

    /// Forcibly set an origination counter. Mostly useful in tests.
    pub fn set_origination_counter(&mut self, v: u32) {
        self.origination_counter = v;
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
            operation_group_hash: OperationListHash::from_base58_check(
                "onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D",
            )
            .unwrap()
            .0
            .as_slice()
            .try_into()
            .unwrap(),
            origination_counter: 0,
        }
    }
}
