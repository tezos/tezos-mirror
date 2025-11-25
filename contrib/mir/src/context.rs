// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! The "outer context" required for typechecking and interpreting Michelson.

#![allow(clippy::type_complexity)]
use crate::ast::big_map::{BigMapId, InMemoryLazyStorage, LazyStorage, LazyStorageError};
use crate::ast::micheline::IntoMicheline;
use crate::ast::michelson_address::AddressHash;
use crate::ast::Entrypoints;
use crate::ast::{Micheline, View};
use crate::ast::{Type, TypedValue};
use crate::gas::Gas;
use crate::typechecker::MichelineView;
use num_bigint::{BigInt, BigUint};
use std::collections::HashMap;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_crypto_rs::{hash::OperationHash, public_key_hash::PublicKeyHash};
use typed_arena::Arena;

#[allow(missing_docs)]
pub trait TypecheckingCtx<'a> {
    fn gas(&mut self) -> &mut Gas;

    fn lookup_entrypoints(
        &self,
        address: &AddressHash,
    ) -> Option<HashMap<crate::ast::Entrypoint, crate::ast::Type>>;

    /// Get key and value types of the map.
    /// This returns None if the map with such ID is not present in the storage.
    fn big_map_get_type(&mut self, id: &BigMapId)
        -> Result<Option<(Type, Type)>, LazyStorageError>;
}

/// Typechecking context used to typecheck pushable values during the
/// typechecking of the PUSH instruction. Since neither contracts nor
/// big_maps are pushable, the lookup_contract and big_map_get_type
/// methods have trivial implementations.
pub struct PushableTypecheckingContext<'a> {
    /// Gas counter
    pub gas: &'a mut Gas,
}

impl<'a, 'b> TypecheckingCtx<'a> for PushableTypecheckingContext<'b> {
    fn gas(&mut self) -> &mut Gas {
        self.gas
    }

    fn lookup_entrypoints(
        &self,
        _address: &AddressHash,
    ) -> Option<HashMap<crate::ast::Entrypoint, crate::ast::Type>> {
        None
    }

    fn big_map_get_type(
        &mut self,
        _id: &BigMapId,
    ) -> Result<Option<(Type, Type)>, LazyStorageError> {
        Ok(None)
    }
}

#[allow(missing_docs)]
pub trait CtxTrait<'a>: TypecheckingCtx<'a> {
    fn amount(&self) -> i64;

    fn balance(&self) -> i64;

    fn level(&self) -> BigUint;

    fn sender(&self) -> AddressHash;

    fn source(&self) -> PublicKeyHash;

    fn min_block_time(&self) -> BigUint;

    fn chain_id(&self) -> tezos_crypto_rs::hash::ChainId;

    fn self_address(&self) -> AddressHash;

    fn voting_power(&self, pkh: &PublicKeyHash) -> BigUint;

    fn now(&self) -> BigInt;

    fn total_voting_power(&self) -> BigUint;

    fn operation_group_hash(&self) -> [u8; 32];

    fn origination_counter(&mut self) -> u32;

    fn operation_counter(&mut self) -> u128;

    fn lazy_storage(&mut self) -> Box<&mut dyn LazyStorage<'a>>;

    fn lookup_view_and_storage(
        &self,
        contract: ContractKt1Hash,
        name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Option<(MichelineView<Micheline<'a>>, (Micheline<'a>, Vec<u8>))>;
}

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
    pub source: PublicKeyHash,
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
    pub lookup_entrypoints: Box<dyn Fn(&AddressHash) -> Option<Entrypoints>>,
    /// A map of contract addresses to their views. It only
    /// needs to work with smart contract, defaulting to [None] otherwise.
    pub views: HashMap<AddressHash, HashMap<String, View<'a>>>,
    /// A map of contract addresses to their storage. It only
    /// needs to work with smart contract, defaulting to [None] otherwise.
    pub storage: HashMap<AddressHash, (Type, TypedValue<'a>)>,
    /// A function that maps public key hashes (i.e. effectively implicit
    /// account addresses) to their corresponding voting powers. Note that if
    /// you provide a custom function here, you also must define
    /// [Self::total_voting_power] to be consistent with your function! See also
    /// [Self::set_voting_powers]. Defaults to returning `0` for any address.
    pub voting_powers: Box<dyn Fn(&PublicKeyHash) -> BigUint>,
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
    pub big_map_storage: InMemoryLazyStorage<'a>,
    origination_counter: u32,
    operation_counter: u128,
}

impl<'a> Ctx<'a> {
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
        self.lookup_entrypoints = Box::new(move |ah| map.get(ah).cloned());
    }

    /// Set a resonable implementation for [Self::big_map_storage] by providing
    /// something that implements the [LazyStorage] trait.
    pub fn set_big_map_storage(&mut self, v: InMemoryLazyStorage<'a>) {
        self.big_map_storage = v;
    }

    /// Set a reasonable implementation for [Self::voting_powers] and a
    /// consistent value for [Self::total_voting_power] by providing something
    /// that converts into  [`HashMap<PublicKeyHash, BigUint>`], mapping key hashes to
    /// voting powers. If a given key hash is unspecified, its voting power is
    /// assumed to be `0`. [Self::total_voting_power] is set to the sum of all
    /// values.
    pub fn set_voting_powers(&mut self, v: impl Into<HashMap<PublicKeyHash, BigUint>>) {
        let map: HashMap<PublicKeyHash, BigUint> = v.into();
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
            chain_id: tezos_crypto_rs::hash::ChainId::try_from(vec![0xf3, 0xd4, 0x85, 0x54])
                .unwrap(),
            self_address: "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi".try_into().unwrap(),
            sender: "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi".try_into().unwrap(),
            source: "tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP".try_into().unwrap(),
            lookup_entrypoints: Box::new(|_| None),
            voting_powers: Box::new(|_| 0u32.into()),
            total_voting_power: 0u32.into(),
            big_map_storage: InMemoryLazyStorage::new(),
            views: HashMap::new(),
            storage: HashMap::new(),
            operation_counter: 0,
            operation_group_hash: OperationHash::from_base58_check(
                "onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D",
                // "2EouXpxkPGxAvVKCpdCJnfp2wEMWR7Up5DERRZ1Yo99xCLjkCVuq",
            )
            .unwrap()
            .as_ref()
            .try_into()
            .unwrap(),
            origination_counter: 0,
        }
    }
}

impl<'a> TypecheckingCtx<'a> for Ctx<'a> {
    fn gas(&mut self) -> &mut Gas {
        &mut self.gas
    }

    fn lookup_entrypoints(
        &self,
        address: &AddressHash,
    ) -> Option<HashMap<crate::ast::Entrypoint, crate::ast::Type>> {
        (self.lookup_entrypoints)(address)
    }

    fn big_map_get_type(
        &mut self,
        id: &BigMapId,
    ) -> Result<Option<(Type, Type)>, LazyStorageError> {
        self.big_map_storage.big_map_get_type(id)
    }
}

impl<'a> CtxTrait<'a> for Ctx<'a> {
    fn amount(&self) -> i64 {
        self.amount
    }

    fn balance(&self) -> i64 {
        self.balance
    }

    fn level(&self) -> BigUint {
        self.level.clone()
    }

    fn sender(&self) -> AddressHash {
        self.sender.clone()
    }

    fn source(&self) -> PublicKeyHash {
        self.source.clone()
    }

    fn min_block_time(&self) -> BigUint {
        self.min_block_time.clone()
    }

    fn chain_id(&self) -> tezos_crypto_rs::hash::ChainId {
        self.chain_id.clone()
    }

    fn self_address(&self) -> AddressHash {
        self.self_address.clone()
    }

    fn voting_power(&self, pkh: &PublicKeyHash) -> BigUint {
        (self.voting_powers)(pkh)
    }

    fn now(&self) -> BigInt {
        self.now.clone()
    }

    fn total_voting_power(&self) -> BigUint {
        self.total_voting_power.clone()
    }

    fn operation_group_hash(&self) -> [u8; 32] {
        self.operation_group_hash
    }

    fn origination_counter(&mut self) -> u32 {
        self.origination_counter += 1;
        self.origination_counter
    }

    fn operation_counter(&mut self) -> u128 {
        self.operation_counter += 1;
        self.operation_counter
    }

    fn lazy_storage(&mut self) -> Box<&mut dyn LazyStorage<'a>> {
        Box::new(&mut self.big_map_storage)
    }

    fn lookup_view_and_storage(
        &self,
        contract: ContractKt1Hash,
        view_name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Option<(MichelineView<Micheline<'a>>, (Micheline<'a>, Vec<u8>))> {
        let addr = AddressHash::Kt1(contract);
        let contract_view = self.views.get(&addr)?.get(view_name)?;
        let view = MichelineView {
            input_type: contract_view
                .input_type
                .into_micheline_optimized_legacy(arena),
            output_type: contract_view
                .output_type
                .into_micheline_optimized_legacy(arena),
            code: contract_view.code.clone(),
        };
        let (storage_ty, storage) = self.storage.get(&addr)?;
        let mich_storage_ty = storage_ty.into_micheline_optimized_legacy(arena);
        let mich_storage = storage.clone().into_micheline_optimized_legacy(arena);
        Some((view, (mich_storage_ty, mich_storage.encode())))
    }
}
