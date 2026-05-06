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

    fn operation_group_hash(&self) -> &OperationHash;

    fn origination_counter(&mut self) -> u32;

    fn operation_counter(&mut self) -> u128;

    fn lazy_storage(&mut self) -> Box<&mut dyn LazyStorage<'a>>;

    /// Looks up a view definition together with the contract's storage and balance.
    ///
    /// Returns the view named `name` from `contract`, along with:
    /// - the Micheline storage type,
    /// - the packed (serialized) storage,
    /// - the contract balance in mutez.
    ///
    /// Returns [`Ok(None)`] if the contract is not originated or the named
    /// view does not exist. Returns [`Err`] if the lookup itself fails —
    /// host I/O error, corrupted contract code/storage, or a serialization
    /// failure when producing the view's storage bytes. Implementations
    /// must not collapse these latter cases into [`Ok(None)`], as that
    /// would silently impersonate a missing view.
    fn lookup_view_storage_balance(
        &self,
        contract: &ContractKt1Hash,
        name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Result<
        Option<(MichelineView<Micheline<'a>>, Micheline<'a>, Vec<u8>, i64)>,
        tezos_data_encoding::enc::BinError,
    >;

    /// Override the execution context for a view call.
    /// Sets `self_address`, `sender`, `amount`, and `balance`.
    fn set_view_context(
        &mut self,
        self_address: AddressHash,
        sender: AddressHash,
        amount: i64,
        balance: i64,
    );
}

/// Standalone implementation of the MIR execution context, used for tests,
/// examples, and the TZT runner. For the real Etherlink kernel context, see
/// `tezos_execution::mir_ctx::Ctx`.
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
    /// A map of contract addresses to their entrypoints. It only needs to
    /// work with smart contract and smart rollup addresses, as implicit
    /// accounts don't really have entrypoints. For a given address, the map
    /// returns either [None], meaning the contract doesn't exist, or
    /// [`Some(entrypoints)`] with the map of its entrypoints. See also
    /// [Self::set_known_contracts]. Defaults to empty.
    lookup_entrypoints: HashMap<AddressHash, Entrypoints>,
    /// A map of contract addresses to their views. It only
    /// needs to work with smart contract, defaulting to [None] otherwise.
    pub views: HashMap<AddressHash, HashMap<String, View<'a>>>,
    /// A map of contract addresses to their storage. It only
    /// needs to work with smart contract, defaulting to [None] otherwise.
    pub storage: HashMap<AddressHash, (Type, TypedValue<'a>)>,
    /// A map of public key hashes (i.e. effectively implicit account
    /// addresses) to their corresponding voting powers. If a given key hash
    /// is absent, its voting power is assumed to be `0`. See also
    /// [Self::set_voting_powers], which also sets [Self::total_voting_power]
    /// consistently. Defaults to empty.
    voting_powers: HashMap<PublicKeyHash, BigUint>,
    /// The minimal injection time for the current block, as a unix timestamp
    /// (in seconds). Defaults to `0`.
    pub now: BigInt,
    /// Total voting power. Automatically set by [Self::set_voting_powers] to
    /// the sum of all values in [Self::voting_powers]. Defaults to `0`.
    total_voting_power: BigUint,
    /// Hash for the current operation group. This will be used to generate
    /// contract addresses for newly-created contracts (via `CREATE_CONTRACT`
    /// instruction). Defaults to
    /// `onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D`.
    pub operation_group_hash: OperationHash,
    // NB: lifetime is mandatory if we want to use types implementing with
    // references inside for LazyStorage, and we do due to how Runtime is passed
    // as &mut
    /// Storage for `big_map`s. By default uses [InMemoryLazyStorage], but can
    /// admit a custom implementation of [LazyStorage] trait. Defaults to a new,
    /// empty, [InMemoryLazyStorage].
    pub big_map_storage: InMemoryLazyStorage<'a>,
    /// Origination counter. Incremented for each `CREATE_CONTRACT`. Defaults to `0`.
    origination_counter: u32,
    /// Operation counter used as a nonce for operations. Defaults to `0`.
    operation_counter: u128,
    /// A map of contract KT1 addresses to their balances (in mutez).
    /// Used by [`lookup_view_storage_balance`](CtxTrait::lookup_view_storage_balance) to
    /// return the target contract's balance during view execution. Defaults to empty.
    pub balances: HashMap<ContractKt1Hash, i64>,
}

impl<'a> Ctx<'a> {
    /// Forcibly set the operation counter. This is mostly useful for testing purposes.
    pub fn set_operation_counter(&mut self, v: u128) {
        self.operation_counter = v;
    }

    /// Set [Self::lookup_entrypoints] by providing something that can convert
    /// to [`HashMap<AddressHash, Entrypoints>`].
    pub fn set_known_contracts(&mut self, v: impl Into<HashMap<AddressHash, Entrypoints>>) {
        self.lookup_entrypoints = v.into();
    }

    /// Set [Self::big_map_storage] by providing an [InMemoryLazyStorage].
    pub fn set_big_map_storage(&mut self, v: InMemoryLazyStorage<'a>) {
        self.big_map_storage = v;
    }

    /// Set [Self::voting_powers] and a consistent value for
    /// [Self::total_voting_power] by providing something that converts into
    /// [`HashMap<PublicKeyHash, BigUint>`]. [Self::total_voting_power] is set
    /// to the sum of all values.
    pub fn set_voting_powers(&mut self, v: impl Into<HashMap<PublicKeyHash, BigUint>>) {
        let map: HashMap<PublicKeyHash, BigUint> = v.into();
        self.total_voting_power = map.values().sum();
        self.voting_powers = map;
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
            lookup_entrypoints: HashMap::new(),
            voting_powers: HashMap::new(),
            total_voting_power: 0u32.into(),
            big_map_storage: InMemoryLazyStorage::new(),
            views: HashMap::new(),
            storage: HashMap::new(),
            operation_counter: 0,
            operation_group_hash: OperationHash::from_base58_check(
                "onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D",
                // "2EouXpxkPGxAvVKCpdCJnfp2wEMWR7Up5DERRZ1Yo99xCLjkCVuq",
            )
            .unwrap(),
            origination_counter: 0,
            balances: HashMap::new(),
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
        self.lookup_entrypoints.get(address).cloned()
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
        self.voting_powers
            .get(pkh)
            .cloned()
            .unwrap_or(BigUint::ZERO)
    }

    fn now(&self) -> BigInt {
        self.now.clone()
    }

    fn total_voting_power(&self) -> BigUint {
        self.total_voting_power.clone()
    }

    fn operation_group_hash(&self) -> &OperationHash {
        &self.operation_group_hash
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

    fn lookup_view_storage_balance(
        &self,
        contract: &ContractKt1Hash,
        view_name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Result<
        Option<(MichelineView<Micheline<'a>>, Micheline<'a>, Vec<u8>, i64)>,
        tezos_data_encoding::enc::BinError,
    > {
        let addr = AddressHash::Kt1(contract.clone());
        let Some(contract_view) = self.views.get(&addr).and_then(|m| m.get(view_name)) else {
            return Ok(None);
        };
        let view = MichelineView {
            input_type: contract_view
                .input_type
                .into_micheline_optimized_legacy(arena),
            output_type: contract_view
                .output_type
                .into_micheline_optimized_legacy(arena),
            code: contract_view.code.clone(),
        };
        let Some((storage_ty, storage)) = self.storage.get(&addr) else {
            return Ok(None);
        };
        let mich_storage_ty = storage_ty.into_micheline_optimized_legacy(arena);
        let mich_storage = storage.clone().into_micheline_optimized_legacy(arena);
        let view_balance = self.balances.get(contract).cloned().unwrap_or(0);
        let encoded = mich_storage.encode()?;
        Ok(Some((view, mich_storage_ty, encoded, view_balance)))
    }

    fn set_view_context(
        &mut self,
        self_address: AddressHash,
        sender: AddressHash,
        amount: i64,
        balance: i64,
    ) {
        self.self_address = self_address;
        self.sender = sender;
        self.amount = amount;
        self.balance = balance;
    }
}
