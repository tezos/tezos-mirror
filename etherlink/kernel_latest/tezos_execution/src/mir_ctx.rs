// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::collections::BTreeMap;

use crate::account_storage::{
    Code, TezlinkAccount, TezlinkOriginatedAccount, TezosImplicitAccount,
    TezosOriginatedAccount,
};
use crate::address::OriginationNonce;
use crate::context::{big_maps::*, Context};
use crate::get_contract_entrypoint;
use mir::parser::Parser;
use mir::typechecker::{MichelineContractScript, MichelineView};
use mir::{
    ast::{
        big_map::{BigMapId, LazyStorage, LazyStorageError},
        AddressHash, IntoMicheline, Micheline, PublicKeyHash, Type, TypedValue,
    },
    context::{CtxTrait, TypecheckingCtx},
    gas::Gas,
};
use num_bigint::{BigInt, BigUint};
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{ChainId, ContractKt1Hash, OperationHash, ScriptExprHash};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::{Narith, Zarith};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::host::RuntimeError;
use tezos_smart_rollup::types::Timestamp;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_storage::{read_nom_value, read_optional_nom_value, store_bin};
use tezos_tezlink::enc_wrappers::BlockNumber;
use tezos_tezlink::lazy_storage_diff::{
    Alloc, BigMapDiff, Copy, LazyStorageDiff, LazyStorageDiffList, StorageDiff, Update,
};
use tezos_tezlink::operation_result::TransferError;
use typed_arena::Arena;

pub struct InterpretContext {
    lazy_storage_size_diff: Zarith,
}

impl InterpretContext {
    pub fn new() -> Self {
        Self {
            lazy_storage_size_diff: 0.into(),
        }
    }

    /// Accumulate `delta` into the lazy-storage size diff. No-op on
    /// temporary big-maps.
    pub fn record_lazy_storage_size_diff(&mut self, id: &BigMapId, delta: &Zarith) {
        if !id.is_temporary() {
            self.lazy_storage_size_diff.0 += &delta.0;
        }
    }
}

impl Default for InterpretContext {
    fn default() -> Self {
        Self::new()
    }
}

pub struct TcCtx<'operation, Host: StorageV1, C: Context> {
    pub host: &'operation mut Host,
    pub context: &'operation C,
    pub operation_gas: &'operation mut crate::gas::TezlinkOperationGas,
    pub big_map_diff: BTreeMap<Zarith, StorageDiff>,
    pub interpret_context: InterpretContext,
    pub next_temporary_id: &'operation mut BigMapId,
}

pub struct OperationCtx<'operation, A: TezosImplicitAccount> {
    // In reality, 'source' and 'origination_nonce' have
    // a 'batch lifetime. Downgrade it to an 'operation
    // lifetime is not a problem for the compiler.
    // However, it could be misleading in terms of comprehension
    pub source: &'operation A,
    pub origination_nonce: &'operation mut OriginationNonce,
    pub counter: &'operation mut u128,
    // 'level', 'now' and 'chain_id' should outlive operation in reality
    // So having them downcast to 'operation is not a problem
    pub level: &'operation BlockNumber,
    pub now: &'operation Timestamp,
    pub chain_id: &'operation ChainId,
    /// Raw bytes of the source account's public key (from validation).
    pub source_public_key: &'operation [u8],
    pub crac_chain_depth: u32,
    /// Originator of the inbound CRAC being serviced — carried in
    /// `X-Tezos-Source` — or `None` for a top-level Michelson transaction.
    /// May be `Contract::Originated(KT1)` (an EVM alias) or
    /// `Contract::Implicit(pkh)` (a native Michelson implicit account that
    /// re-enters Michelson via EVM). The gateway re-injects it (translated)
    /// as the outbound `X-Tezos-Source` so `tx.origin` stays invariant
    /// across an `EVM -> Michelson -> EVM` round-trip (L2-1363).
    pub crac_origin: Option<Contract>,
}

pub struct ExecCtx {
    pub sender: AddressHash,
    pub amount: i64,
    pub self_address: AddressHash,
    pub balance: i64,
    pub contract_account: TezlinkOriginatedAccount,
}

pub struct Ctx<
    'a,
    'operation,
    Host: StorageV1,
    C: Context,
    R: tezosx_interfaces::Registry,
> {
    pub tc_ctx: &'a mut TcCtx<'operation, Host, C>,
    pub exec_ctx: ExecCtx,
    pub operation_ctx: &'a mut OperationCtx<'operation, C::ImplicitAccountType>,
    pub journal: &'a mut tezosx_journal::TezosXJournal,
    pub registry: &'a R,
}

pub struct BlockCtx<'block> {
    pub level: &'block BlockNumber,
    pub now: &'block Timestamp,
    pub chain_id: &'block ChainId,
}

fn address_from_contract(contract: Contract) -> AddressHash {
    match contract {
        Contract::Originated(kt1) => AddressHash::Kt1(kt1),
        Contract::Implicit(hash) => AddressHash::Implicit(hash),
    }
}

impl ExecCtx {
    pub fn create(
        host: &mut impl StorageV1,
        sender_account: &impl TezlinkAccount,
        dest_account: &impl TezosOriginatedAccount,
        amount: &Narith,
    ) -> Result<Self, TransferError> {
        let sender = address_from_contract(sender_account.contract());
        let amount = amount.0.clone().try_into().map_err(
            |err: num_bigint::TryFromBigIntError<num_bigint::BigUint>| {
                TransferError::MirAmountToNarithError(err.to_string())
            },
        )?;
        let self_address = address_from_contract(dest_account.contract());
        let balance = dest_account
            .balance(host)
            .map_err(|_| TransferError::FailedToFetchSenderBalance)?;
        let balance = balance.0.try_into().map_err(
            |err: num_bigint::TryFromBigIntError<num_bigint::BigUint>| {
                TransferError::MirAmountToNarithError(err.to_string())
            },
        )?;
        let contract_account = TezlinkOriginatedAccount {
            path: dest_account.path().clone(),
            kt1: dest_account.kt1().clone(),
        };
        Ok(Self {
            sender,
            amount,
            self_address,
            balance,
            contract_account,
        })
    }
}

impl<'a, Host: StorageV1, C: Context> TypecheckingCtx<'a> for TcCtx<'a, Host, C> {
    fn gas(&mut self) -> &mut mir::gas::Gas {
        &mut self.operation_gas.remaining
    }

    fn lookup_entrypoints(
        &mut self,
        address: &AddressHash,
    ) -> Option<std::collections::HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
        get_contract_entrypoint(
            self.host,
            self.context,
            address,
            &mut self.operation_gas.remaining,
        )
    }

    fn big_map_get_type(
        &mut self,
        id: &BigMapId,
    ) -> Result<Option<(Type, Type)>, LazyStorageError> {
        let arena = Arena::new();
        let key_type_path = key_type_path(self.context, id)?;
        let value_type_path = value_type_path(self.context, id)?;

        let encoded_key_type = match self.host.store_read_all(&key_type_path) {
            Ok(key_type) => Ok(key_type),
            Err(RuntimeError::PathNotFound) => return Ok(None),
            Err(err) => Err(err),
        }?;

        let key_type = Micheline::decode_raw(
            &arena,
            &encoded_key_type,
            &mut self.operation_gas.remaining,
        )??
        .parse_ty(self.gas())?;

        let encoded_value_type = match self.host.store_read_all(&value_type_path) {
            Ok(key_type) => Ok(key_type),
            Err(RuntimeError::PathNotFound) => return Ok(None),
            Err(err) => Err(err),
        }?;
        let value_type = Micheline::decode_raw(
            &arena,
            &encoded_value_type,
            &mut self.operation_gas.remaining,
        )??
        .parse_ty(self.gas())?;

        Ok(Some((key_type, value_type)))
    }
}

impl<'a, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry> TypecheckingCtx<'a>
    for Ctx<'_, '_, Host, C, R>
{
    fn gas(&mut self) -> &mut mir::gas::Gas {
        self.tc_ctx.gas()
    }

    fn lookup_entrypoints(
        &mut self,
        address: &AddressHash,
    ) -> Option<std::collections::HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
        self.tc_ctx.lookup_entrypoints(address)
    }

    fn big_map_get_type(
        &mut self,
        id: &BigMapId,
    ) -> Result<Option<(Type, Type)>, LazyStorageError> {
        self.tc_ctx.big_map_get_type(id)
    }
}

impl<'a, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry> CtxTrait<'a>
    for Ctx<'_, 'a, Host, C, R>
{
    fn sender(&self) -> AddressHash {
        self.exec_ctx.sender.clone()
    }

    fn source(&self) -> PublicKeyHash {
        self.operation_ctx.source.pkh().clone()
    }

    fn amount(&self) -> i64 {
        self.exec_ctx.amount
    }

    fn self_address(&self) -> AddressHash {
        self.exec_ctx.self_address.clone()
    }

    fn balance(&self) -> i64 {
        self.exec_ctx.balance
    }

    fn level(&self) -> BigUint {
        self.operation_ctx.level.block_number.into()
    }

    fn min_block_time(&self) -> BigUint {
        1u32.into()
    }

    fn chain_id(&self) -> mir::ast::ChainId {
        self.operation_ctx.chain_id.clone()
    }

    fn voting_power(&self, _: &PublicKeyHash) -> BigUint {
        0u32.into()
    }

    fn now(&self) -> BigInt {
        i64::from(*self.operation_ctx.now).into()
    }

    fn total_voting_power(&self) -> BigUint {
        1u32.into()
    }

    fn operation_group_hash(&self) -> &OperationHash {
        &self.operation_ctx.origination_nonce.operation
    }

    fn origination_counter(&mut self) -> u32 {
        let c: &mut u32 = &mut self.operation_ctx.origination_nonce.index;
        *c += 1;
        *c
    }

    fn operation_counter(&mut self) -> u128 {
        let c: &mut u128 = self.operation_ctx.counter;
        *c += 1;
        *c
    }

    fn lazy_storage(&mut self) -> Box<&mut dyn LazyStorage<'a>> {
        Box::new(self.tc_ctx)
    }

    fn lookup_view_storage_balance(
        &mut self,
        contract: &ContractKt1Hash,
        view_name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Result<
        Option<(
            mir::typechecker::MichelineView<Micheline<'a>>,
            Micheline<'a>,
            Vec<u8>,
            i64,
        )>,
        mir::context::LookupViewError,
    > {
        use mir::context::LookupViewError;
        // `originated_from_kt1` only builds the contract's durable path
        // from the index; it does not check existence. Failures here
        // mean a path/index corruption (or some host-layer issue) and
        // are surfaced as host errors.
        let account = self
            .tc_ctx
            .context
            .originated_from_kt1(contract)
            .map_err(|e| LookupViewError::HostError(e.to_string()))?;
        // L1 VIEW semantics push `None` when the target KT1 does not
        // exist at all (cf. `script_interpreter.ml::iview`). Probe
        // existence explicitly here — if we skipped this and went
        // straight to `account.code(host)`, a never-originated KT1
        // would error out on the missing `code` path and we would
        // propagate that as a hard `HostError`, breaking parity with
        // L1 for any contract doing
        //   `VIEW addr "name"; IF_NONE { ... }`
        // against a possibly-missing KT1.
        if !account
            .exists(self.tc_ctx.host)
            .map_err(|e| LookupViewError::HostError(e.to_string()))?
        {
            return Ok(None);
        }
        // The contract is originated past this point; any further
        // failure (read, decode, balance overflow) means corruption or
        // I/O on a known contract and must surface as an error rather
        // than impersonate "view not found".
        let serialized_script = account
            .code(self.tc_ctx.host)
            .map_err(|e| LookupViewError::HostError(e.to_string()))?;
        match serialized_script {
            Code::Code(serialized_script) => {
                let decoded = Micheline::decode_raw(
                    arena,
                    &serialized_script,
                    &mut self.tc_ctx.operation_gas.remaining,
                )??;
                let MichelineContractScript {
                    code: _,
                    parameter_ty: _,
                    storage_ty,
                    views,
                } = decoded.split_script()?;
                let Some(view) = views.get(view_name) else {
                    return Ok(None);
                };
                let owned_view = MichelineView {
                    input_type: view.input_type.clone(),
                    output_type: view.output_type.clone(),
                    code: view.code.clone(),
                };
                let storage = account
                    .storage(self.tc_ctx.host)
                    .map_err(|e| LookupViewError::HostError(e.to_string()))?;
                let balance = account
                    .balance(self.tc_ctx.host)
                    .map_err(|e| LookupViewError::HostError(e.to_string()))?;
                let balance: i64 = balance
                    .0
                    .try_into()
                    .map_err(|_| LookupViewError::BalanceOverflow)?;
                Ok(Some((owned_view, storage_ty.clone(), storage, balance)))
            }
            Code::Enshrined(_) => {
                // Current enshrined contracts have no views
                Ok(None)
            }
        }
    }

    fn set_view_context(
        &mut self,
        self_address: AddressHash,
        sender: AddressHash,
        amount: i64,
        balance: i64,
    ) {
        self.exec_ctx.self_address = self_address;
        self.exec_ctx.sender = sender;
        self.exec_ctx.amount = amount;
        self.exec_ctx.balance = balance;
    }

    /// Synthesize the gateway's `staticcall_evm` view: when
    /// `(kt1, name)` matches the TezosX gateway, that view name, and
    /// the declared `return_type` is `bytes`, build the cross-runtime
    /// GET to the EVM destination via
    /// [`dispatch_staticcall_evm_view`](Ctx::dispatch_staticcall_evm_view).
    /// Any mismatch (wrong KT1, return type, input shape) falls
    /// through to the standard view-code dispatch by returning
    /// `None`.
    ///
    /// The boundary cost (base + calldata + response payload, in
    /// milligas) is charged here, mirroring the state-mutating
    /// `%call_evm` entrypoint. The inner EVM execution cost
    /// (gas the EVM target spent on its read) is charged by
    /// `dispatch_staticcall_evm_get` via the
    /// `X-Tezos-Gas-Consumed` response header.
    fn try_dispatch_enshrined_view(
        &mut self,
        kt1: &ContractKt1Hash,
        name: &str,
        input: &TypedValue<'a>,
        return_type: &Type,
        // Reserved for future synthetic views that need to
        // materialize Micheline values; the EVM bridge's output is
        // a plain byte vector so the arena is unused here.
        _arena: &'a typed_arena::Arena<Micheline<'a>>,
    ) -> Option<Result<Option<TypedValue<'a>>, mir::interpreter::InterpretError<'a>>>
    {
        let contract = crate::enshrined_contracts::from_kt1(kt1)?;
        // Gate on [`enshrined_synthetic_views`] so the off-chain
        // `/script` synthesizer (which mirrors that enumeration) and
        // the on-chain dispatch below stay in lockstep. An
        // unenumerated name falls through to the standard view-code
        // dispatch by returning `None`; an enumerated name that has
        // no matching arm below trips `unreachable!()` —
        // [`enshrined_synthetic_views_dispatch_in_sync`] exercises
        // this for every enumerated view.
        if !enshrined_synthetic_views(contract)
            .iter()
            .any(|(n, _, _)| *n == name)
        {
            return None;
        }
        match (contract, name) {
            (
                crate::enshrined_contracts::EnshrinedContracts::TezosXGateway,
                "staticcall_evm",
            ) => self.dispatch_staticcall_evm_view(input, return_type),
            (
                crate::enshrined_contracts::EnshrinedContracts::TezosXGateway,
                "originOf",
            ) => self.dispatch_origin_of_view(input, return_type),
            (
                crate::enshrined_contracts::EnshrinedContracts::TezosXGateway,
                "resolveAddress",
            ) => self.dispatch_resolve_address_view(input, return_type),
            _ => unreachable!(
                "synthetic view {name:?} on {contract:?} is enumerated in \
                 enshrined_synthetic_views but has no dispatch arm in \
                 try_dispatch_enshrined_view"
            ),
        }
    }
}

/// Enumeration of the synthetic views exposed by an enshrined
/// contract, as `(view_name, parameter_type, return_type)` triples.
///
/// This is the **single source of truth**:
/// [`CtxTrait::try_dispatch_enshrined_view`] gates on this list, so a
/// name absent from it never reaches the dispatch match (the view is
/// invisible to both off-chain `/script` consumers and the on-chain
/// hook), while a name present here without a matching dispatch arm
/// trips `unreachable!()` — see
/// [`tests::enshrined_synthetic_views_dispatch_in_sync`], which
/// exercises the hook for every enumerated view.
///
/// Returning a `Vec` (rather than a `&'static`) keeps the API simple
/// for `Type` values, whose `Rc`-wrapped sub-types are not `const`.
pub fn enshrined_synthetic_views(
    contract: crate::enshrined_contracts::EnshrinedContracts,
) -> Vec<(&'static str, Type, Type)> {
    match contract {
        crate::enshrined_contracts::EnshrinedContracts::TezosXGateway => vec![
            // view "staticcall_evm" (pair string bytes) bytes
            //   Cross-runtime GET to an EVM destination; the real
            //   implementation lives in
            //   [`Ctx::dispatch_staticcall_evm_view`]. The view body
            //   in the synthesized script is `FAILWITH` — the
            //   stub is for discoverability only.
            (
                "staticcall_evm",
                Type::new_pair(Type::String, Type::Bytes),
                Type::Bytes,
            ),
            // view "originOf" (pair string nat) (or unit (or nat (pair nat string)))
            (
                "originOf",
                Type::new_pair(Type::String, Type::Nat),
                Type::new_or(
                    Type::Unit,
                    Type::new_or(Type::Nat, Type::new_pair(Type::Nat, Type::String)),
                ),
            ),
            // view "resolveAddress" (pair string (pair nat nat)) (option (pair nat string))
            (
                "resolveAddress",
                Type::new_pair(Type::String, Type::new_pair(Type::Nat, Type::Nat)),
                Type::new_option(Type::new_pair(Type::Nat, Type::String)),
            ),
        ],
        crate::enshrined_contracts::EnshrinedContracts::ERC20Wrapper => vec![],
    }
}

impl<'a, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    Ctx<'_, 'a, Host, C, R>
{
    /// Body of the `originOf` arm of
    /// [`try_dispatch_enshrined_view`](CtxTrait::try_dispatch_enshrined_view).
    ///
    /// Input shape: `pair string nat` — `(addr_str, source_runtime_nat)`.
    /// Return type: `or unit (or nat (pair nat string))`.
    ///
    /// Gas: only `Registry::read_origin`'s `consumed` is charged (alias
    /// lookup + optional EVM code-presence back-stop) via
    /// `classify_origin_for_view`. No flat per-call constant —
    /// see [`crate::enshrined_contracts::charge_gateway_base_cost`].
    ///
    /// On an invalid `source_runtime` nat, returns
    /// `Err(InterpretError::FailedWith(...))` with the Michelson payload
    /// `(Pair "INVALID_RUNTIME_ID" received_nat)` — standard Michelson
    /// FAILWITH semantics, causing the operation to revert with that value.
    fn dispatch_origin_of_view(
        &mut self,
        input: &TypedValue<'a>,
        return_type: &Type,
    ) -> Option<Result<Option<TypedValue<'a>>, mir::interpreter::InterpretError<'a>>>
    {
        // Return-type guard: `or unit (or nat (pair nat string))`.
        if !matches!(
            return_type,
            Type::Or(rc) if matches!(
                rc.as_ref(),
                (Type::Unit, Type::Or(inner))
                    if matches!(
                        inner.as_ref(),
                        (Type::Nat, Type::Pair(inner2))
                            if matches!(inner2.as_ref(), (Type::Nat, Type::String))
                    )
            )
        ) {
            return Some(Ok(None));
        }
        // Input shape: `pair string nat`.
        let (addr_str, source_runtime_nat) = match input {
            TypedValue::Pair(a, b) => match (a.as_ref(), b.as_ref()) {
                (TypedValue::String(a), TypedValue::Nat(b)) => (a.as_str(), b),
                _ => return Some(Ok(None)),
            },
            _ => return Some(Ok(None)),
        };
        // Distinct-field borrows.
        let host: &Host = self.tc_ctx.host;
        let operation_gas: &mut crate::gas::TezlinkOperationGas =
            self.tc_ctx.operation_gas;
        let result = crate::enshrined_contracts::dispatch_origin_of_get(
            host,
            operation_gas,
            self.registry,
            addr_str,
            source_runtime_nat,
        );
        Some(result.map(Some))
    }

    /// Body of the `resolveAddress` arm of
    /// [`try_dispatch_enshrined_view`](CtxTrait::try_dispatch_enshrined_view).
    ///
    /// Input shape: `pair string (pair nat nat)` —
    /// `(addr_str, (source_runtime_nat, target_runtime_nat))`.
    /// Return type: `option (pair nat string)`.
    ///
    /// Same gas model as `originOf` plus derivation overhead when the
    /// derivation path is taken.
    fn dispatch_resolve_address_view(
        &mut self,
        input: &TypedValue<'a>,
        return_type: &Type,
    ) -> Option<Result<Option<TypedValue<'a>>, mir::interpreter::InterpretError<'a>>>
    {
        // Return-type guard: `option (pair nat string)`.
        if !matches!(
            return_type,
            Type::Option(inner) if matches!(
                inner.as_ref(),
                Type::Pair(rc) if matches!(rc.as_ref(), (Type::Nat, Type::String))
            )
        ) {
            return Some(Ok(None));
        }
        // Input shape: `pair string (pair nat nat)`.
        let (addr_str, source_runtime_nat, target_runtime_nat) = match input {
            TypedValue::Pair(a, b) => match (a.as_ref(), b.as_ref()) {
                (TypedValue::String(a), TypedValue::Pair(s, t)) => {
                    match (s.as_ref(), t.as_ref()) {
                        (TypedValue::Nat(s), TypedValue::Nat(t)) => (a.as_str(), s, t),
                        _ => return Some(Ok(None)),
                    }
                }
                _ => return Some(Ok(None)),
            },
            _ => return Some(Ok(None)),
        };
        // Distinct-field borrows.
        let host: &Host = self.tc_ctx.host;
        let operation_gas: &mut crate::gas::TezlinkOperationGas =
            self.tc_ctx.operation_gas;
        let result = crate::enshrined_contracts::dispatch_resolve_address_get(
            host,
            operation_gas,
            self.registry,
            addr_str,
            source_runtime_nat,
            target_runtime_nat,
        );
        Some(result.map(Some))
    }

    /// Body of the `staticcall_evm` arm of
    /// [`try_dispatch_enshrined_view`](CtxTrait::try_dispatch_enshrined_view).
    /// Type-checks `return_type == bytes`, destructures the
    /// `(string, bytes)` input, charges the boundary cost, then
    /// dispatches through
    /// [`crate::enshrined_contracts::dispatch_staticcall_evm_get`]
    /// with `self.journal` and `self.registry`.
    fn dispatch_staticcall_evm_view(
        &mut self,
        input: &TypedValue<'a>,
        return_type: &Type,
    ) -> Option<Result<Option<TypedValue<'a>>, mir::interpreter::InterpretError<'a>>>
    {
        if !matches!(return_type, Type::Bytes) {
            return Some(Ok(None));
        }
        let (destination, calldata) = match input {
            TypedValue::Pair(d, c) => match (d.as_ref(), c.as_ref()) {
                (TypedValue::String(d), TypedValue::Bytes(c)) => {
                    (d.as_str(), c.as_slice())
                }
                _ => return Some(Ok(None)),
            },
            _ => return Some(Ok(None)),
        };
        // Destination validation is delegated to the EVM runtime's URL
        // parser (`parse_ethereum_url`); empty / malformed destinations
        // surface there as 4xx, which `dispatch_staticcall_evm_get`
        // then maps to `Ok(None)`. Keeping a single source of truth.
        let calling_kt1 = match &self.exec_ctx.self_address {
            AddressHash::Kt1(kt1) => kt1.clone(),
            _ => return Some(Ok(None)),
        };
        if crate::enshrined_contracts::charge_gateway_base_cost(self).is_err()
            || crate::enshrined_contracts::charge_gateway_payload(self, calldata.len())
                .is_err()
        {
            return Some(Err(mir::interpreter::InterpretError::OutOfGas));
        }
        let crac_id_str = self.journal.crac_id().to_string();
        let timestamp_str = i64::from(*self.operation_ctx.now).to_string();
        let block_number_str = u32::from(*self.operation_ctx.level).to_string();
        // Distinct-field split borrows on `self`: `tc_ctx.host`,
        // `tc_ctx.operation_gas`, `tc_ctx.context`, `journal`,
        // `registry`. The dispatcher consumes them for the call only.
        let host: &mut Host = self.tc_ctx.host;
        let operation_gas: &mut crate::gas::TezlinkOperationGas =
            self.tc_ctx.operation_gas;
        let context: &C = self.tc_ctx.context;
        let crac_chain_depth = self.operation_ctx.crac_chain_depth;
        let result = crate::enshrined_contracts::dispatch_staticcall_evm_get(
            host,
            operation_gas,
            self.registry,
            self.journal,
            context,
            &calling_kt1,
            &crac_id_str,
            &timestamp_str,
            &block_number_str,
            crac_chain_depth,
            destination,
            calldata,
        );
        if let Ok(Some(ref bytes)) = result {
            if crate::enshrined_contracts::charge_gateway_payload(self, bytes.len())
                .is_err()
            {
                return Some(Err(mir::interpreter::InterpretError::OutOfGas));
            }
        }
        match result {
            Ok(Some(bytes)) => Some(Ok(Some(TypedValue::Bytes(bytes)))),
            Ok(None) => Some(Ok(None)),
            Err(e) => Some(Err(e)),
        }
    }
}

pub trait HasHost<Host> {
    fn host(&mut self) -> &mut Host;
}

pub trait HasContractAccount {
    type Account: TezosOriginatedAccount;
    fn contract_account(&self) -> &Self::Account;
}

pub trait HasOperationGas {
    fn operation_gas(&mut self) -> &mut crate::gas::TezlinkOperationGas;
}

pub trait HasSourcePublicKey {
    fn source_public_key(&self) -> &[u8];
}

pub trait HasCracChainDepth {
    fn crac_chain_depth(&self) -> u32;

    /// Originator of the inbound CRAC being serviced, if any.
    /// See [`OperationCtx::crac_origin`].
    fn crac_origin(&self) -> Option<Contract>;
}

/// Read the runtime classification record for an address.
/// Handles host and context access so callers can resolve
/// cross-runtime aliases without separate borrows.
pub trait HasOriginLookup {
    fn read_origin_for_address(
        &self,
        address: &AddressHash,
    ) -> Result<Option<tezosx_interfaces::Origin>, tezos_storage::error::Error>;
}

/// Access the per-operation CRAC journal.
pub trait HasJournal {
    fn journal(&mut self) -> &mut tezosx_journal::TezosXJournal;
}

/// Access the per-operation cross-runtime registry.
pub trait HasRegistry {
    type R: tezosx_interfaces::Registry;
    fn registry(&self) -> &Self::R;
}

/// Atomically expose `host`, `journal`, and `registry` as a tuple
/// of disjoint borrows on the same `ctx`. Required for the
/// `registry.serve(host, journal, request)` and
/// `registry.ensure_alias(host, journal, ...)` calls where the three
/// fields must be live simultaneously — sequential `host()`,
/// `journal()`, `registry()` trait method calls would each hold
/// `&mut self` exclusively and conflict. Implementors construct the
/// tuple via direct field access so the borrow checker accepts the
/// distinct-field split.
pub trait HasCrossRuntime<Host: StorageV1>: HasJournal + HasRegistry {
    fn cross_runtime_split(
        &mut self,
    ) -> (
        &mut Host,
        &mut tezosx_journal::TezosXJournal,
        &<Self as HasRegistry>::R,
    );
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasContractAccount for Ctx<'a, 'operation, Host, C, R>
{
    type Account = TezlinkOriginatedAccount;
    fn contract_account(&self) -> &Self::Account {
        &self.exec_ctx.contract_account
    }
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasHost<Host> for Ctx<'a, 'operation, Host, C, R>
{
    fn host(&mut self) -> &mut Host {
        self.tc_ctx.host
    }
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasOriginLookup for Ctx<'a, 'operation, Host, C, R>
{
    fn read_origin_for_address(
        &self,
        address: &AddressHash,
    ) -> Result<Option<tezosx_interfaces::Origin>, tezos_storage::error::Error> {
        self.tc_ctx
            .context
            .read_origin_for_address(&*self.tc_ctx.host, address)
    }
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasJournal for Ctx<'a, 'operation, Host, C, R>
{
    fn journal(&mut self) -> &mut tezosx_journal::TezosXJournal {
        self.journal
    }
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasRegistry for Ctx<'a, 'operation, Host, C, R>
{
    type R = R;
    fn registry(&self) -> &Self::R {
        self.registry
    }
}

impl<'a, 'operation, Host: StorageV1, C: Context, R: tezosx_interfaces::Registry>
    HasCrossRuntime<Host> for Ctx<'a, 'operation, Host, C, R>
{
    fn cross_runtime_split(
        &mut self,
    ) -> (&mut Host, &mut tezosx_journal::TezosXJournal, &R) {
        (self.tc_ctx.host, self.journal, self.registry)
    }
}

impl<'operation, Host: StorageV1, C: Context> HasOriginLookup
    for TcCtx<'operation, Host, C>
{
    fn read_origin_for_address(
        &self,
        address: &AddressHash,
    ) -> Result<Option<tezosx_interfaces::Origin>, tezos_storage::error::Error> {
        self.context.read_origin_for_address(&*self.host, address)
    }
}

impl<Host: StorageV1, C: Context, R: tezosx_interfaces::Registry> HasOperationGas
    for Ctx<'_, '_, Host, C, R>
{
    fn operation_gas(&mut self) -> &mut crate::gas::TezlinkOperationGas {
        self.tc_ctx.operation_gas
    }
}

impl<Host: StorageV1, C: Context, R: tezosx_interfaces::Registry> HasSourcePublicKey
    for Ctx<'_, '_, Host, C, R>
{
    fn source_public_key(&self) -> &[u8] {
        self.operation_ctx.source_public_key
    }
}

impl<Host: StorageV1, C: Context, R: tezosx_interfaces::Registry> HasCracChainDepth
    for Ctx<'_, '_, Host, C, R>
{
    fn crac_chain_depth(&self) -> u32 {
        self.operation_ctx.crac_chain_depth
    }

    fn crac_origin(&self) -> Option<Contract> {
        self.operation_ctx.crac_origin.clone()
    }
}

impl<Host: StorageV1, C: Context> TcCtx<'_, Host, C> {
    /// Insert in the context a big_map diff that represents an allocation
    fn big_map_diff_alloc(&mut self, id: Zarith, key_type: Vec<u8>, value_type: Vec<u8>) {
        let allocation = StorageDiff::Alloc(Alloc {
            updates: vec![],
            key_type,
            value_type,
        });
        self.big_map_diff.insert(id, allocation);
    }

    /// Insert in the context a big_map diff that represents an update
    fn big_map_diff_update(
        &mut self,
        id: &Zarith,
        key_hash: ScriptExprHash,
        key: Vec<u8>,
        value: Option<Vec<u8>>,
    ) {
        let update = Update {
            key_hash,
            key,
            value,
        };
        match self.big_map_diff.get_mut(id) {
            None => {
                self.big_map_diff
                    .insert(id.clone(), StorageDiff::Update(vec![update]));
            }
            Some(diff) => diff.push_update(update),
        }
    }

    /// Insert in the context a big_map diff that represents a remove
    fn big_map_diff_remove(&mut self, id: Zarith) {
        self.big_map_diff.insert(id, StorageDiff::Remove);
    }

    /// Insert in the context a big_map diff that represents a copy
    fn big_map_diff_copy(&mut self, id: Zarith, source: Zarith) {
        self.big_map_diff.insert(
            id,
            StorageDiff::Copy(Copy {
                source,
                updates: vec![],
            }),
        );
    }

    fn generate_id(&mut self, temporary: bool) -> Result<BigMapId, LazyStorageError> {
        if temporary {
            let new_id = self.next_temporary_id.clone();
            self.next_temporary_id.incr();
            Ok(new_id)
        } else {
            let next_id_path = next_id_path(self.context)?;
            let id: BigMapId =
                read_nom_value(self.host, &next_id_path).unwrap_or(0.into());
            store_bin(&id.succ(), self.host, &next_id_path)
                .map_err(storage_error_to_lazy)?;
            Ok(id)
        }
    }
}

fn remove_big_map<Host: StorageV1, C: Context>(
    host: &mut Host,
    context: &C,
    id: &BigMapId,
) -> Result<(), LazyStorageError> {
    // Remove the key type of the big_map
    let key_type_path = key_type_path(context, id)?;
    host.store_delete(&key_type_path)?;

    // Remove the value type of the big_map
    let value_type_path = value_type_path(context, id)?;
    host.store_delete(&value_type_path)?;

    // Removing the content of the big_map
    BigMapKeys::remove_keys_in_storage(host, context, id)?;

    let total_bytes_path = total_bytes_path(context, id)?;
    host.store_delete_value(&total_bytes_path)?;

    Ok(())
}

/// Function to clear temporary big_maps create for an operation
///
/// This function also reset the next temporary id to minus one
pub fn clear_temporary_big_maps<Host: StorageV1, C: Context>(
    host: &mut Host,
    context: &C,
    next_temp_id: &mut BigMapId,
) -> Result<(), LazyStorageError> {
    while next_temp_id.dec() {
        remove_big_map(host, context, next_temp_id)?;
    }
    Ok(())
}

/// Hashes a Micheline expression using the packed format (with 0x05 prefix)
/// to match L1's Script_expr_hash.
/// See: https://gitlab.com/tezos/tezos/-/blob/master/src/proto_023_PtSeouLo/lib_protocol/script_ir_translator.ml#L159
fn hash_micheline_expr(expr: &Micheline<'_>) -> Result<ScriptExprHash, LazyStorageError> {
    let bytes = expr.encode_for_pack()??;
    Ok(digest_256(&bytes).into())
}

/// Adapter for the legacy `tezos_storage::Error → LazyStorageError`
/// stringification path. `tezos_storage::Error` is not a `BinError`, so
/// the new `From<BinError>` impl on `LazyStorageError` does not apply
/// here; keep the explicit conversion isolated in one helper instead of
/// inlining `.map_err(|e| LazyStorageError::BinWriteError(...))` at
/// each call site.
fn storage_error_to_lazy(e: tezos_storage::error::Error) -> LazyStorageError {
    LazyStorageError::BinWriteError(std::rc::Rc::new(
        tezos_data_encoding::enc::BinError::custom(e.to_string()),
    ))
}

/// Computes the hash of a big_map key (TypedValue), used for storage path
/// See [hash_micheline_expr] for details on the hashing format.
fn hash_key(
    key: TypedValue<'_>,
    gas: &mut Gas,
) -> Result<ScriptExprHash, LazyStorageError> {
    let parser = Parser::new();
    let key_micheline = key.into_micheline_optimized_legacy(&parser.arena, gas)?;
    hash_micheline_expr(&key_micheline)
}

/// Function to convert a BtreeMap that represent the lazy_storage_diff
/// in a valid Tezos representation.
pub fn convert_big_map_diff(
    big_map_diff: BTreeMap<Zarith, StorageDiff>,
) -> Option<LazyStorageDiffList> {
    let mut list_diff = vec![];
    // L1 receipts big_map diffs are in reverse order, this is mandatory for external tools that
    // except such an order.
    for (id, storage_diff) in big_map_diff.into_iter().rev() {
        let diff = LazyStorageDiff::BigMap(BigMapDiff { id, storage_diff });
        list_diff.push(diff);
    }
    if list_diff.is_empty() {
        None
    } else {
        Some(LazyStorageDiffList { diff: list_diff })
    }
}

#[derive(Debug, BinWriter, NomReader)]
struct BigMapKeys {
    #[encoding(list)]
    keys: Vec<ScriptExprHash>,
}

impl BigMapKeys {
    #[cfg(test)]
    fn get<C: Context>(host: &mut impl StorageV1, context: &C, id: &BigMapId) -> Self {
        let path = keys_of_big_map(context, id).unwrap();
        read_nom_value(host, &path).unwrap_or(BigMapKeys { keys: vec![] })
    }

    fn add_key<C: Context>(
        host: &mut impl StorageV1,
        context: &C,
        id: &BigMapId,
        key: &ScriptExprHash,
    ) -> Result<(), LazyStorageError> {
        let path = keys_of_big_map(context, id)?;
        let size = host.store_value_size(&path).unwrap_or(0usize);
        host.store_write(&path, key.as_ref(), size)?;
        Ok(())
    }

    fn remove_key<C: Context>(
        host: &mut impl StorageV1,
        context: &C,
        id: &BigMapId,
        key: &ScriptExprHash,
    ) -> Result<(), LazyStorageError> {
        let path = keys_of_big_map(context, id)?;
        let mut big_map_keys: Self = read_nom_value(host, &path)
            .map_err(|e| LazyStorageError::NomReadError(e.to_string()))?;
        big_map_keys.keys.retain(|elt| elt != key);
        store_bin(&big_map_keys, host, &path).map_err(storage_error_to_lazy)?;
        Ok(())
    }

    fn remove_keys_in_storage<C: Context>(
        host: &mut impl StorageV1,
        context: &C,
        id: &BigMapId,
    ) -> Result<(), LazyStorageError> {
        let path = keys_of_big_map(context, id)?;
        let big_map_keys_opt: Option<Self> = read_optional_nom_value(host, &path)
            .map_err(|e| LazyStorageError::NomReadError(e.to_string()))?;
        let big_map_keys = match big_map_keys_opt {
            Some(big_map_keys) => big_map_keys,
            None => {
                // If the big_map keys doesn't exist, no need to remove
                // anything
                return Ok(());
            }
        };
        for key in big_map_keys.keys {
            let value_path = value_path(context, id, &key)?;
            host.store_delete(&value_path)?;
        }

        // Remove keys for the big_map
        host.store_delete(&path)?;

        Ok(())
    }

    fn copy_keys_in_storage<C: Context>(
        host: &mut impl StorageV1,
        context: &C,
        source: &BigMapId,
        dest: &BigMapId,
    ) -> Result<(), LazyStorageError> {
        let source_path = keys_of_big_map(context, source)?;
        let big_map_keys_opt: Option<Self> = read_optional_nom_value(host, &source_path)
            .map_err(|e| LazyStorageError::NomReadError(e.to_string()))?;
        let big_map_keys = match big_map_keys_opt {
            Some(big_map_keys) => big_map_keys,
            None => {
                // If the big_map keys doesn't exist, no need to try
                // the copy we can just return instantly
                return Ok(());
            }
        };

        for key in &big_map_keys.keys {
            let source_value_path = value_path(context, source, key)?;
            let dest_value_path = value_path(context, dest, key)?;

            // Copy the value at from source path to dest path
            let value = host.store_read_all(&source_value_path)?;
            host.store_write_all(&dest_value_path, &value)?;
        }

        let dest_path = keys_of_big_map(context, dest)?;
        store_bin(&big_map_keys, host, &dest_path).map_err(storage_error_to_lazy)?;

        Ok(())
    }
}

/// Flat per-entry forfait L1 charges to cover the on-disk cost of
/// indexing a big-map entry.
///
/// Value from `src/proto_023_PtSeouLo/lib_protocol/lazy_storage_diff.ml`.
const BYTES_SIZE_FOR_BIG_MAP_KEY: u64 = 65;

/// Flat per-big-map forfait L1 charges to cover the on-disk cost of
/// the big-map slot itself (independent of entries).
///
/// Value from `src/proto_023_PtSeouLo/lib_protocol/lazy_storage_diff.ml`.
const BYTES_SIZE_FOR_EMPTY: u64 = 33;

/// Reconstruct the `total_bytes` counter for a big-map allocated
/// before the counter existed. Walks the persisted keys list and
/// sums `BYTES_SIZE_FOR_BIG_MAP_KEY` plus each value's stored size,
/// then persists the result so subsequent reads are O(1).
fn init_total_bytes_from_existing<C: Context>(
    host: &mut impl StorageV1,
    context: &C,
    id: &BigMapId,
) -> Result<Zarith, LazyStorageError> {
    let keys_path = keys_of_big_map(context, id)?;
    let keys: BigMapKeys = read_optional_nom_value(host, &keys_path)
        .map_err(|e| LazyStorageError::NomReadError(e.to_string()))?
        .unwrap_or(BigMapKeys { keys: vec![] });
    let mut total: u64 = 0;
    for key_hash in &keys.keys {
        let vp = value_path(context, id, key_hash)?;
        let size = host.store_value_size(&vp)? as u64;
        total = total.saturating_add(BYTES_SIZE_FOR_BIG_MAP_KEY + size);
    }
    let migrated = Zarith(total.into());
    set_total_bytes(host, context, id, &migrated)?;
    Ok(migrated)
}

/// Read the `total_bytes` counter persisted for a big-map. Lazily
/// migrates pre-counter big-maps via `init_total_bytes_from_existing`.
fn total_bytes<C: Context>(
    host: &mut impl StorageV1,
    context: &C,
    id: &BigMapId,
) -> Result<Zarith, LazyStorageError> {
    let path = total_bytes_path(context, id)?;
    match read_optional_nom_value::<Zarith>(host, &path) {
        Ok(Some(total)) => Ok(total),
        Ok(None) => init_total_bytes_from_existing(host, context, id),
        Err(e) => Err(LazyStorageError::NomReadError(e.to_string())),
    }
}

/// Write the `total_bytes` counter persisted for a big-map.
fn set_total_bytes<C: Context>(
    host: &mut impl StorageV1,
    context: &C,
    id: &BigMapId,
    value: &Zarith,
) -> Result<(), LazyStorageError> {
    let path = total_bytes_path(context, id)?;
    store_bin(value, host, &path).map_err(storage_error_to_lazy)?;
    Ok(())
}

impl<'a, Host: StorageV1, C: Context> LazyStorage<'a> for TcCtx<'a, Host, C> {
    fn big_map_get(
        &mut self,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        let value_path =
            value_path(self.context, id, &hash_key(key.clone(), self.gas())?)?;
        if self.host.store_has(&value_path)?.is_none() {
            return Ok(None);
        }

        let value_type_path = value_type_path(self.context, id)?;
        let encoded_value_type = self.host.store_read_all(&value_type_path)?;
        let value_type = Micheline::decode_raw(
            arena,
            &encoded_value_type,
            &mut self.operation_gas.remaining,
        )??;

        let encoded_value = self.host.store_read_all(&value_path)?;
        let value = Micheline::decode_raw(
            arena,
            &encoded_value,
            &mut self.operation_gas.remaining,
        )??;
        Ok(Some(value.typecheck_value(self, &value_type)?))
    }

    fn big_map_mem(
        &mut self,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<bool, LazyStorageError> {
        let path = value_path(self.context, id, &hash_key(key.clone(), self.gas())?)?;
        Ok(self.host.store_has(&path)?.is_some())
    }

    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError> {
        let parser = Parser::new();
        let micheline_expr =
            key.into_micheline_optimized_legacy(&parser.arena, self.gas())?;
        // key_encoded: raw Micheline encoding (no 0x05 prefix), used in big_map_diff receipts
        let key_encoded = micheline_expr.encode(&mut self.operation_gas.remaining)??;
        // key_hashed: hash of packed encoding (with 0x05 prefix), used for storage path
        // See: https://gitlab.com/tezos/tezos/-/blob/master/src/proto_023_PtSeouLo/lib_protocol/script_ir_translator.ml#L5563
        let key_hashed = hash_micheline_expr(&micheline_expr)?;
        let value_path = value_path(self.context, id, &key_hashed)?;
        match value {
            None => {
                if self.host.store_has(&value_path)?.is_some() {
                    let previous_value_size: BigInt =
                        self.host.store_value_size(&value_path)?.into();
                    self.host.store_delete(&value_path)?;
                    BigMapKeys::remove_key(self.host, self.context, id, &key_hashed)?;

                    let current = total_bytes(self.host, self.context, id)?;
                    let lazy_storage_size_diff = Zarith(
                        -(BigInt::from(BYTES_SIZE_FOR_BIG_MAP_KEY) + previous_value_size),
                    );
                    let new_total_bytes = Zarith(current.0 + &lazy_storage_size_diff.0);
                    set_total_bytes(self.host, self.context, id, &new_total_bytes)?;
                    self.interpret_context
                        .record_lazy_storage_size_diff(id, &lazy_storage_size_diff);
                }

                // Write the update in the big_map_diff
                self.big_map_diff_update(&id.value, key_hashed, key_encoded, None);
                Ok(())
            }
            Some(v) => {
                let arena = Arena::new();
                let encoded = v
                    .into_micheline_optimized_legacy(&arena, self.gas())?
                    .encode(&mut self.operation_gas.remaining)??;
                let new_value_size: BigInt = encoded.len().into();
                let current = total_bytes(self.host, self.context, id)?;
                let lazy_storage_size_diff = match self.host.store_value_size(&value_path)
                {
                    Err(RuntimeError::PathNotFound) => {
                        // We should write the key in the list only if it's an add in the big_map not an update
                        BigMapKeys::add_key(self.host, self.context, id, &key_hashed)?;
                        Zarith(BYTES_SIZE_FOR_BIG_MAP_KEY + new_value_size)
                    }
                    Ok(previous_value_size) => {
                        let previous_value_size: BigInt = previous_value_size.into();
                        Zarith(new_value_size - previous_value_size)
                    }
                    Err(err) => return Err(err.into()),
                };
                let new_total_bytes = Zarith(current.0 + &lazy_storage_size_diff.0);
                set_total_bytes(self.host, self.context, id, &new_total_bytes)?;
                self.interpret_context
                    .record_lazy_storage_size_diff(id, &lazy_storage_size_diff);

                self.host.store_write_all(&value_path, &encoded)?;

                // Write the update in the big_map_diff
                self.big_map_diff_update(
                    &id.value,
                    key_hashed,
                    key_encoded,
                    Some(encoded),
                );
                Ok(())
            }
        }
    }

    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError> {
        let arena = Arena::new();
        let id = self.generate_id(temporary)?;
        let key_type_path = key_type_path(self.context, &id)?;
        let value_type_path = value_type_path(self.context, &id)?;
        let key_type_encoded = key_type
            .into_micheline_optimized_legacy(&arena, self.gas())?
            .encode(&mut self.operation_gas.remaining)??;
        let value_type_encoded = value_type
            .into_micheline_optimized_legacy(&arena, self.gas())?
            .encode(&mut self.operation_gas.remaining)??;
        self.host
            .store_write_all(&value_type_path, &value_type_encoded)?;
        self.host
            .store_write_all(&key_type_path, &key_type_encoded)?;

        self.interpret_context.record_lazy_storage_size_diff(
            &id,
            &Zarith(BigInt::from(BYTES_SIZE_FOR_EMPTY)),
        );

        // Write in the diff that there was an allocation
        self.big_map_diff_alloc(id.value.clone(), key_type_encoded, value_type_encoded);
        Ok(id)
    }

    fn big_map_copy(
        &mut self,
        id: &BigMapId,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError> {
        let dest_id = self.generate_id(temporary)?;

        // Retrieve the path of the key_type
        let src_key_type_path = key_type_path(self.context, id)?;
        let dest_key_type_path = key_type_path(self.context, &dest_id)?;

        // Copy the key type to the destination
        let key_type = self.host.store_read_all(&src_key_type_path)?;
        self.host.store_write_all(&dest_key_type_path, &key_type)?;

        // Retrieve the path of the value_type
        let src_value_type_path = value_type_path(self.context, id)?;
        let dest_value_type_path = value_type_path(self.context, &dest_id)?;

        // Copy the value type to the destination
        let value_type = self.host.store_read_all(&src_value_type_path)?;
        self.host
            .store_write_all(&dest_value_type_path, &value_type)?;

        // Copy the content of the big_map
        BigMapKeys::copy_keys_in_storage(self.host, self.context, id, &dest_id)?;

        let source_total_bytes = total_bytes(self.host, self.context, id)?;
        set_total_bytes(self.host, self.context, &dest_id, &source_total_bytes)?;
        self.interpret_context.record_lazy_storage_size_diff(
            &dest_id,
            &Zarith(BigInt::from(BYTES_SIZE_FOR_EMPTY) + source_total_bytes.0),
        );

        // Write in the diff that there was a copy
        self.big_map_diff_copy(dest_id.value.clone(), id.value.clone());
        Ok(dest_id)
    }

    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError> {
        let total = total_bytes(self.host, self.context, id)?;
        self.interpret_context.record_lazy_storage_size_diff(
            id,
            &Zarith(-(BigInt::from(BYTES_SIZE_FOR_EMPTY) + total.0)),
        );
        remove_big_map(self.host, self.context, id)?;

        // Write in the diff that there was a remove
        self.big_map_diff_remove(id.value.clone());

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        context::{Context, TezlinkContext},
        gas::TezlinkOperationGas,
    };
    use mir::ast::big_map::{
        dump_big_map_updates, BigMap, BigMapContent, BigMapFromId, BigMapId,
    };
    use std::collections::BTreeMap;
    use tezos_evm_runtime::runtime::MockKernelHost;

    #[macro_export]
    macro_rules! make_default_ctx {
        ($ctx:ident, $host: expr, $context: expr) => {
            let mut operation_gas = TezlinkOperationGas::default();
            let mut $ctx = TcCtx {
                host: $host,
                context: $context,
                operation_gas: &mut operation_gas,
                big_map_diff: BTreeMap::new(),
                interpret_context: $crate::mir_ctx::InterpretContext::new(),
                next_temporary_id: &mut BigMapId { value: (-1).into() },
            };
        };
    }

    #[track_caller]
    fn check_is_dumped_map(map: BigMap, id: BigMapId) {
        match map.content {
            BigMapContent::InMemory(_) => panic!("Big map has not been dumped"),
            BigMapContent::FromId(map) => {
                assert_eq!((map.id, map.overlay), (id, BTreeMap::new()))
            }
        };
    }

    pub fn assert_big_map_eq<'a, Host: StorageV1, C: Context>(
        ctx: &mut TcCtx<'a, Host, C>,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key_type: Type,
        value_type: Type,
        content: BTreeMap<TypedValue<'a>, TypedValue<'a>>,
    ) {
        let (stored_key_type, stored_value_type) = ctx
            .big_map_get_type(id)
            .expect("Failed to read key and value types from storage")
            .expect("Big map should be present in storage");

        assert_eq!(stored_key_type, key_type);
        assert_eq!(stored_value_type, value_type);

        let nb_passed_keys = content.len();
        let nb_stored_keys = BigMapKeys::get(ctx.host, ctx.context, id).keys.len();
        // The big_map storage contains the key_type and value_type subkeys followed by the other keys corresponding to values
        assert_eq!(nb_passed_keys, nb_stored_keys);

        for (key, value) in &content {
            let stored_value = ctx
                .big_map_get(arena, id, key)
                .expect("Failed to read value from storage")
                .expect("Key should be present in storage");
            assert_eq!(&stored_value, value);
        }
    }

    fn assert_big_map_removed<'a, Host: StorageV1, C: Context>(
        ctx: &TcCtx<'a, Host, C>,
        id: &BigMapId,
        removed_keys: &BigMapKeys,
    ) {
        let key_type_path = key_type_path(ctx.context, id).unwrap();
        assert!(
            ctx.host.store_has(&key_type_path).unwrap().is_none(),
            "Key type should have been removed",
        );

        let value_type_path = value_type_path(ctx.context, id).unwrap();
        assert!(
            ctx.host.store_has(&value_type_path).unwrap().is_none(),
            "Value type should have been removed",
        );

        let keys_path = keys_of_big_map(ctx.context, id).unwrap();
        assert!(
            ctx.host.store_has(&keys_path).unwrap().is_none(),
            "List of keys of the big_map should have been removed",
        );

        for key in &removed_keys.keys {
            let value_path = value_path(ctx.context, id, key).unwrap();
            assert!(
                ctx.host.store_has(&value_path).unwrap().is_none(),
                "{key:?} should have been removed from the storage"
            );
        }
    }

    #[test]
    fn test_map_from_memory() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &TezlinkContext::init_context());
        let content = BTreeMap::from([
            (TypedValue::int(1), TypedValue::String("one".into())),
            (TypedValue::int(2), TypedValue::String("two".into())),
        ]);

        let mut map = BigMap {
            content: BigMapContent::InMemory(content.clone()),
            key_type: Type::Int,
            value_type: Type::String,
        };
        dump_big_map_updates(&mut storage, &[], &mut [&mut map], false).unwrap();

        check_is_dumped_map(map, 0.into());

        assert_big_map_eq(
            &mut storage,
            &Arena::new(),
            &0.into(),
            Type::Int,
            Type::String,
            content,
        );
    }

    #[test]
    fn test_map_updates_to_storage() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &TezlinkContext::init_context());
        let map_id = storage
            .big_map_new(&Type::Int, &Type::String, false)
            .unwrap();
        storage
            .big_map_update(
                &map_id,
                TypedValue::int(1),
                Some(TypedValue::String("a".into())),
            )
            .unwrap();
        storage
            .big_map_update(
                &map_id,
                TypedValue::int(2),
                Some(TypedValue::String("b".into())),
            )
            .unwrap();
        storage
            .big_map_update(
                &map_id,
                TypedValue::int(3),
                Some(TypedValue::String("c".into())),
            )
            .unwrap();

        let big_map_keys_before = BigMapKeys::get(storage.host, storage.context, &map_id);
        assert_eq!(
            big_map_keys_before.keys.len(),
            3usize,
            "{big_map_keys_before:?}"
        );

        storage
            .big_map_update(&map_id, TypedValue::int(2), None)
            .unwrap();
        storage
            .big_map_update(
                &map_id,
                TypedValue::int(3),
                Some(TypedValue::String("gamma".into())),
            )
            .unwrap();

        let big_map_keys_after = BigMapKeys::get(storage.host, storage.context, &map_id);
        assert_eq!(
            big_map_keys_after.keys.len(),
            2usize,
            "{big_map_keys_after:?}"
        );
        assert_eq!(
            big_map_keys_before.keys.first(),
            big_map_keys_after.keys.first(),
        );
        assert_eq!(
            big_map_keys_before.keys.get(2),
            big_map_keys_after.keys.get(1),
        );

        let expected_content = BTreeMap::from([
            (TypedValue::int(1), TypedValue::String("a".into())),
            (TypedValue::int(3), TypedValue::String("gamma".into())),
        ]);

        assert_big_map_eq(
            &mut storage,
            &Arena::new(),
            &map_id,
            Type::Int,
            Type::String,
            expected_content,
        );
    }

    #[test]
    fn test_copy() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &TezlinkContext::init_context());
        let content = BTreeMap::from([
            (TypedValue::int(1), TypedValue::String("one".into())),
            (TypedValue::int(2), TypedValue::String("two".into())),
        ]);

        let mut map = BigMap {
            content: BigMapContent::InMemory(content.clone()),
            key_type: Type::Int,
            value_type: Type::String,
        };
        dump_big_map_updates(&mut storage, &[], &mut [&mut map], false).unwrap();

        check_is_dumped_map(map, 0.into());

        let copied_id = storage
            .big_map_copy(&0.into(), false)
            .expect("Failed to copy big_map in storage");

        assert_eq!(copied_id, 1.into());

        assert_big_map_eq(
            &mut storage,
            &Arena::new(),
            &copied_id,
            Type::Int,
            Type::String,
            content,
        );
    }

    #[test]
    fn test_remove_big_map() {
        // Setup the context and big_map for the test
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &TezlinkContext::init_context());
        let key_type = Type::Int;
        let value_type = Type::Int;
        let map_id = storage.big_map_new(&key_type, &value_type, false).unwrap();
        let key = TypedValue::int(0);
        let value = TypedValue::int(0);
        storage
            .big_map_update(&map_id, key.clone(), Some(value.clone()))
            .unwrap();

        // Ensure that the big_map is existing
        let mut content = BTreeMap::new();
        content.insert(key.clone(), value);
        let arena = Arena::new();
        assert_big_map_eq(&mut storage, &arena, &map_id, key_type, value_type, content);

        // Remove the big_map
        storage.big_map_remove(&map_id).unwrap();

        // Ensure that the big_map has been removed
        let removed_keys = BigMapKeys {
            keys: vec![hash_key(key, storage.gas()).unwrap()],
        };
        assert_big_map_removed(&storage, &map_id, &removed_keys);

        // Verify that the big_map_mem function returns the expected result
        assert!(!storage.big_map_mem(&map_id, &TypedValue::int(0)).unwrap());
    }

    #[test]
    fn test_remove_with_dump() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &TezlinkContext::init_context());
        // Arena must outlive `map1` so its `BigMap<'_>` destructor can still
        // borrow from the arena lifetime; values are dropped in reverse
        // declaration order.
        let arena = Arena::new();
        let map_id1 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id1, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let map_id2 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id2, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let content_diff = BigMapContent::FromId(BigMapFromId {
            id: map_id1.clone(),
            overlay: BTreeMap::from([(TypedValue::int(1), Some(TypedValue::int(1)))]),
        });
        let mut map1 = BigMap {
            content: content_diff,
            key_type: Type::Int,
            value_type: Type::Int,
        };

        dump_big_map_updates(
            &mut storage,
            &[map_id1.clone(), map_id2.clone()],
            &mut [&mut map1],
            false,
        )
        .unwrap();

        let expected_content = BTreeMap::from([
            (TypedValue::int(0), TypedValue::int(0)),
            (TypedValue::int(1), TypedValue::int(1)),
        ]);

        assert!(!storage.big_map_mem(&map_id2, &TypedValue::int(0)).unwrap());

        assert_big_map_eq(
            &mut storage,
            &arena,
            &map_id1,
            Type::Int,
            Type::Int,
            expected_content,
        );
    }

    // L1 receipts big_map diffs are in reverse order, this is mandatory for external tools that
    // except such an order.
    #[test]
    fn test_convert_big_map_diff_order() {
        let mut gas = Gas::default();
        let key_type = mir::ast::Micheline::prim0(mir::lexer::Prim::nat, &mut gas)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let value_type = mir::ast::Micheline::prim0(mir::lexer::Prim::unit, &mut gas)
            .unwrap()
            .encode(&mut Gas::default())
            .unwrap()
            .unwrap();
        let alloc_0 = StorageDiff::Alloc(Alloc {
            updates: vec![],
            key_type: key_type.clone(),
            value_type: value_type.clone(),
        });
        let alloc_5 = StorageDiff::Alloc(Alloc {
            updates: vec![],
            key_type: key_type.clone(),
            value_type: value_type.clone(),
        });
        let alloc_4 = StorageDiff::Alloc(Alloc {
            updates: vec![],
            key_type: key_type.clone(),
            value_type: value_type.clone(),
        });
        let mut map: BTreeMap<Zarith, StorageDiff> = BTreeMap::new();
        map.insert(0u64.into(), alloc_0.clone());
        map.insert(5u64.into(), alloc_5.clone());
        map.insert(4u64.into(), alloc_4.clone());
        let diff_list = convert_big_map_diff(map);
        let expected = Some(LazyStorageDiffList {
            diff: vec![
                LazyStorageDiff::BigMap(BigMapDiff {
                    id: 5u64.into(),
                    storage_diff: alloc_5,
                }),
                LazyStorageDiff::BigMap(BigMapDiff {
                    id: 4u64.into(),
                    storage_diff: alloc_4,
                }),
                LazyStorageDiff::BigMap(BigMapDiff {
                    id: 0u64.into(),
                    storage_diff: alloc_0,
                }),
            ],
        });
        assert_eq!(diff_list, expected, "Receipt should be in reverse order");
    }

    /// Regression test for the existence-probe in
    /// [`Ctx::lookup_view_storage_balance`]. At L1, calling
    /// `VIEW addr "name"` on a never-originated KT1 must push
    /// `V::Option(None)` (cf. `script_interpreter.ml::iview`); the
    /// trait contract is for `Ok(None)` to surface that case so the
    /// interpreter then maps it to `V::Option(None)`. This test fires
    /// the production impl directly against a fresh host and a KT1
    /// that has never been written, asserting it does not surface the
    /// missing-`code` durable read as a `LookupViewError::HostError`.
    #[test]
    fn lookup_view_storage_balance_returns_none_for_unoriginated_kt1() {
        use crate::account_storage::{TezlinkImplicitAccount, TezlinkOriginatedAccount};
        use crate::address::OriginationNonce;
        use mir::ast::michelson_address::AddressHash;
        use tezos_smart_rollup_host::path::RefPath;

        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(tc_ctx, &mut host, &context);

        // OperationCtx + ExecCtx are unread by `lookup_view_storage_balance`
        // but the type system requires a fully constructed `Ctx`. Use
        // placeholder backing values for everything that is not under test.
        let bootstrap_pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let placeholder_kt1 = ContractKt1Hash::from([0u8; 20]);
        let source = TezlinkImplicitAccount {
            path: RefPath::assert_from(b"/mock_source").into(),
            pkh: bootstrap_pkh.clone(),
        };
        let mut origination_nonce = OriginationNonce::default();
        let mut counter = 0u128;
        let level = BlockNumber { block_number: 0 };
        let now = Timestamp::from(0);
        let chain_id = ChainId::from([0, 0, 0, 0]);
        let source_public_key: Vec<u8> = Vec::new();

        let mut operation_ctx = OperationCtx {
            source: &source,
            origination_nonce: &mut origination_nonce,
            counter: &mut counter,
            level: &level,
            now: &now,
            chain_id: &chain_id,
            source_public_key: &source_public_key,
            crac_chain_depth: 0,
            crac_origin: None,
        };

        let exec_ctx = ExecCtx {
            sender: AddressHash::Implicit(bootstrap_pkh),
            amount: 0,
            self_address: AddressHash::Kt1(placeholder_kt1.clone()),
            balance: 0,
            contract_account: TezlinkOriginatedAccount {
                path: RefPath::assert_from(b"/mock_self").into(),
                kt1: placeholder_kt1,
            },
        };

        // The test exercises `lookup_view_storage_balance` directly,
        // not the enshrined-view hook, so journal and registry are
        // just placeholders that satisfy the type system.
        let mut journal = tezosx_journal::TezosXJournal::new(
            tezosx_journal::CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        let registry = tezosx_interfaces::testing::UnimplementedRegistry;
        let mut ctx = Ctx {
            tc_ctx: &mut tc_ctx,
            exec_ctx,
            operation_ctx: &mut operation_ctx,
            journal: &mut journal,
            registry: &registry,
        };

        // A KT1 that the fresh `MockKernelHost` has never seen.
        let unoriginated_kt1 =
            ContractKt1Hash::from_base58_check("KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton")
                .unwrap();
        let arena = Arena::new();
        let result =
            ctx.lookup_view_storage_balance(&unoriginated_kt1, "anything", &arena);
        assert!(
            matches!(result, Ok(None)),
            "expected Ok(None) for never-originated KT1, got {result:?}",
        );
    }

    fn encoded_size(v: &TypedValue) -> u64 {
        let arena = Arena::new();
        let mut gas = Gas::default();
        v.clone()
            .into_micheline_optimized_legacy(&arena, &mut gas)
            .unwrap()
            .encode(&mut gas)
            .unwrap()
            .unwrap()
            .len() as u64
    }

    #[test]
    fn big_map_new_initializes_total_bytes_to_zero() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);

        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();

        assert_eq!(total_bytes(ctx.host, ctx.context, &id).unwrap(), 0.into());
    }

    #[test]
    fn big_map_update_insert_adds_key_forfait_plus_value_size() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let value = TypedValue::String("hello".into());
        let value_size = encoded_size(&value);
        let old_total_bytes = total_bytes(ctx.host, ctx.context, &id).unwrap();

        ctx.big_map_update(&id, TypedValue::int(1), Some(value))
            .unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            (old_total_bytes.0 + (BYTES_SIZE_FOR_BIG_MAP_KEY + value_size)).into()
        );
    }

    #[test]
    fn big_map_update_overwrite_grows_by_value_diff() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let small = TypedValue::String("hi".into());
        let small_size = encoded_size(&small);

        ctx.big_map_update(&id, TypedValue::int(1), Some(small))
            .unwrap();

        let old_total_bytes = total_bytes(ctx.host, ctx.context, &id).unwrap();

        let big = TypedValue::String("hello, world".into());
        let big_size = encoded_size(&big);

        ctx.big_map_update(&id, TypedValue::int(1), Some(big))
            .unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            (old_total_bytes.0 + big_size - small_size).into()
        );
    }

    #[test]
    fn big_map_update_overwrite_shrinks_by_value_diff() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let big = TypedValue::String("hello, world".into());
        let big_size = encoded_size(&big);

        ctx.big_map_update(&id, TypedValue::int(1), Some(big))
            .unwrap();

        let old_total_bytes = total_bytes(ctx.host, ctx.context, &id).unwrap();

        let small = TypedValue::String("hi".into());
        let small_size = encoded_size(&small);

        ctx.big_map_update(&id, TypedValue::int(1), Some(small))
            .unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            (old_total_bytes.0 + small_size - big_size).into()
        );
    }

    #[test]
    fn big_map_update_delete_existing_subtracts_key_forfait_plus_prev_size() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let value = TypedValue::String("hello".into());
        let value_size = encoded_size(&value);

        ctx.big_map_update(&id, TypedValue::int(1), Some(value))
            .unwrap();

        let old_total_bytes = total_bytes(ctx.host, ctx.context, &id).unwrap();

        ctx.big_map_update(&id, TypedValue::int(1), None).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            (old_total_bytes.0 - (BYTES_SIZE_FOR_BIG_MAP_KEY + value_size)).into()
        );
    }

    #[test]
    fn big_map_update_delete_absent_is_noop() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let old_total_bytes = total_bytes(ctx.host, ctx.context, &id).unwrap();

        ctx.big_map_update(&id, TypedValue::int(42), None).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            old_total_bytes
        );
    }

    #[test]
    fn big_map_copy_replicates_source_total_bytes() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let src = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        ctx.big_map_update(
            &src,
            TypedValue::int(1),
            Some(TypedValue::String("a".into())),
        )
        .unwrap();
        ctx.big_map_update(
            &src,
            TypedValue::int(2),
            Some(TypedValue::String("bb".into())),
        )
        .unwrap();
        let src_total = total_bytes(ctx.host, ctx.context, &src).unwrap();

        let dest = ctx.big_map_copy(&src, false).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &dest).unwrap(),
            src_total
        );
    }

    #[test]
    fn big_map_update_insert_then_delete_returns_to_zero() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();

        ctx.big_map_update(
            &id,
            TypedValue::int(1),
            Some(TypedValue::String("hello".into())),
        )
        .unwrap();
        ctx.big_map_update(&id, TypedValue::int(1), None).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            0u64.into()
        );
    }

    #[test]
    fn big_map_update_multi_key_cumul_matches_sum() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();

        let values = [
            TypedValue::String("a".into()),
            TypedValue::String("bb".into()),
            TypedValue::String("ccc".into()),
            TypedValue::String("dddd".into()),
        ];
        let mut expected = num_bigint::BigInt::from(0u64);
        for (i, v) in values.iter().enumerate() {
            let size = encoded_size(v);
            ctx.big_map_update(&id, TypedValue::int(i as i64), Some(v.clone()))
                .unwrap();
            expected += BYTES_SIZE_FOR_BIG_MAP_KEY + size;
        }

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            Zarith(expected)
        );
    }

    #[test]
    fn big_map_copy_then_modify_source_leaves_dest_counter_unchanged() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let src = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        ctx.big_map_update(
            &src,
            TypedValue::int(1),
            Some(TypedValue::String("a".into())),
        )
        .unwrap();
        let dest = ctx.big_map_copy(&src, false).unwrap();
        let dest_total_after_copy = total_bytes(ctx.host, ctx.context, &dest).unwrap();

        ctx.big_map_update(
            &src,
            TypedValue::int(2),
            Some(TypedValue::String("bigger value".into())),
        )
        .unwrap();
        ctx.big_map_update(&src, TypedValue::int(1), None).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &dest).unwrap(),
            dest_total_after_copy
        );
    }

    #[test]
    fn big_map_realistic_cross_hook_sequence_stays_consistent() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);

        let src = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let inserts = [
            (1i64, TypedValue::String("x".into())),
            (2, TypedValue::String("yy".into())),
            (3, TypedValue::String("zzz".into())),
        ];
        for (k, v) in inserts.iter() {
            ctx.big_map_update(&src, TypedValue::int(*k), Some(v.clone()))
                .unwrap();
        }
        let src_total_after_inserts = total_bytes(ctx.host, ctx.context, &src).unwrap();

        let dest = ctx.big_map_copy(&src, false).unwrap();

        ctx.big_map_update(
            &src,
            TypedValue::int(1),
            Some(TypedValue::String("xx".into())),
        )
        .unwrap();
        let extra = encoded_size(&TypedValue::String("xx".into()))
            - encoded_size(&TypedValue::String("x".into()));
        assert_eq!(
            total_bytes(ctx.host, ctx.context, &src).unwrap(),
            Zarith(src_total_after_inserts.0.clone() + extra)
        );

        ctx.big_map_update(
            &dest,
            TypedValue::int(4),
            Some(TypedValue::String("wwww".into())),
        )
        .unwrap();
        let added =
            BYTES_SIZE_FOR_BIG_MAP_KEY + encoded_size(&TypedValue::String("wwww".into()));
        assert_eq!(
            total_bytes(ctx.host, ctx.context, &dest).unwrap(),
            Zarith(src_total_after_inserts.0 + added)
        );

        ctx.big_map_remove(&src).unwrap();
        let src_path = total_bytes_path(ctx.context, &src).unwrap();
        assert!(ctx.host.store_has(&src_path).unwrap().is_none());

        assert!(total_bytes(ctx.host, ctx.context, &dest).unwrap().0 > 0u64.into());
    }

    #[test]
    fn big_map_update_shrink_then_delete_returns_to_zero() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();

        ctx.big_map_update(
            &id,
            TypedValue::int(1),
            Some(TypedValue::String("hello, world".into())),
        )
        .unwrap();
        ctx.big_map_update(
            &id,
            TypedValue::int(1),
            Some(TypedValue::String("hi".into())),
        )
        .unwrap();
        ctx.big_map_update(&id, TypedValue::int(1), None).unwrap();

        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            0u64.into()
        );
    }

    #[test]
    fn big_map_remove_clears_total_bytes_path() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        ctx.big_map_update(
            &id,
            TypedValue::int(1),
            Some(TypedValue::String("hello".into())),
        )
        .unwrap();
        let path = total_bytes_path(ctx.context, &id).unwrap();
        assert!(ctx.host.store_has(&path).unwrap().is_some());

        ctx.big_map_remove(&id).unwrap();

        assert!(ctx.host.store_has(&path).unwrap().is_none());
    }

    #[test]
    fn total_bytes_lazily_migrates_pre_counter_big_map() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let entries = [
            TypedValue::String("a".into()),
            TypedValue::String("bb".into()),
            TypedValue::String("ccc".into()),
        ];
        let mut expected_sum: u64 = 0;
        for (i, v) in entries.iter().enumerate() {
            ctx.big_map_update(&id, TypedValue::int(i as i64), Some(v.clone()))
                .unwrap();
            expected_sum += BYTES_SIZE_FOR_BIG_MAP_KEY + encoded_size(v);
        }

        // Simulate a pre-counter big-map: drop the `total_bytes` path,
        // leaving the keys list and the values intact.
        let path = total_bytes_path(ctx.context, &id).unwrap();
        ctx.host.store_delete(&path).unwrap();
        assert!(ctx.host.store_has(&path).unwrap().is_none());

        // First read triggers the lazy migration.
        let migrated = total_bytes(ctx.host, ctx.context, &id).unwrap();
        assert_eq!(migrated, Zarith(expected_sum.into()));

        // Migration persisted the counter so subsequent reads are O(1).
        assert!(ctx.host.store_has(&path).unwrap().is_some());
        assert_eq!(total_bytes(ctx.host, ctx.context, &id).unwrap(), migrated);

        // Maintenance hooks now operate from the correct baseline: an
        // overwrite produces the expected delta against the migrated value.
        let new_value = TypedValue::String("dddd".into());
        let new_size = encoded_size(&new_value);
        let old_size = encoded_size(&entries[0]);
        ctx.big_map_update(&id, TypedValue::int(0), Some(new_value))
            .unwrap();
        assert_eq!(
            total_bytes(ctx.host, ctx.context, &id).unwrap(),
            Zarith((expected_sum + new_size - old_size).into()),
        );
    }

    #[test]
    fn clear_temporary_big_maps_removes_total_bytes_paths() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let temp1 = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        let temp2 = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        ctx.big_map_update(
            &temp1,
            TypedValue::int(1),
            Some(TypedValue::String("hello".into())),
        )
        .unwrap();
        ctx.big_map_update(
            &temp2,
            TypedValue::int(2),
            Some(TypedValue::String("world".into())),
        )
        .unwrap();
        let path1 = total_bytes_path(ctx.context, &temp1).unwrap();
        let path2 = total_bytes_path(ctx.context, &temp2).unwrap();
        assert!(ctx.host.store_has(&path1).unwrap().is_some());
        assert!(ctx.host.store_has(&path2).unwrap().is_some());

        clear_temporary_big_maps(ctx.host, ctx.context, ctx.next_temporary_id).unwrap();

        assert!(ctx.host.store_has(&path1).unwrap().is_none());
        assert!(ctx.host.store_has(&path2).unwrap().is_none());
    }

    /// The TezosX gateway exposes three synthetic views today:
    /// `staticcall_evm`, `originOf`, and `resolveAddress`. This test
    /// pins the enumeration (names and types) so off-chain consumers
    /// of the synthesized `/script` get a stable schema; any change to
    /// the enumeration must also update the matching dispatch arm in
    /// [`CtxTrait::try_dispatch_enshrined_view`].
    #[test]
    fn test_gateway_synthetic_views_enumeration() {
        let views = enshrined_synthetic_views(
            crate::enshrined_contracts::EnshrinedContracts::TezosXGateway,
        );
        assert_eq!(
            views,
            vec![
                (
                    "staticcall_evm",
                    Type::new_pair(Type::String, Type::Bytes),
                    Type::Bytes,
                ),
                (
                    "originOf",
                    Type::new_pair(Type::String, Type::Nat),
                    Type::new_or(
                        Type::Unit,
                        Type::new_or(Type::Nat, Type::new_pair(Type::Nat, Type::String),),
                    ),
                ),
                (
                    "resolveAddress",
                    Type::new_pair(Type::String, Type::new_pair(Type::Nat, Type::Nat)),
                    Type::new_option(Type::new_pair(Type::Nat, Type::String)),
                ),
            ]
        );
    }

    /// The ERC-20 wrapper has no synthetic views today; pin this so
    /// the encoded `views` list is empty for that contract.
    #[test]
    fn test_erc20_wrapper_has_no_synthetic_views() {
        let views = enshrined_synthetic_views(
            crate::enshrined_contracts::EnshrinedContracts::ERC20Wrapper,
        );
        assert!(views.is_empty());
    }

    /// Enforce that every view enumerated in
    /// [`enshrined_synthetic_views`] has a matching dispatch arm in
    /// [`CtxTrait::try_dispatch_enshrined_view`]. Without this test,
    /// adding a view to the enumeration without wiring up the
    /// dispatch arm would silently ship a `view "name" T1 T2`
    /// declaration in `/script` that the kernel refuses to serve at
    /// runtime — discoverability would break in the worst possible
    /// way.
    ///
    /// Mechanism: with `return_type = Type::Unit`, the
    /// `dispatch_staticcall_evm_view` body short-circuits to
    /// `Some(Ok(None))` before touching gas, the host, or the EVM
    /// runtime, so we can probe the hook safely with placeholder
    /// inputs. A missing dispatch arm trips the `unreachable!()` in
    /// the hook (rather than returning `None`), turning the desync
    /// into a hard test failure.
    #[test]
    fn enshrined_synthetic_views_dispatch_in_sync() {
        use crate::account_storage::{TezlinkImplicitAccount, TezlinkOriginatedAccount};
        use crate::address::OriginationNonce;
        use crate::enshrined_contracts::EnshrinedContracts;
        use mir::ast::michelson_address::AddressHash;
        use tezos_crypto_rs::hash::HashTrait;
        use tezos_smart_rollup_host::path::RefPath;

        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(tc_ctx, &mut host, &context);

        // OperationCtx + ExecCtx fields are unread by the
        // `return_type = Unit` early-exit path. Use placeholder
        // values for everything that is not under test.
        let bootstrap_pkh =
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap();
        let placeholder_kt1 = ContractKt1Hash::from([0u8; 20]);
        let source = TezlinkImplicitAccount {
            path: RefPath::assert_from(b"/mock_source").into(),
            pkh: bootstrap_pkh.clone(),
        };
        let mut origination_nonce = OriginationNonce::default();
        let mut counter = 0u128;
        let level = BlockNumber { block_number: 0 };
        let now = Timestamp::from(0);
        let chain_id = ChainId::from([0, 0, 0, 0]);
        let source_public_key: Vec<u8> = Vec::new();

        let mut operation_ctx = OperationCtx {
            source: &source,
            origination_nonce: &mut origination_nonce,
            counter: &mut counter,
            level: &level,
            now: &now,
            chain_id: &chain_id,
            source_public_key: &source_public_key,
            crac_chain_depth: 0,
            crac_origin: None,
        };

        let exec_ctx = ExecCtx {
            sender: AddressHash::Implicit(bootstrap_pkh),
            amount: 0,
            self_address: AddressHash::Kt1(placeholder_kt1.clone()),
            balance: 0,
            contract_account: TezlinkOriginatedAccount {
                path: RefPath::assert_from(b"/mock_self").into(),
                kt1: placeholder_kt1,
            },
        };

        let mut journal = tezosx_journal::TezosXJournal::new(
            tezosx_journal::CracId::new(1, 0),
            tezos_crypto_rs::hash::OperationHash::default(),
            tezos_ethereum::block::BlockConstants::dummy(),
        );
        // The synthetic-view dispatcher under test short-circuits on
        // `Type::Unit` before any registry call, so unwiring all
        // registry methods catches any future regression where a code
        // path accidentally starts calling into them.
        let registry = tezosx_interfaces::testing::UnimplementedRegistry;
        let mut ctx = Ctx {
            tc_ctx: &mut tc_ctx,
            exec_ctx,
            operation_ctx: &mut operation_ctx,
            journal: &mut journal,
            registry: &registry,
        };

        // Recover each enshrined contract's KT1 from its 22-byte
        // address-hash encoding (`[0x01][20-byte hash][0x00]`).
        let arena = Arena::new();
        for contract in [
            EnshrinedContracts::TezosXGateway,
            EnshrinedContracts::ERC20Wrapper,
        ] {
            let bytes = contract.address_hash_bytes();
            let kt1 = ContractKt1Hash::try_from_bytes(&bytes[1..21]).unwrap();
            for (name, _, _) in enshrined_synthetic_views(contract) {
                let result = ctx.try_dispatch_enshrined_view(
                    &kt1,
                    name,
                    &TypedValue::Unit,
                    // `Type::Unit` (not `Type::Bytes`) makes the
                    // `staticcall_evm` body short-circuit to
                    // `Some(Ok(None))`, avoiding any gas charge or
                    // EVM runtime call.
                    &Type::Unit,
                    &arena,
                );
                assert!(
                    result.is_some(),
                    "synthetic view {name:?} on {contract:?} is enumerated \
                     in enshrined_synthetic_views but try_dispatch_enshrined_view \
                     returned None (no dispatch arm)"
                );
            }
        }
    }

    #[test]
    fn interpret_context_alloc_permanent_adds_33() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let _ = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            BYTES_SIZE_FOR_EMPTY.into()
        );
    }

    #[test]
    fn interpret_context_alloc_temporary_is_zero() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let _ = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        assert_eq!(ctx.interpret_context.lazy_storage_size_diff, 0.into());
    }

    #[test]
    fn interpret_context_insert_permanent_adds_65_plus_value_size() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let value = TypedValue::String("hello".into());
        let value_size = encoded_size(&value);
        ctx.big_map_update(&id, TypedValue::int(1), Some(value))
            .unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            (BYTES_SIZE_FOR_EMPTY + BYTES_SIZE_FOR_BIG_MAP_KEY + value_size).into()
        );
    }

    #[test]
    fn interpret_context_overwrite_permanent_adds_value_size_diff() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let small = TypedValue::String("hi".into());
        let big = TypedValue::String("hello, world".into());
        let small_size = encoded_size(&small);
        let big_size: BigInt = encoded_size(&big).into();
        ctx.big_map_update(&id, TypedValue::int(1), Some(small))
            .unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            (BYTES_SIZE_FOR_EMPTY + BYTES_SIZE_FOR_BIG_MAP_KEY + small_size).into()
        );
        ctx.interpret_context.lazy_storage_size_diff = 0.into();
        ctx.big_map_update(&id, TypedValue::int(1), Some(big))
            .unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            (big_size - small_size).into()
        );
    }

    #[test]
    fn interpret_context_delete_permanent_subtracts_65_plus_prev_size() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let value = TypedValue::String("hello".into());
        let value_size = encoded_size(&value);
        ctx.big_map_update(&id, TypedValue::int(1), Some(value))
            .unwrap();
        ctx.interpret_context.lazy_storage_size_diff = 0.into();
        ctx.big_map_update(&id, TypedValue::int(1), None).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(
                -((BYTES_SIZE_FOR_BIG_MAP_KEY + value_size) as i64)
            ))
        );
    }

    #[test]
    fn interpret_context_copy_permanent_adds_33_plus_source_total() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let src = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let v1 = TypedValue::String("a".into());
        let v2 = TypedValue::String("bb".into());
        let v1_size = encoded_size(&v1);
        let v2_size = encoded_size(&v2);
        ctx.big_map_update(&src, TypedValue::int(1), Some(v1))
            .unwrap();
        ctx.big_map_update(&src, TypedValue::int(2), Some(v2))
            .unwrap();
        let src_total = (2 * BYTES_SIZE_FOR_BIG_MAP_KEY + v1_size + v2_size) as i64;
        ctx.interpret_context.lazy_storage_size_diff = 0.into();
        let _dest = ctx.big_map_copy(&src, false).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(BYTES_SIZE_FOR_EMPTY as i64 + src_total))
        );
    }

    #[test]
    fn interpret_context_copy_promotes_temp_to_perm_adds_33_plus_total() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        // Create a temporary big-map and fill it — no accumulator activity
        // for the alloc nor the updates (the id is in the temp range).
        let temp_src = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        let v1 = TypedValue::String("a".into());
        let v2 = TypedValue::String("bb".into());
        let v1_size = encoded_size(&v1);
        let v2_size = encoded_size(&v2);
        ctx.big_map_update(&temp_src, TypedValue::int(1), Some(v1))
            .unwrap();
        ctx.big_map_update(&temp_src, TypedValue::int(2), Some(v2))
            .unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(0)),
            "alloc and updates on a temp big-map must not accumulate"
        );

        let src_total = (2 * BYTES_SIZE_FOR_BIG_MAP_KEY + v1_size + v2_size) as i64;
        // Promote temp -> permanent. This is what MIR's
        // `dump_big_map_updates` does at the contract-storage boundary
        // when the final storage references a freshly-allocated
        // big-map: it calls `big_map_copy(temp_id, temporary=false)`.
        let _perm_dest = ctx.big_map_copy(&temp_src, false).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(BYTES_SIZE_FOR_EMPTY as i64 + src_total)),
            "temp→perm promotion must contribute 33 + total_bytes(temp_src)"
        );
    }

    #[test]
    fn interpret_context_copy_temporary_is_zero() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let src = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        ctx.interpret_context.lazy_storage_size_diff = 0.into();
        let _dest = ctx.big_map_copy(&src, true).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(0))
        );
    }

    #[test]
    fn interpret_context_remove_permanent_subtracts_33_plus_total() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        let value = TypedValue::String("hello".into());
        let value_size = encoded_size(&value);
        ctx.big_map_update(&id, TypedValue::int(1), Some(value))
            .unwrap();
        ctx.interpret_context.lazy_storage_size_diff = 0.into();
        ctx.big_map_remove(&id).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(
                -((BYTES_SIZE_FOR_EMPTY + BYTES_SIZE_FOR_BIG_MAP_KEY + value_size)
                    as i64)
            ))
        );
    }

    #[test]
    fn interpret_context_update_on_temporary_skipped() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let id = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        ctx.big_map_update(
            &id,
            TypedValue::int(1),
            Some(TypedValue::String("hi".into())),
        )
        .unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff,
            Zarith(BigInt::from(0))
        );
    }

    #[test]
    fn interpret_context_clear_temporaries_does_not_touch_accumulator() {
        let mut host = MockKernelHost::default();
        let context = TezlinkContext::init_context();
        make_default_ctx!(ctx, &mut host, &context);
        let perm = ctx.big_map_new(&Type::Int, &Type::String, false).unwrap();
        ctx.big_map_update(
            &perm,
            TypedValue::int(1),
            Some(TypedValue::String("perm".into())),
        )
        .unwrap();
        let _ = ctx.big_map_new(&Type::Int, &Type::String, true).unwrap();
        let before_clear = ctx.interpret_context.lazy_storage_size_diff.clone();
        clear_temporary_big_maps(ctx.host, ctx.context, ctx.next_temporary_id).unwrap();
        assert_eq!(
            ctx.interpret_context.lazy_storage_size_diff, before_clear,
            "clear_temporary_big_maps must not touch the accumulator",
        );
    }
}

#[cfg(test)]
pub(crate) mod mock {
    use super::*;
    use mir::ast::{ByteReprTrait, Entrypoint};
    use num_bigint::BigInt;
    use std::collections::HashMap;
    use tezos_crypto_rs::hash::HashTrait;
    use tezos_smart_rollup_host::path::RefPath;

    /// Mock execution context for testing enshrined contracts.
    /// Implements CtxTrait and HasHost with configurable values.
    ///
    /// Lifetimes are decoupled (host/journal/registry independently) so
    /// tests can construct `MockCtx` from a long-lived host and shorter-
    /// lived journal/registry locals without forcing the borrow checker
    /// to unify them under a single `'a`.
    pub struct MockCtx<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> {
        pub host: &'h mut Host,
        pub journal: &'j mut tezosx_journal::TezosXJournal,
        pub registry: &'r R,
        pub sender: AddressHash,
        pub amount: i64,
        pub level: BigUint,
        pub now: BigInt,
        pub operation_group_hash: OperationHash,
        pub operation_gas: crate::gas::TezlinkOperationGas,
        pub contract_account: TezlinkOriginatedAccount,
        pub operation_counter: u128,
        pub context: crate::context::TezlinkContext,
        pub crac_chain_depth: u32,
        pub crac_origin: Option<Contract>,
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry>
        MockCtx<'h, 'j, 'r, Host, R>
    {
        pub fn new(
            host: &'h mut Host,
            journal: &'j mut tezosx_journal::TezosXJournal,
            registry: &'r R,
            sender: AddressHash,
            amount: i64,
        ) -> Self {
            Self {
                host,
                journal,
                registry,
                sender,
                amount,
                level: 1u32.into(),
                now: 0.into(),
                operation_group_hash: OperationHash::from([0u8; 32]),
                operation_gas: crate::gas::TezlinkOperationGas::default(),
                operation_counter: 0,
                contract_account: TezlinkOriginatedAccount {
                    path: RefPath::assert_from(b"/mock").into(),
                    kt1: ContractKt1Hash::from([0u8; 20]),
                },
                context: crate::context::TezlinkContext::init_context(),
                crac_chain_depth: 0,
                crac_origin: None,
            }
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasCracChainDepth
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn crac_chain_depth(&self) -> u32 {
            self.crac_chain_depth
        }

        fn crac_origin(&self) -> Option<Contract> {
            self.crac_origin.clone()
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasJournal
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn journal(&mut self) -> &mut tezosx_journal::TezosXJournal {
            self.journal
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasRegistry
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        type R = R;
        fn registry(&self) -> &Self::R {
            self.registry
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry>
        HasCrossRuntime<Host> for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn cross_runtime_split(
            &mut self,
        ) -> (&mut Host, &mut tezosx_journal::TezosXJournal, &R) {
            (self.host, self.journal, self.registry)
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasOriginLookup
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn read_origin_for_address(
            &self,
            address: &AddressHash,
        ) -> Result<Option<tezosx_interfaces::Origin>, tezos_storage::error::Error>
        {
            self.context.read_origin_for_address(&*self.host, address)
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasHost<Host>
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn host(&mut self) -> &mut Host {
            self.host
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasContractAccount
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        type Account = TezlinkOriginatedAccount;
        fn contract_account(&self) -> &Self::Account {
            &self.contract_account
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasOperationGas
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn operation_gas(&mut self) -> &mut crate::gas::TezlinkOperationGas {
            &mut self.operation_gas
        }
    }

    impl<'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> HasSourcePublicKey
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn source_public_key(&self) -> &[u8] {
            &[]
        }
    }

    impl<'a, 'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry>
        TypecheckingCtx<'a> for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn gas(&mut self) -> &mut mir::gas::Gas {
            &mut self.operation_gas.remaining
        }

        fn lookup_entrypoints(
            &mut self,
            _address: &AddressHash,
        ) -> Option<HashMap<Entrypoint, Type>> {
            None
        }

        fn big_map_get_type(
            &mut self,
            _id: &BigMapId,
        ) -> Result<Option<(Type, Type)>, LazyStorageError> {
            Ok(None)
        }
    }

    impl<'a, 'h, 'j, 'r, Host: StorageV1, R: tezosx_interfaces::Registry> CtxTrait<'a>
        for MockCtx<'h, 'j, 'r, Host, R>
    {
        fn sender(&self) -> AddressHash {
            self.sender.clone()
        }

        fn source(&self) -> PublicKeyHash {
            PublicKeyHash::from_b58check("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx").unwrap()
        }

        fn amount(&self) -> i64 {
            self.amount
        }

        fn self_address(&self) -> AddressHash {
            // KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi
            AddressHash::from_bytes(&[
                0x01, 0x29, 0x58, 0x93, 0x60, 0xad, 0xf1, 0x56, 0x94, 0xac, 0x33, 0x0d,
                0xe5, 0x9f, 0x46, 0x44, 0x15, 0xb5, 0xf7, 0xea, 0x69, 0x00,
            ])
            .unwrap()
        }

        fn balance(&self) -> i64 {
            0
        }

        fn level(&self) -> BigUint {
            self.level.clone()
        }

        fn min_block_time(&self) -> BigUint {
            1u32.into()
        }

        fn chain_id(&self) -> ChainId {
            ChainId::try_from_bytes(&[0u8; 4]).unwrap()
        }

        fn voting_power(&self, _: &PublicKeyHash) -> BigUint {
            0u32.into()
        }

        fn now(&self) -> BigInt {
            self.now.clone()
        }

        fn total_voting_power(&self) -> BigUint {
            1u32.into()
        }

        fn operation_group_hash(&self) -> &OperationHash {
            &self.operation_group_hash
        }

        fn origination_counter(&mut self) -> u32 {
            0
        }

        fn operation_counter(&mut self) -> u128 {
            self.operation_counter += 1;
            self.operation_counter
        }

        fn lazy_storage(&mut self) -> Box<&mut dyn LazyStorage<'a>> {
            unimplemented!("MockCtx does not support lazy_storage")
        }

        fn lookup_view_storage_balance(
            &mut self,
            _contract: &ContractKt1Hash,
            _name: &str,
            _arena: &'a Arena<Micheline<'a>>,
        ) -> Result<
            Option<(MichelineView<Micheline<'a>>, Micheline<'a>, Vec<u8>, i64)>,
            mir::context::LookupViewError,
        > {
            Ok(None)
        }

        fn set_view_context(
            &mut self,
            _self_address: AddressHash,
            _sender: AddressHash,
            _amount: i64,
            _balance: i64,
        ) {
            // MockCtx does not support views
        }
    }
}
