// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::collections::BTreeMap;

use crate::account_storage::{
    TezlinkAccount, TezlinkImplicitAccount, TezlinkOriginatedAccount,
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
};
use num_bigint::BigUint;
use primitive_types::H256;
use tezos_crypto_rs::blake2b::digest_256;
use tezos_crypto_rs::hash::{ChainId, ContractKt1Hash};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::{Narith, Zarith};
use tezos_evm_runtime::runtime::Runtime;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup::host::RuntimeError;
use tezos_smart_rollup::types::Timestamp;
use tezos_storage::{read_nom_value, read_optional_nom_value, store_bin};
use tezos_tezlink::enc_wrappers::{BlockNumber, ScriptExprHash};
use tezos_tezlink::lazy_storage_diff::{
    Alloc, BigMapDiff, Copy, LazyStorageDiff, LazyStorageDiffList, StorageDiff, Update,
};
use tezos_tezlink::operation_result::TransferError;
use typed_arena::Arena;

pub struct TcCtx<'operation, Host: Runtime> {
    pub host: &'operation mut Host,
    pub context: &'operation Context,
    pub gas: &'operation mut crate::gas::TezlinkOperationGas,
    pub big_map_diff: &'operation mut BTreeMap<Zarith, StorageDiff>,
    pub next_temporary_id: BigMapId,
}

pub struct OperationCtx<'operation> {
    // In reality, 'source' and 'origination_nonce' have
    // a 'batch lifetime. Downgrade it to an 'operation
    // lifetime is not a problem for the compiler.
    // However, it could be misleading in terms of comprehension
    pub source: &'operation TezlinkImplicitAccount,
    pub origination_nonce: &'operation mut OriginationNonce,
    pub counter: &'operation mut u128,
    // 'level', 'now' and 'chain_id' should outlive operation in reality
    // So having them downcast to 'operation is not a problem
    pub level: &'operation BlockNumber,
    pub now: &'operation Timestamp,
    pub chain_id: &'operation ChainId,
}

pub struct ExecCtx {
    pub sender: AddressHash,
    pub amount: i64,
    pub self_address: AddressHash,
    pub balance: i64,
}

pub struct Ctx<'a, 'operation, Host: Runtime> {
    pub tc_ctx: &'a mut TcCtx<'operation, Host>,
    pub exec_ctx: ExecCtx,
    pub operation_ctx: &'a mut OperationCtx<'operation>,
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
        host: &mut impl Runtime,
        sender_account: &impl TezlinkAccount,
        dest_account: &TezlinkOriginatedAccount,
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
        Ok(Self {
            sender,
            amount,
            self_address,
            balance,
        })
    }
}

impl<'a, Host: Runtime> TypecheckingCtx<'a> for TcCtx<'a, Host> {
    fn gas(&mut self) -> &mut mir::gas::Gas {
        &mut self.gas.current_gas
    }

    fn lookup_entrypoints(
        &self,
        address: &AddressHash,
    ) -> Option<std::collections::HashMap<mir::ast::Entrypoint, mir::ast::Type>> {
        get_contract_entrypoint(self.host, self.context, address)
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

        let key_type =
            Micheline::decode_raw(&arena, &encoded_key_type)?.parse_ty(self.gas())?;

        let encoded_value_type = match self.host.store_read_all(&value_type_path) {
            Ok(key_type) => Ok(key_type),
            Err(RuntimeError::PathNotFound) => return Ok(None),
            Err(err) => Err(err),
        }?;
        let value_type =
            Micheline::decode_raw(&arena, &encoded_value_type)?.parse_ty(self.gas())?;

        Ok(Some((key_type, value_type)))
    }
}

impl<'a, Host: Runtime> TypecheckingCtx<'a> for Ctx<'_, '_, Host> {
    fn gas(&mut self) -> &mut mir::gas::Gas {
        self.tc_ctx.gas()
    }

    fn lookup_entrypoints(
        &self,
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

impl<'a, Host: Runtime> CtxTrait<'a> for Ctx<'_, 'a, Host> {
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

    fn now(&self) -> num_bigint::BigInt {
        i64::from(*self.operation_ctx.now).into()
    }

    fn total_voting_power(&self) -> BigUint {
        1u32.into()
    }

    fn operation_group_hash(&self) -> [u8; 32] {
        self.operation_ctx.origination_nonce.operation.0 .0
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

    fn lookup_view_and_storage(
        &self,
        contract: ContractKt1Hash,
        view_name: &str,
        arena: &'a Arena<Micheline<'a>>,
    ) -> Option<(
        mir::typechecker::MichelineView<Micheline<'a>>,
        (Micheline<'a>, Vec<u8>),
    )> {
        let account =
            TezlinkOriginatedAccount::from_kt1(self.tc_ctx.context, &contract).ok()?;
        let serialized_script = account.code(self.tc_ctx.host).ok()?;
        let MichelineContractScript {
            code: _,
            parameter_ty: _,
            storage_ty,
            views,
        } = Micheline::decode_raw(arena, &serialized_script)
            .ok()?
            .split_script()
            .ok()?;
        let view = views.get(view_name)?;
        let owned_view = MichelineView {
            input_type: view.input_type.clone(),
            output_type: view.output_type.clone(),
            code: view.code.clone(),
        };
        let storage = account.storage(self.tc_ctx.host).ok()?;
        Some((owned_view, (storage_ty.clone(), storage)))
    }
}

impl<Host: Runtime> Ctx<'_, '_, Host> {
    pub fn host(&mut self) -> &mut Host {
        self.tc_ctx.host
    }
}

impl<Host: Runtime> TcCtx<'_, Host> {
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
        key_hash: Vec<u8>,
        key: Vec<u8>,
        value: Option<Vec<u8>>,
    ) {
        let update = Update {
            key_hash: H256::from_slice(&key_hash).into(),
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
            self.next_temporary_id = new_id.succ();
            Ok(new_id)
        } else {
            let next_id_path = next_id_path(self.context)?;
            let id: BigMapId =
                read_nom_value(self.host, &next_id_path).unwrap_or(0.into());
            store_bin(&id.succ(), self.host, &next_id_path)
                .map_err(|e| LazyStorageError::BinWriteError(e.to_string()))?;
            Ok(id)
        }
    }
}

/// Function to retrieve the hash of a TypedValue.
/// Used to retrieve the path where a value is stored in the
/// lazy storage.
fn hash_key(key: TypedValue<'_>) -> Vec<u8> {
    let parser = Parser::new();
    let key_encoded = key.into_micheline_optimized_legacy(&parser.arena).encode();
    digest_256(&key_encoded)
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
    fn get(host: &mut impl Runtime, context: &Context, id: &BigMapId) -> Self {
        let path = keys_of_big_map(context, id).unwrap();
        read_nom_value(host, &path).unwrap_or(BigMapKeys { keys: vec![] })
    }

    fn add_key(
        host: &mut impl Runtime,
        context: &Context,
        id: &BigMapId,
        key: &[u8],
    ) -> Result<(), LazyStorageError> {
        let path = keys_of_big_map(context, id)?;
        let size = host.store_value_size(&path).unwrap_or(0usize);
        host.store_write(&path, key, size)?;
        Ok(())
    }

    fn remove_key(
        host: &mut impl Runtime,
        context: &Context,
        id: &BigMapId,
        key: &[u8],
    ) -> Result<(), LazyStorageError> {
        let path = keys_of_big_map(context, id)?;
        let key = ScriptExprHash(H256::from_slice(key));
        let mut big_map_keys: Self = read_nom_value(host, &path)
            .map_err(|e| LazyStorageError::NomReadError(e.to_string()))?;
        big_map_keys.keys.retain(|elt| elt != &key);
        store_bin(&big_map_keys, host, &path)
            .map_err(|e| LazyStorageError::BinWriteError(e.to_string()))?;
        Ok(())
    }

    fn remove_keys_in_storage(
        host: &mut impl Runtime,
        context: &Context,
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
            let value_path = value_path(context, id, key.0.as_bytes())?;
            host.store_delete(&value_path)?;
        }

        // Remove keys for the big_map
        host.store_delete(&path)?;

        Ok(())
    }

    fn copy_keys_in_storage(
        host: &mut impl Runtime,
        context: &Context,
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
            let key_hashed = key.0.as_bytes();
            let source_value_path = value_path(context, source, key_hashed)?;
            let dest_value_path = value_path(context, dest, key_hashed)?;

            // Copy the value at from source path to dest path
            let value = host.store_read_all(&source_value_path)?;
            host.store_write_all(&dest_value_path, &value)?;
        }

        let dest_path = keys_of_big_map(context, dest)?;
        store_bin(&big_map_keys, host, &dest_path)
            .map_err(|e| LazyStorageError::BinWriteError(e.to_string()))?;

        Ok(())
    }
}

impl<'a, Host: Runtime> LazyStorage<'a> for TcCtx<'a, Host> {
    fn big_map_get(
        &mut self,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        let value_path = value_path(self.context, id, &hash_key(key.clone()))?;
        if self.host.store_has(&value_path)?.is_none() {
            return Ok(None);
        }

        let value_type_path = value_type_path(self.context, id)?;
        let encoded_value_type = self.host.store_read_all(&value_type_path)?;
        let value_type = Micheline::decode_raw(arena, &encoded_value_type)?;

        let encoded_value = self.host.store_read_all(&value_path)?;
        let value = Micheline::decode_raw(arena, &encoded_value)?;
        Ok(Some(value.typecheck_value(self, &value_type)?))
    }

    fn big_map_mem(
        &mut self,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<bool, LazyStorageError> {
        let path = value_path(self.context, id, &hash_key(key.clone()))?;
        Ok(self.host.store_has(&path)?.is_some())
    }

    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError> {
        let parser = Parser::new();
        let key_encoded = key.into_micheline_optimized_legacy(&parser.arena).encode();
        let key_hashed = digest_256(&key_encoded);
        let value_path = value_path(self.context, id, &key_hashed)?;
        match value {
            None => {
                if self.host.store_has(&value_path)?.is_some() {
                    self.host.store_delete(&value_path)?;
                    BigMapKeys::remove_key(self.host, self.context, id, &key_hashed)?;
                }

                // Write the update in the big_map_diff
                self.big_map_diff_update(&id.value, key_hashed, key_encoded, None);
                Ok(())
            }
            Some(v) => {
                let arena = Arena::new();
                let encoded = v.into_micheline_optimized_legacy(&arena).encode();
                if self.host.store_has(&value_path)?.is_none() {
                    // We should write the key in the list only if it's an add in the big_map not an update
                    BigMapKeys::add_key(self.host, self.context, id, &key_hashed)?;
                }

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
        let key_type_encoded = key_type.into_micheline_optimized_legacy(&arena).encode();
        let value_type_encoded =
            value_type.into_micheline_optimized_legacy(&arena).encode();
        self.host
            .store_write_all(&value_type_path, &value_type_encoded)?;
        self.host
            .store_write_all(&key_type_path, &key_type_encoded)?;

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

        // Write in the diff that there was a copy
        self.big_map_diff_copy(dest_id.value.clone(), id.value.clone());
        Ok(dest_id)
    }

    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError> {
        // Remove the key type of the big_map
        let key_type_path = key_type_path(self.context, id)?;
        self.host.store_delete(&key_type_path)?;

        // Remove the value type of the big_map
        let value_type_path = value_type_path(self.context, id)?;
        self.host.store_delete(&value_type_path)?;

        // Removing the content of the big_map
        BigMapKeys::remove_keys_in_storage(self.host, self.context, id)?;

        // Write in the diff that there was a remove
        self.big_map_diff_remove(id.value.clone());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gas::TezlinkOperationGas;
    use mir::ast::big_map::{
        dump_big_map_updates, BigMap, BigMapContent, BigMapFromId, BigMapId,
    };
    use std::collections::BTreeMap;
    use tezos_evm_runtime::runtime::MockKernelHost;

    macro_rules! make_default_ctx {
        ($ctx:ident, $host: expr, $context: expr) => {
            let mut gas = TezlinkOperationGas::default();
            let mut $ctx = TcCtx {
                host: $host,
                context: $context,
                gas: &mut gas,
                big_map_diff: &mut BTreeMap::new(),
                next_temporary_id: BigMapId { value: (-1).into() },
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

    fn assert_big_map_eq<'a, Host: Runtime>(
        ctx: &mut TcCtx<'a, Host>,
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

    fn assert_big_map_removed<'a, Host: Runtime>(
        ctx: &TcCtx<'a, Host>,
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
            let value_path = value_path(ctx.context, id, key.0.as_bytes()).unwrap();
            assert!(
                ctx.host.store_has(&value_path).unwrap().is_none(),
                "{key:?} should have been removed from the storage"
            );
        }
    }

    #[test]
    fn test_map_from_memory() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &Context::init_context());
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
        make_default_ctx!(storage, &mut host, &Context::init_context());
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
        make_default_ctx!(storage, &mut host, &Context::init_context());
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
        make_default_ctx!(storage, &mut host, &Context::init_context());
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
            keys: vec![ScriptExprHash(H256::from_slice(&hash_key(key)))],
        };
        assert_big_map_removed(&storage, &map_id, &removed_keys);

        // Verify that the big_map_mem function returns the expected result
        assert!(!storage.big_map_mem(&map_id, &TypedValue::int(0)).unwrap());
    }

    #[test]
    fn test_remove_with_dump() {
        let mut host = MockKernelHost::default();
        make_default_ctx!(storage, &mut host, &Context::init_context());
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
            &Arena::new(),
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
        let key_type = mir::ast::Micheline::prim0(mir::lexer::Prim::nat).encode();
        let value_type = mir::ast::Micheline::prim0(mir::lexer::Prim::unit).encode();
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
}
