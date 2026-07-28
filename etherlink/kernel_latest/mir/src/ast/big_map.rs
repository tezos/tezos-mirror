// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! `big_map` typed representation and utilities for working with `big_map`s.

use num_bigint::{BigInt, Sign};
use num_traits::{One, Zero};
use rpds::RedBlackTreeMap;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    mem,
    rc::Rc,
};
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::nom::NomReader;
use tezos_data_encoding::types::Zarith;
use tezos_smart_rollup_host::{path::PathError, runtime::RuntimeError};
use typed_arena::Arena;

use super::{
    CreateContract, Micheline, MichelsonList, OperationInfo, TransferTokens, Type,
    TypedValue,
};
use crate::gas::{tc_cost, OutOfGas};
use crate::serializer::DecodeError;
use crate::typechecker::TcError;

/// Id of big map in the lazy storage.
#[derive(BinWriter, NomReader, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigMapId {
    /// Value of the ID
    pub value: Zarith,
}

impl Display for BigMapId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Zarith(ref int_value) = self.value;
        write!(f, "{int_value}")
    }
}

impl From<i64> for BigMapId {
    fn from(n: i64) -> Self {
        BigMapId {
            value: Zarith(BigInt::from(n)),
        }
    }
}

impl From<BigInt> for BigMapId {
    fn from(n: BigInt) -> Self {
        BigMapId { value: Zarith(n) }
    }
}

impl BigMapId {
    /// Get successor of the id
    pub fn succ(&self) -> Self {
        let Zarith(ref int_value) = self.value;
        let result = if self.is_temporary() {
            int_value - BigInt::one()
        } else {
            int_value + BigInt::one()
        };
        BigMapId {
            value: Zarith(result),
        }
    }

    /// Increment the mutable id
    pub fn incr(&mut self) {
        let Zarith(ref int_value) = self.value;
        let result = if self.is_temporary() {
            int_value - BigInt::one()
        } else {
            int_value + BigInt::one()
        };
        self.value = Zarith(result);
    }

    /// Decrement the id
    ///
    /// If there's no predecessor return false, otherwise true
    pub fn dec(&mut self) -> bool {
        let Zarith(ref int_value) = self.value;
        let result = if self.is_temporary() {
            int_value + BigInt::one()
        } else {
            int_value - BigInt::one()
        };
        let has_pred = !result.eq(&BigInt::zero());
        if has_pred {
            self.value = Zarith(result);
        }
        has_pred
    }

    /// Tells if a big_map id is temporary
    pub fn is_temporary(&self) -> bool {
        self.value.0.sign() == Sign::Minus
    }
}

/// Represents the content of a big_map value in the case it is
/// backed by a map in the lazy storage.
///
/// It is split into two parts - the id pointing to the map in the
/// lazy storage, and the in-memory overlay that carries a diff from
/// the map in the lazy storage.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BigMapFromId<'a> {
    /// Id of the big map in the lazy storage.
    pub id: BigMapId,

    /// In-memory part, carries the diff that is to be applied to the map in the
    /// storage.
    ///
    /// Normally, execution of all writing instructions update this part, and at
    /// certain key points like the end of the contract execution this diff is
    /// dumped into the storage. Change in storage can be applied in-place or,
    /// if necessary, with copy of the stored map.
    pub overlay: RedBlackTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>,
}

/// The content of a big map, either backed by a map in the lazy
/// storage or fully in memory
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BigMapContent<'a> {
    /// Big map can be backed by no map in the lazy storage and yet
    /// stay fully in memory.
    InMemory(RedBlackTreeMap<TypedValue<'a>, TypedValue<'a>>),
    /// Otherwise they come from the lazy storage and have both an
    /// identifier and an overlay
    FromId(BigMapFromId<'a>),
}

/// An empty in-memory big map, used as a cheap placeholder when moving the
/// content out of a `&mut BigMap` without cloning.
impl Default for BigMapContent<'_> {
    fn default() -> Self {
        BigMapContent::InMemory(RedBlackTreeMap::new())
    }
}

/// Represents a big_map value.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BigMap<'a> {
    /// Content of the big map.
    pub content: BigMapContent<'a>,

    /// Type of the map key.
    pub key_type: Type,

    /// Type of the map value.
    pub value_type: Type,
}

impl<'a> BigMap<'a> {
    /// Create an in-memory big map from the given entries.
    pub fn new(
        key_type: Type,
        value_type: Type,
        map: RedBlackTreeMap<TypedValue<'a>, TypedValue<'a>>,
    ) -> Self {
        Self {
            content: BigMapContent::InMemory(map),
            key_type,
            value_type,
        }
    }

    /// Michelson's `EMPTY_BIG_MAP`
    pub fn empty(key_type: Type, value_type: Type) -> Self {
        Self::new(key_type, value_type, RedBlackTreeMap::new())
    }

    /// Michelson's `GET`.
    pub fn get(
        &self,
        arena: &'a Arena<Micheline<'a>>,
        key: &TypedValue,
        storage: &mut (impl LazyStorage<'a> + ?Sized),
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        Ok(match &self.content {
            BigMapContent::InMemory(m) => m.get(key).cloned(),
            BigMapContent::FromId(BigMapFromId { id, overlay }) => {
                match overlay.get(key) {
                    // If the key is mentioned in the overlay, the associated value is
                    // always used, even if it is `None` (and `get` returned
                    // `Some(None)`) which means removal.
                    Some(change) => change.clone(),
                    None => storage.big_map_get(arena, id, key, &self.value_type)?,
                }
            }
        })
    }

    /// Michelson's `MEM`.
    pub fn mem(
        &self,
        key: &TypedValue,
        storage: &mut (impl LazyStorage<'a> + ?Sized),
    ) -> Result<bool, LazyStorageError> {
        Ok(match &self.content {
            BigMapContent::InMemory(m) => m.get(key).is_some(),
            BigMapContent::FromId(BigMapFromId { id, overlay }) => {
                match overlay.get(key) {
                    // If the key is mentioned in the overlay, the associated value is
                    // always used, even if it is `None` (and `get` returned
                    // `Some(None)`) which means removal.
                    Some(change) => change.is_some(),
                    None => storage.big_map_mem(id, key)?,
                }
            }
        })
    }

    /// Michelson's `UPDATE`.
    pub fn update(&mut self, key: TypedValue<'a>, value: Option<TypedValue<'a>>) {
        match &mut self.content {
            BigMapContent::InMemory(m) => match value {
                Some(value) => {
                    m.insert_mut(key, value);
                }
                None => {
                    m.remove_mut(&key);
                }
            },
            BigMapContent::FromId(BigMapFromId { id: _, overlay }) => {
                overlay.insert_mut(key, value);
            }
        }
    }

    /// Length of the in-memory part of the big map
    pub fn len_for_gas(&self) -> usize {
        match &self.content {
            BigMapContent::InMemory(m) => m.size(),
            BigMapContent::FromId(BigMapFromId { id: _, overlay }) => overlay.size(),
        }
    }
}

/// Errors that can happen when working with lazy storage.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum LazyStorageError {
    /// Error when computing path in storage
    #[error("Error generating path: {0}")]
    PathError(#[from] PathError),
    /// Error when decoding value from storage
    #[error("Error decoding stored value: {0}")]
    DecodeError(#[from] DecodeError),
    /// Runtime error while manipulating storage
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] RuntimeError),
    /// Typecheck error when returning value from storage
    #[error("Error typechecking stored value: {0}")]
    TcError(#[from] TcError),
    /// Error when using nom_read to interact with the storage
    #[error("Error nom_reading stored value: {0}")]
    NomReadError(String),
    /// Error when using bin_write to interact with the storage
    ///
    /// Wrapped in `Rc` because [`tezos_data_encoding::enc::BinError`]
    /// does not implement [`Clone`] (its `IOError` variant carries a
    /// non-Clone `std::io::Error`), and this enum derives `Clone`.
    #[error("Error bin_writing value to store: {0}")]
    BinWriteError(std::rc::Rc<tezos_data_encoding::enc::BinError>),
    /// The requested big_map id was not found in the lazy storage.
    #[error("big map with ID {0} not found in the lazy storage")]
    BigMapNotFound(BigMapId),
    /// Gas exhaustion while serializing a key.
    #[error("{0}")]
    OutOfGasError(#[from] OutOfGas),
}

impl From<tezos_data_encoding::enc::BinError> for LazyStorageError {
    fn from(err: tezos_data_encoding::enc::BinError) -> Self {
        LazyStorageError::BinWriteError(std::rc::Rc::new(err))
    }
}

/// All the operations for working with the lazy storage.
///
/// Note that in the Tezos protocol implementation, work with this layer is
/// observable. When you call a contract with `octez-client`, you can see, for
/// instance:
///
/// ```txt
/// Updated storage: (Pair 69183 70325)
/// Updated big_maps:
///   Clear map(69179)
///   New map(70325) of type (big_map int int)
///   Set map(69183)[5] to 5
/// ```
///
/// So we try to mimic what the Tezos protocol does, and do it carefully so that
/// if we also need to log actions done at this layer, it would be close to what
/// the Tezos protocol does.
///
/// Lifetime parameter `'a` matches the lifetime of the arena used to place
/// Micheline.
pub trait LazyStorage<'a> {
    /// Get a value under the given key of the given big map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key type must match the type of key of the stored map.
    fn big_map_get(
        &mut self,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
        value_type: &Type,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError>;

    /// Check whether a value is present under the given key of the given big
    /// map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key type must match the type of key of the stored map.
    fn big_map_mem(
        &mut self,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<bool, LazyStorageError>;

    /// Add or remove a value in big map, accepts `Option` as value like in
    /// Michelson.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key and value types must match the type of key of the stored map.
    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError>;

    /// Allocate a new empty big map.
    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError>;

    /// Allocate a new big map, filling it with the contents from another map
    /// in the lazy storage.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    fn big_map_copy(
        &mut self,
        id: &BigMapId,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError>;

    /// Remove a big map.
    ///
    /// The caller is obliged to never use this big map ID in the given
    /// storage.
    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError>;

    /// Record a big map's id at its position in the dump walk (AST pre-order);
    /// the receipt builder reverses it to match L1's order. Default: no-op.
    fn record_diff_order(&mut self, _id: &BigMapId) {}

    /// Charge `milligas` for one step of the end-of-execution lazy-storage
    /// walk. Called once per node visited, with
    /// [crate::gas::tc_cost::LAZY_STORAGE_CYCLE].
    ///
    /// The walk runs on every successful execution, over the returned storage
    /// and every outgoing operation, on a value whose shape the contract
    /// chooses — so it is the only traversal in the result pipeline that must
    /// price its own work. L1 charges `Typecheck_costs.parse_instr_cycle` per
    /// node in `extract_lazy_storage_updates` for the same reason.
    ///
    /// It lives on this trait rather than as a `&mut Gas` parameter because
    /// the kernel's lazy storage *is* its gas holder, so the two cannot be
    /// borrowed separately at the call site.
    fn consume_gas(&mut self, milligas: u32) -> Result<(), LazyStorageError>;
}

/// Bulk-update the big_map. This trait exists mostly for convenience, and has a
/// blanket implementation.
pub trait LazyStorageBulkUpdate<'a>: LazyStorage<'a> {
    /// Update big map with multiple changes, generalizes
    /// [LazyStorage::big_map_update].
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key and value types must match the type of key of the stored map.
    fn big_map_bulk_update(
        &mut self,
        id: &BigMapId,
        entries_iter: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
    ) -> Result<(), LazyStorageError> {
        for (k, v) in entries_iter {
            self.big_map_update(id, k, v)?
        }
        Ok(())
    }
}

impl<'a, T: LazyStorage<'a> + ?Sized> LazyStorageBulkUpdate<'a> for T {}

/// A `big_map` representation with metadata, used in [InMemoryLazyStorage].
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct MapInfo<'a> {
    map: RedBlackTreeMap<TypedValue<'a>, TypedValue<'a>>,
    key_type: Type,
    value_type: Type,
}

impl<'a> MapInfo<'a> {
    /// Construct a new, empty, in-memory storage.
    pub fn new(
        map: RedBlackTreeMap<TypedValue<'a>, TypedValue<'a>>,
        key_type: Type,
        value_type: Type,
    ) -> Self {
        MapInfo {
            map,
            key_type,
            value_type,
        }
    }
}

/// Simple implementation for [LazyStorage].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InMemoryLazyStorage<'a> {
    next_id: BigMapId,
    next_temp_id: BigMapId,
    big_maps: BTreeMap<BigMapId, MapInfo<'a>>,
}

impl<'a> InMemoryLazyStorage<'a> {
    /// Construct a new, empty, in-memory storage.
    pub fn new() -> Self {
        InMemoryLazyStorage {
            next_id: 0.into(),
            next_temp_id: (-1).into(),
            big_maps: BTreeMap::new(),
        }
    }

    /// Construct in tzt with given big maps.
    pub(crate) fn with_big_maps(big_maps: BTreeMap<BigMapId, MapInfo<'a>>) -> Self {
        let next_id = big_maps
            .keys()
            .max()
            .map(|id| id.succ())
            .unwrap_or_else(|| 0.into());
        InMemoryLazyStorage {
            next_id,
            next_temp_id: (-1).into(),
            big_maps,
        }
    }

    fn get_next_id(&mut self) -> BigMapId {
        let id = self.next_id.clone();
        self.next_id = self.next_id.succ();
        id
    }

    fn get_next_temp_id(&mut self) -> BigMapId {
        let id = self.next_temp_id.clone();
        self.next_temp_id = self.next_temp_id.succ();
        id
    }

    pub(crate) fn big_map_get_type(
        &mut self,
        id: &BigMapId,
    ) -> Result<Option<(Type, Type)>, LazyStorageError> {
        Ok(self
            .big_maps
            .get(id)
            .map(|info| (info.key_type.clone(), info.value_type.clone())))
    }
}

impl Default for InMemoryLazyStorage<'_> {
    fn default() -> Self {
        InMemoryLazyStorage::new()
    }
}

impl<'a> InMemoryLazyStorage<'a> {
    fn access_big_map(&self, id: &BigMapId) -> Result<&MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get(id)
            .ok_or_else(|| LazyStorageError::BigMapNotFound(id.clone()))
    }

    fn access_big_map_mut(
        &mut self,
        id: &BigMapId,
    ) -> Result<&mut MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get_mut(id)
            .ok_or_else(|| LazyStorageError::BigMapNotFound(id.clone()))
    }
}

impl<'a> LazyStorage<'a> for InMemoryLazyStorage<'a> {
    /// Unmetered: this storage holds no gas counter, and none of its users —
    /// the tzt runner, `Ctx`'s default storage, unit tests — runs inside the
    /// kernel's bounded heap. The kernel's own implementation
    /// (`tezos_execution`'s `TcCtx`) is where the charge has to bite; a test
    /// that needs a metered walk wraps this in a counting storage of its own.
    fn consume_gas(&mut self, _milligas: u32) -> Result<(), LazyStorageError> {
        Ok(())
    }

    fn big_map_get(
        &mut self,
        _arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
        _value_type: &Type,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok(info.map.get(key).cloned())
    }

    fn big_map_mem(
        &mut self,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<bool, LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok(info.map.contains_key(key))
    }

    fn big_map_update(
        &mut self,
        id: &BigMapId,
        key: TypedValue<'a>,
        value: Option<TypedValue<'a>>,
    ) -> Result<(), LazyStorageError> {
        let info = self.access_big_map_mut(id)?;
        match value {
            None => {
                info.map.remove_mut(&key);
            }
            Some(value) => {
                info.map.insert_mut(key, value);
            }
        }
        Ok(())
    }

    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError> {
        let id = if temporary {
            self.get_next_temp_id()
        } else {
            self.get_next_id()
        };
        self.big_maps.insert(
            id.clone(),
            MapInfo {
                map: RedBlackTreeMap::new(),
                key_type: key_type.clone(),
                value_type: value_type.clone(),
            },
        );
        Ok(id)
    }

    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError> {
        self.big_maps.remove(id);
        Ok(())
    }

    fn big_map_copy(
        &mut self,
        copied_id: &BigMapId,
        temporary: bool,
    ) -> Result<BigMapId, LazyStorageError> {
        let id = if temporary {
            self.get_next_temp_id()
        } else {
            self.get_next_id()
        };
        let info = self.access_big_map(copied_id)?.clone();
        self.big_maps.insert(id.clone(), info);
        Ok(id)
    }
}

/// This will also implement [LazyStorage].
///
/// Worth mentioning, that this type will eventually wrap `&mut impl Runtime`
/// (or rather `&mut R (where R: Runtime)` which has to be a mere reference,
/// this `Runtime` will provide access to rollup's persistent storage. And this
/// potentially indicates some problems for us.
///
/// * We will have to put this storage to the context, meaning that in `Ctx` we
///   will have to account for lifetimes and for the `R` generic argument.
/// * One cannot put `&mut impl Runtime` twice into context because of borrowing
///   restrictions. So trying to have e.g. `RollupStorage` as one context field,
///   and some `OriginatedContractParameterGetter` that also works via `Runtime`
///   as another context field won't be possible. We may end up with the entire
///   `Ctx` implementing all the traits that our engine should be polymorphic
///   over (`LazyStorage` is probably one of them), which is ugly and
///   boilerplat-y.
/// * The caller may have a similar problem - he will have to give
///   typechecker/interpreter's context exclusive access to `Runtime`. We will
///   have to make sure it doesn't restrict the caller from e.g. writing
///   something to the storage between typechecker and interpreter calls.
#[allow(dead_code)]
struct RollupStorage();

#[cfg(test)]
mod test_big_map_operations {
    use super::*;

    fn check_get_mem<'a>(
        map: &BigMap<'a>,
        arena: &'a Arena<Micheline<'a>>,
        storage: &mut impl LazyStorage<'a>,
        key: TypedValue,
        expected_val: Option<TypedValue<'a>>,
    ) {
        assert_eq!(map.get(arena, &key, storage).unwrap(), expected_val);
        assert_eq!(map.mem(&key, storage).unwrap(), expected_val.is_some());
    }

    #[test]
    fn test_get_mem_in_memory() {
        let arena = &Arena::new();
        let storage = &mut InMemoryLazyStorage::new();
        let content = BigMapContent::InMemory(RedBlackTreeMap::from_iter([(
            TypedValue::int(1),
            TypedValue::int(1),
        )]));
        let map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };

        check_get_mem(&map, arena, storage, TypedValue::int(0), None);
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(1),
            Some(TypedValue::int(1)),
        );
    }

    #[test]
    fn test_get_mem_backed_by_storage() {
        let arena = &Arena::new();
        let storage = &mut InMemoryLazyStorage::new();
        let map_id = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(1), Some(TypedValue::int(1)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(2), Some(TypedValue::int(2)))
            .unwrap();
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id,
            overlay: RedBlackTreeMap::from_iter([
                (TypedValue::int(1), Some(TypedValue::int(-1))),
                (TypedValue::int(2), None),
                (TypedValue::int(3), Some(TypedValue::int(3))),
            ]),
        });
        let map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };

        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(0),
            Some(TypedValue::int(0)),
        );
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(1),
            Some(TypedValue::int(-1)),
        );
        check_get_mem(&map, arena, storage, TypedValue::int(2), None);
        check_get_mem(
            &map,
            arena,
            storage,
            TypedValue::int(3),
            Some(TypedValue::int(3)),
        );
    }
}

impl<'a> TypedValue<'a> {
    /// Traverses a `TypedValue` in AST pre-order and applies `f` to every big
    /// map inside it, replacing self with the updated value — or leaving it
    /// untouched, if it held no big map at all.
    fn for_each_big_map_mut<S: LazyStorage<'a> + ?Sized>(
        &mut self,
        storage: &mut S,
        f: &mut impl FnMut(&mut S, &mut BigMap<'a>) -> Result<(), LazyStorageError>,
    ) -> Result<(), LazyStorageError> {
        if let Some(updated) = self.update_big_maps(storage, f)? {
            // Sole owner of a freshly built value, so this moves, never clones.
            *self = TypedValue::unwrap_rc(updated);
        }
        Ok(())
    }

    /// Copy-on-write core of [TypedValue::for_each_big_map_mut]: applies `f`
    /// to every big map in self, in AST pre-order, and returns
    /// `Some(rebuilt)` when self held at least one big map, `None` when it
    /// held none (in which case self is left untouched).
    ///
    /// Only the spine leading to a big map is rebuilt; every other child is
    /// carried over as an `Rc::clone`. Descending with [Rc::make_mut]
    /// instead would deep-copy *every* shared child on the way down, big map
    /// below it or not — including leaves that can hold gigabytes, such as
    /// `bytes`. A value duplicated N times then cost N full copies, enough to
    /// exhaust the kernel's `wasm32` heap and abort it (L2-1831).
    ///
    /// A shared subtree is still visited once per occurrence and each
    /// occurrence gets its own rebuilt big map, so [dump_one_big_map]'s dedup
    /// sees them separately exactly as the `Rc::make_mut` walk made it. Only
    /// the *absence* of a big map is ever memoized, never a rebuild — see
    /// `big_map_free` below.
    ///
    /// Kept exhaustive (no catch-all) and in lock-step with
    /// [TypedValue::for_each_big_map], see that function's comment.
    ///
    /// # Why this is iterative
    ///
    /// The walk descends once per level of a value whose depth the contract
    /// chooses, against the kernel's 1 MiB `wasm32` stack. Michelson has no
    /// recursive types, so a value's depth is bounded by its type's depth —
    /// but this walk only ever sees values at *declared* types (the returned
    /// storage, a callee's parameter, a `CREATE_CONTRACT` storage), and
    /// `MICHELSON_MAXIMUM_TYPE_SIZE` still permits about 2 000 levels through
    /// a `list` spine, which a recursive formulation does not survive. So the
    /// walk runs on an explicit `frames`/`results` worklist, in the same shape
    /// as the iterative `Drop`, `Debug`, comparison and untyper walks
    /// (L2-1446, L2-1672); depth now costs heap, which is already bounded by
    /// the size of the value being walked.
    ///
    /// # Why this is metered
    ///
    /// Every visited node is charged
    /// [crate::gas::tc_cost::LAZY_STORAGE_CYCLE] through
    /// [LazyStorage::consume_gas], mirroring L1's
    /// `extract_lazy_storage_updates`. The charge happens *before* the
    /// `big_map_free` lookup, as L1 charges before its `has_lazy_storage`
    /// match, so a memo hit still costs a cycle — which is what makes the
    /// bound hold independently of the memo being correct.
    ///
    /// The memo alone does not bound this walk. It caches only the *absence*
    /// of a big map, so a value with one under a shared spine is still
    /// unfolded once per occurrence, and `DUP`-sharing puts `2^K` occurrences
    /// within `O(K²)` gas. Metering is what makes that run out of gas rather
    /// than out of heap or time.
    fn update_big_maps<'b, S: LazyStorage<'a> + ?Sized>(
        &'b self,
        storage: &mut S,
        f: &mut impl FnMut(&mut S, &mut BigMap<'a>) -> Result<(), LazyStorageError>,
    ) -> Result<Option<Rc<Self>>, LazyStorageError> {
        use crate::ast::Or::*;
        use TypedValue::*;

        /// One step of the walk. `Visit` descends a source node and pushes
        /// exactly one result; each `Build*` pops the results its children
        /// pushed and assembles the replacement node from them.
        enum Frame<'b, 'a> {
            Visit(&'b TypedValue<'a>),
            /// Pops 2: the left child's result, then the right one's.
            BuildPair(&'b Rc<TypedValue<'a>>, &'b Rc<TypedValue<'a>>),
            /// Pops 1.
            BuildOr {
                is_left: bool,
            },
            /// Pops 1.
            BuildOption,
            /// Pops one per element, in element order.
            BuildList(&'b MichelsonList<Rc<TypedValue<'a>>>),
            /// Pops one per entry, in ascending key order.
            BuildMap(&'b RedBlackTreeMap<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>),
            /// Pops 1: the transfer parameter.
            BuildTransferTokens(&'b OperationInfo<'a>, &'b TransferTokens<'a>),
            /// Pops 1: the originated storage.
            BuildCreateContract(&'b OperationInfo<'a>, &'b CreateContract<'a>),
            /// Pops nothing, peeks: records a shared subtree that turned out
            /// to hold no big map.
            Memoize(*const TypedValue<'a>),
        }

        // Subtrees already known to hold no big map, keyed by address.
        //
        // Sound because the walk never mutates the source and the caller keeps
        // it alive throughout, so every address here denotes one live node for
        // the whole walk and cannot be recycled under us. Sound to *act* on
        // because "holds no big map" is a property of an immutable subtree,
        // independent of which occurrence we reached it by: skipping a repeat
        // visit calls no `f`, allocates no id, records nothing in
        // `seen_source_ids` and rebuilds nothing, so it is unobservable.
        //
        // Caching a `Some` would *not* be: each occurrence of a shared big map
        // must get its own rebuilt copy, which is what `dump_one_big_map`'s
        // dedup keys off.
        //
        // Without this, a value whose in-memory DAG unfolds to a `2^K`-node
        // tree — `DUP` is an `Rc::clone`, so `DUP; NIL t; SWAP; CONS; SWAP;
        // CONS` doubles the occurrence count per level for `O(K²)` gas while
        // the *type* grows by one node — is walked as that tree (L2-1831).
        //
        // The memo turns that into `O(distinct nodes)`, but only while no big
        // map is present: with one under the shared spine every result is
        // `Some`, nothing is cacheable, and the `2^K` unfolding is back. The
        // per-node gas charge is what bounds that case, and the memo is what
        // keeps the ordinary case from paying for the bound.
        let mut big_map_free: BTreeSet<*const TypedValue<'a>> = BTreeSet::new();
        let mut frames: Vec<Frame<'b, 'a>> = vec![Frame::Visit(self)];
        let mut results: Vec<std::option::Option<Rc<TypedValue<'a>>>> = Vec::new();

        /// Queues `child`, remembering the answer if it is shared.
        ///
        /// The gate is an optimisation, not a correctness condition, and it is
        /// deliberately not an exact "can be reached twice" test — a uniquely
        /// held child of a shared parent has `strong_count == 1` and is still
        /// reached once per occurrence of that parent. It costs nothing to
        /// miss: if such a child is re-reached, some ancestor on those paths
        /// *is* shared, and the topmost one was memoized on its first visit,
        /// so the descent stops there. The exception is an ancestor holding a
        /// big map, which is never memoized — and that case is bounded by gas,
        /// not by the memo.
        ///
        /// Getting the gate wrong in the other direction is equally harmless:
        /// memoizing a node reached only once wastes one `BTreeSet` insert,
        /// since "holds no big map" does not depend on how it was reached.
        fn push_child<'b, 'a>(
            frames: &mut Vec<Frame<'b, 'a>>,
            child: &'b Rc<TypedValue<'a>>,
        ) {
            if Rc::strong_count(child) > 1 {
                frames.push(Frame::Memoize(Rc::as_ptr(child)));
            }
            frames.push(Frame::Visit(child.as_ref()));
        }

        /// Wraps a rebuilt operation back up, carrying the nonce over.
        fn rebuilt_operation<'a>(
            op: &OperationInfo<'a>,
            operation: crate::ast::Operation<'a>,
        ) -> std::option::Option<Rc<TypedValue<'a>>> {
            Some(Rc::new(TypedValue::Operation(Box::new(OperationInfo {
                operation,
                counter: op.counter,
            }))))
        }

        while let Some(frame) = frames.pop() {
            match frame {
                Frame::Visit(v) => {
                    // Charged before the memo lookup, as L1 charges before its
                    // `has_lazy_storage` match: a skipped subtree still costs
                    // its root, so the bound does not rest on the memo.
                    storage.consume_gas(tc_cost::LAZY_STORAGE_CYCLE)?;
                    if big_map_free.contains(&(v as *const TypedValue<'a>)) {
                        results.push(None);
                        continue;
                    }
                    match v {
                        Int(_) => results.push(None),
                        Nat(_) => results.push(None),
                        Mutez(_) => results.push(None),
                        Bool(_) => results.push(None),
                        Unit => results.push(None),
                        String(_) => results.push(None),
                        Bytes(_) => results.push(None),
                        Address(_) => results.push(None),
                        KeyHash(_) => results.push(None),
                        Key(_) => results.push(None),
                        Signature(_) => results.push(None),
                        ChainId(_) => results.push(None),
                        Contract(_) => results.push(None),
                        Timestamp(_) => results.push(None),
                        #[cfg(feature = "bls")]
                        Bls12381Fr(_) => results.push(None),
                        #[cfg(feature = "bls")]
                        Bls12381G1(_) => results.push(None),
                        #[cfg(feature = "bls")]
                        Bls12381G2(_) => results.push(None),
                        // Children are queued back-to-front throughout, so
                        // they are visited front-to-back: [dump_one_big_map]
                        // allocates lazy-storage ids in visit order, which
                        // makes that order consensus-observable.
                        Pair(l, r) => {
                            frames.push(Frame::BuildPair(l, r));
                            push_child(&mut frames, r);
                            push_child(&mut frames, l);
                        }
                        Or(p) => match p {
                            Left(x) => {
                                frames.push(Frame::BuildOr { is_left: true });
                                push_child(&mut frames, x);
                            }
                            Right(x) => {
                                frames.push(Frame::BuildOr { is_left: false });
                                push_child(&mut frames, x);
                            }
                        },
                        Option(None) => results.push(None),
                        Option(Some(x)) => {
                            frames.push(Frame::BuildOption);
                            push_child(&mut frames, x);
                        }
                        List(l) => {
                            frames.push(Frame::BuildList(l));
                            for x in l.iter().rev() {
                                push_child(&mut frames, x);
                            }
                        }
                        Set(_) => {
                            // Elements are comparable and so have no big maps
                            results.push(None)
                        }
                        Map(m) => {
                            // Keys are comparable (no big maps); only values
                            // are visited.
                            frames.push(Frame::BuildMap(m));
                            for (_, x) in m.iter().rev() {
                                push_child(&mut frames, x);
                            }
                        }
                        BigMap(m) => {
                            let mut m = m.clone();
                            f(storage, &mut m)?;
                            results.push(Some(Rc::new(BigMap(m))));
                        }
                        Ticket(_) => {
                            // Value is comparable, has no big map
                            results.push(None)
                        }
                        Lambda(_) => {
                            // Can contain only pushable values, thus no big maps
                            results.push(None)
                        }
                        // `param` and `storage` are `Rc`-shared like every other
                        // deep value (L2-1831), so they go through
                        // `push_child`: an operation `DUP`ed into the returned
                        // list several times shares one payload, and that is
                        // exactly the shape the memo exists for.
                        Operation(op) => match &op.operation {
                            crate::ast::Operation::TransferTokens(t) => {
                                frames.push(Frame::BuildTransferTokens(op, t));
                                push_child(&mut frames, &t.param);
                            }
                            crate::ast::Operation::SetDelegate(_) => results.push(None),
                            crate::ast::Operation::Emit(_) => {
                                // Can contain only pushable values, thus no big maps
                                results.push(None)
                            }
                            crate::ast::Operation::CreateContract(cc) => {
                                frames.push(Frame::BuildCreateContract(op, cc));
                                push_child(&mut frames, &cc.storage);
                            }
                        },
                    }
                }
                Frame::Memoize(node) => {
                    // The result just pushed is this node's, since the `Visit`
                    // this frame was queued behind pushes exactly one.
                    if matches!(results.last(), Some(None)) {
                        big_map_free.insert(node);
                    }
                }
                Frame::BuildPair(l, r) => {
                    let new_r = results.pop().expect("BuildPair: missing right");
                    let new_l = results.pop().expect("BuildPair: missing left");
                    results.push(match (new_l, new_r) {
                        (None, None) => None,
                        (new_l, new_r) => Some(Rc::new(Pair(
                            new_l.unwrap_or_else(|| l.clone()),
                            new_r.unwrap_or_else(|| r.clone()),
                        ))),
                    });
                }
                Frame::BuildOr { is_left } => {
                    let new = results.pop().expect("BuildOr: missing child");
                    results.push(
                        new.map(|x| {
                            Rc::new(Or(if is_left { Left(x) } else { Right(x) }))
                        }),
                    );
                }
                Frame::BuildOption => {
                    let new = results.pop().expect("BuildOption: missing child");
                    results.push(new.map(|x| Rc::new(Option(Some(x)))));
                }
                Frame::BuildList(orig) => {
                    let at = results.len() - orig.len();
                    if results[at..].iter().all(std::option::Option::is_none) {
                        // No big map anywhere in the list: leave it shared,
                        // without even allocating one pointer per element.
                        results.truncate(at);
                        results.push(None);
                    } else {
                        let new: Vec<Rc<TypedValue<'a>>> = orig
                            .iter()
                            .zip(results.drain(at..))
                            .map(|(x, new_x)| new_x.unwrap_or_else(|| x.clone()))
                            .collect();
                        results.push(Some(Rc::new(List(new.into()))));
                    }
                }
                Frame::BuildMap(orig) => {
                    let at = results.len() - orig.size();
                    if results[at..].iter().all(std::option::Option::is_none) {
                        results.truncate(at);
                        results.push(None);
                    } else {
                        // `insert_mut` path-copies, so `orig` and every other
                        // holder of it are left untouched; the entries that
                        // did not change stay shared with it.
                        let mut new = orig.clone();
                        for ((k, _), new_v) in orig.iter().zip(results.drain(at..)) {
                            if let Some(new_v) = new_v {
                                new.insert_mut(k.clone(), new_v);
                            }
                        }
                        results.push(Some(Rc::new(Map(new))));
                    }
                }
                Frame::BuildTransferTokens(op, t) => {
                    let new = results.pop().expect("BuildTransferTokens: missing param");
                    results.push(new.and_then(|param| {
                        rebuilt_operation(
                            op,
                            crate::ast::Operation::TransferTokens(TransferTokens {
                                // Already an `Rc`, and freshly built: stored
                                // as is, no unwrap and no copy.
                                param,
                                destination_address: t.destination_address.clone(),
                                amount: t.amount,
                            }),
                        )
                    }));
                }
                Frame::BuildCreateContract(op, cc) => {
                    let new =
                        results.pop().expect("BuildCreateContract: missing storage");
                    results.push(new.and_then(|storage| {
                        rebuilt_operation(
                            op,
                            crate::ast::Operation::CreateContract(CreateContract {
                                storage,
                                delegate: cc.delegate.clone(),
                                amount: cc.amount,
                                code: cc.code.clone(),
                                micheline_code: cc.micheline_code,
                                address: cc.address.clone(),
                            }),
                        )
                    }));
                }
            }
        }

        // The root's `Visit` pushed exactly one result and every `Build*`
        // replaces the ones it popped, so this is the whole answer.
        Ok(results.pop().flatten())
    }

    /// Read-only counterpart of [TypedValue::update_big_maps]: applies
    /// `f` to every big map inside self in AST order without mutating it. Map
    /// values are walked via [RedBlackTreeMap::values].
    ///
    /// Kept exhaustive (no catch-all) and in lock-step with
    /// [TypedValue::update_big_maps] **on which variants it visits**: this
    /// read-only walk feeds `started_with_map_ids` while the mutable walk feeds
    /// `seen_source_ids`, and [dump_big_map_updates] removes ids present in the
    /// former but not the latter. A new big-map-carrying variant must be added
    /// to both walks or a dropped big map could leak; a wildcard here would
    /// silently skip it while the mutable walk still compile-errored, so the
    /// two would diverge.
    ///
    /// The lock-step is on variant coverage only. This walk is still
    /// recursive, and it neither memoizes nor is bounded by anything, where
    /// the mutable walk is iterative and memoized. That is deliberate rather
    /// than an oversight, because the two see different values: this one runs
    /// on the storage returned by `typecheck_storage`, parsed from Micheline
    /// before the contract body runs, so it is a tree with no `Rc` sharing and
    /// no `DUP`-built DAG can reach it. Its depth is bounded by the storage
    /// type, hence by `MICHELSON_MAXIMUM_TYPE_SIZE`, and at that depth its
    /// frame fits the kernel's 1 MiB stack with room to spare — which the
    /// mutable walk's did not once it grew a rebuild, which is what forced
    /// that one onto a worklist. Converting this one too would be tidier and
    /// costs little; it is left alone here to keep the diff to the walk that
    /// had to change.
    fn for_each_big_map<'b>(&'b self, f: &mut impl FnMut(&'b BigMap<'a>)) {
        use crate::ast::Or::*;
        use TypedValue::*;
        match self {
            Int(_) => {}
            Nat(_) => {}
            Mutez(_) => {}
            Bool(_) => {}
            Unit => {}
            String(_) => {}
            Bytes(_) => {}
            Address(_) => {}
            KeyHash(_) => {}
            Key(_) => {}
            Signature(_) => {}
            ChainId(_) => {}
            Contract(_) => {}
            Timestamp(_) => {}
            #[cfg(feature = "bls")]
            Bls12381Fr(_) => {}
            #[cfg(feature = "bls")]
            Bls12381G1(_) => {}
            #[cfg(feature = "bls")]
            Bls12381G2(_) => {}
            Pair(l, r) => {
                l.for_each_big_map(f);
                r.for_each_big_map(f);
            }
            Or(p) => match p {
                Left(x) | Right(x) => x.for_each_big_map(f),
            },
            Option(p) => {
                if let Some(x) = p {
                    x.for_each_big_map(f)
                }
            }
            List(l) => l.iter().for_each(|v| v.for_each_big_map(f)),
            Set(_) => {
                // Elements are comparable and so have no big maps
            }
            Map(m) => m.values().for_each(|v| v.for_each_big_map(f)),
            BigMap(m) => f(m),
            Ticket(_) => {
                // Value is comparable, has no big map
            }
            Lambda(_) => {
                // Can contain only pushable values, thus no big maps
            }
            Operation(op) => match &op.operation {
                crate::ast::Operation::TransferTokens(t) => t.param.for_each_big_map(f),
                crate::ast::Operation::SetDelegate(_) => {}
                crate::ast::Operation::Emit(_) => {}
                crate::ast::Operation::CreateContract(cc) => {
                    cc.storage.for_each_big_map(f)
                }
            },
        }
    }

    /// Collects the big_map identifiers reachable from self.
    pub fn view_big_map_ids(&self, out: &mut Vec<BigMapId>) {
        self.for_each_big_map(&mut |m| {
            if let BigMapContent::FromId(content) = &m.content {
                out.push(content.id.clone())
            }
        });
    }
}

/// Given big map IDs before contract execution and big maps after the
/// execution, dump all the updates to the lazy storage. All the big maps
/// remaining unused will be removed from the storage.
///
/// After the call, [BigMap::overlay] field in all provided big maps is
/// guaranteed to be empty and all [BigMap::id]s are guaranteed to be non-None.
/// Also, some [BigMap::id] fields may change to avoid duplications.
pub fn dump_big_map_updates<'a>(
    storage: &mut (impl LazyStorage<'a> + ?Sized),
    started_with_map_ids: &[BigMapId],
    root: &mut TypedValue<'a>,
    temporary: bool,
) -> Result<(), LazyStorageError> {
    // Note: structurally similar to `extract_lazy_storage_diff` /
    // `extract_lazy_storage_updates` in the L1 protocol implementation
    // (`script_ir_translator.ml`). Like L1, we allocate fresh ids as we
    // walk the storage in source order; [TypedValue::for_each_big_map_mut]
    // visits big maps in AST order, so the assigned ids respect that order.
    //
    // After this call, the provided big maps satisfy:
    // * Every [BigMap]'s content is `FromId` with an empty overlay.
    // * No `BigMapId` appears twice across the slice. This guarantees
    //   that a big map in the storage cannot be updated in parallel via
    //   different `Value::BigMap` values.
    // * Big maps whose ids were in `started_with_map_ids` but are no
    //   longer referenced have been removed from the lazy storage.
    // * Best effort is made to avoid copies: when a big map can keep
    //   its existing id, its overlay is applied in place.
    //
    // The `temporary` flag indicates whether the result should live in
    // the temporary id range (e.g. the dump used at the end of contract
    // execution for big maps reachable from the produced operation
    // list, before the internal operations are interpreted).
    //
    // The general policy when visiting a big map in AST order:
    //   * `InMemory`                           -> allocate a fresh id.
    //   * `FromId(id)` with `id` temporary
    //     OR result must be temporary         -> allocate a fresh id
    //                                            via `big_map_copy`,
    //                                            so the value escapes
    //                                            the (soon-to-be
    //                                            cleared) temporary
    //                                            range.
    //   * `FromId(id)` already seen earlier
    //     in the walk                          -> dedup: copy from `id`.
    //   * `FromId(id)`, first occurrence,
    //     not temporary                        -> keep `id`, update in
    //                                            place.
    //
    // First-occurrence in-place updates are deferred to a final pass so
    // that, when a later occurrence of the same source id triggers a
    // copy, the copy reads the pre-update state of the source (matching
    // L1, which adds the in-place diff first to the accumulator so it
    // is applied last).
    //
    // `seen_source_ids` records, for every encountered `FromId` map,
    // the source id at the moment we visited it. This drives both the
    // dedup detection and the "remove gone ids" pass: a started id that
    // never appears in `seen_source_ids` is dropped storage that we
    // must clean up.
    let mut seen_source_ids: BTreeSet<BigMapId> = BTreeSet::new();
    let deferred = dump_big_map_walk(storage, root, temporary, &mut seen_source_ids)?;
    remove_unreferenced_big_maps(storage, started_with_map_ids, &seen_source_ids)?;
    // Single walk: apply the deferral now (safe after the removal pass, which
    // only touches ids absent from `seen_source_ids`).
    apply_deferred_big_map_updates(storage, deferred)?;
    Ok(())
}

/// Deferred first-occurrence in-place overlays from [dump_big_map_walk],
/// applied once every walk sharing the lazy storage has run.
pub type DeferredBigMapUpdates<'a> = Vec<(
    BigMapId,
    RedBlackTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>,
)>;

/// Apply the deferred in-place overlays returned by [dump_big_map_walk].
pub fn apply_deferred_big_map_updates<'a>(
    storage: &mut (impl LazyStorage<'a> + ?Sized),
    deferred: DeferredBigMapUpdates<'a>,
) -> Result<(), LazyStorageError> {
    for (id, overlay) in deferred {
        storage.big_map_bulk_update(
            &id,
            overlay.iter().map(|(k, v)| (k.clone(), v.clone())),
        )?;
    }
    Ok(())
}

/// Walk `finished_with_maps` in AST order, dumping each big map's updates
/// to the lazy storage and recording every encountered `FromId` source id
/// in `seen_source_ids`. See [dump_big_map_updates] for the full policy
/// driving how each big map is dumped.
///
/// This performs the dump and the in-walk dedup, but NOT the removal of
/// started ids that are no longer referenced — that decision is deferred
/// to [remove_unreferenced_big_maps]. Splitting the two lets a caller run
/// several walks against the same storage (e.g. the returned storage then
/// the emitted operation list) and copy from a source id during a later
/// walk before that id is removed. See the interpreter's contract-result
/// dump, where a big map moved from parent storage into an outgoing
/// operation must survive the storage walk's removal pass.
pub fn dump_big_map_walk<'a>(
    storage: &mut (impl LazyStorage<'a> + ?Sized),
    root: &mut TypedValue<'a>,
    temporary: bool,
    seen_source_ids: &mut BTreeSet<BigMapId>,
) -> Result<DeferredBigMapUpdates<'a>, LazyStorageError> {
    let mut deferred_in_place_updates: DeferredBigMapUpdates<'a> = Vec::new();
    // The walk hands `storage` to the visitor rather than letting it capture
    // one, so the per-big-map work can be fallible and stop the walk on the
    // first error — and so the walk itself can charge gas through the same
    // `&mut` (see [LazyStorage::consume_gas]).
    root.for_each_big_map_mut(storage, &mut |storage, map: &mut BigMap<'a>| {
        dump_one_big_map(
            map,
            storage,
            temporary,
            seen_source_ids,
            &mut deferred_in_place_updates,
        )
    })?;

    // Return the deferred updates so a multi-walk caller applies them only
    // after every walk has copied from the pre-update source (L2-1761).
    Ok(deferred_in_place_updates)
}

/// Runs the mutable walk against a throwaway unmetered storage, for tests that
/// care only about which big maps are visited and in what order. Tests that
/// need the gas charge to bite supply a metered storage of their own.
#[cfg(test)]
fn walk_big_maps<'a>(root: &mut TypedValue<'a>, f: &mut impl FnMut(&mut BigMap<'a>)) {
    let mut storage = InMemoryLazyStorage::new();
    root.for_each_big_map_mut(&mut storage, &mut |_, map| {
        f(map);
        Ok(())
    })
    .expect("InMemoryLazyStorage is unmetered and this visitor cannot fail")
}

/// Dump a single big map to the lazy storage, threading the dedup state across
/// maps. Split from [dump_big_map_walk] to keep the per-map work readable.
fn dump_one_big_map<'a>(
    map: &mut BigMap<'a>,
    storage: &mut (impl LazyStorage<'a> + ?Sized),
    temporary: bool,
    seen_source_ids: &mut BTreeSet<BigMapId>,
    deferred_in_place_updates: &mut DeferredBigMapUpdates<'a>,
) -> Result<(), LazyStorageError> {
    match map.content {
        BigMapContent::FromId(ref mut m) => {
            let source_id = m.id.clone();
            let already_seen = !seen_source_ids.insert(source_id);
            let must_copy = temporary || m.id.is_temporary() || already_seen;
            if must_copy {
                let new_id = storage.big_map_copy(&m.id, temporary)?;
                let overlay = mem::take(&mut m.overlay);
                storage.big_map_bulk_update(
                    &new_id,
                    overlay.iter().map(|(k, v)| (k.clone(), v.clone())),
                )?;
                m.id = new_id;
            } else {
                // First occurrence of a permanent id with a
                // permanent result: keep the id and defer the
                // overlay write until later occurrences (if any)
                // have completed their copies.
                deferred_in_place_updates.push((m.id.clone(), mem::take(&mut m.overlay)));
            }
            storage.record_diff_order(&m.id);
        }
        BigMapContent::InMemory(ref mut m) => {
            // The entire big map is still in memory. Allocate a
            // fresh id and write the data straight away — there is
            // no source for any later occurrence to read from, so
            // no need to defer.
            let id = storage.big_map_new(&map.key_type, &map.value_type, temporary)?;
            storage.record_diff_order(&id);
            let in_memory = mem::take(m);
            storage.big_map_bulk_update(
                &id,
                in_memory
                    .iter()
                    .map(|(key, value)| (key.clone(), Some(value.clone()))),
            )?;
            map.content = BigMapContent::FromId(BigMapFromId {
                id,
                overlay: RedBlackTreeMap::new(),
            });
        }
    }
    Ok(())
}

/// Remove every id in `started_with_map_ids` that is absent from
/// `seen_source_ids` — i.e. storage that started the execution but is no
/// longer reachable from any dumped big map (neither kept in place nor
/// used as a copy source).
///
/// Call this only after every relevant [dump_big_map_walk] has run, so an
/// id still reachable from a later walk's payload has been copied before
/// it is removed. `seen_source_ids` must therefore reflect reachability
/// from the result(s) that own those started ids — for a contract result,
/// the returned storage, since a big map moved out into an operation is
/// copied into the temporary range and no longer keeps the started id
/// alive.
pub fn remove_unreferenced_big_maps<'a>(
    storage: &mut (impl LazyStorage<'a> + ?Sized),
    started_with_map_ids: &[BigMapId],
    seen_source_ids: &BTreeSet<BigMapId>,
) -> Result<(), LazyStorageError> {
    for map_id in started_with_map_ids {
        if !seen_source_ids.contains(map_id) {
            storage.big_map_remove(map_id)?
        }
    }
    Ok(())
}

#[cfg(test)]
mod test_big_map_to_storage_update {
    use crate::ast::Type;

    use super::*;

    #[track_caller]
    fn check_is_dumped_map(map: BigMap, id: BigMapId) {
        match map.content {
            BigMapContent::InMemory(_) => panic!("Big map has not been dumped"),
            BigMapContent::FromId(map) => {
                assert_eq!((map.id, map.overlay), (id, RedBlackTreeMap::new()))
            }
        };
    }

    /// Run [dump_big_map_updates] over a list of big maps by wrapping them in a
    /// right-nested comb of pairs, visited in AST order by for_each_big_map_mut,
    /// and unwinding the mutated maps back out in the same order.
    fn dump_maps<'a>(
        storage: &mut (impl LazyStorage<'a> + ?Sized),
        started: &[BigMapId],
        maps: Vec<BigMap<'a>>,
        temporary: bool,
    ) -> Vec<BigMap<'a>> {
        let mut iter = maps.into_iter().rev();
        let mut root = TypedValue::BigMap(iter.next().expect("at least one map"));
        for m in iter {
            root = TypedValue::new_pair(TypedValue::BigMap(m), root);
        }
        dump_big_map_updates(storage, started, &mut root, temporary).unwrap();
        // Walk the comb in the same AST order it was built and take each mutated
        // big map back out. A by-value `Pair(l, r)` destructure is rejected
        // because `TypedValue` implements `Drop` (L2-1672), so we visit through
        // [TypedValue::for_each_big_map_mut] and swap a placeholder in instead.
        let mut out = Vec::new();
        walk_big_maps(&mut root, &mut |m| {
            out.push(mem::replace(m, BigMap::empty(Type::Unit, Type::Unit)));
        });
        out
    }

    #[test]
    fn test_map_from_memory() {
        let storage = &mut InMemoryLazyStorage::new();
        let content = BigMapContent::InMemory(RedBlackTreeMap::from_iter([
            (TypedValue::int(1), TypedValue::int(1)),
            (TypedValue::int(2), TypedValue::int(2)),
        ]));
        let map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let map = dump_maps(storage, &[], vec![map], false).pop().unwrap();

        check_is_dumped_map(map, 0.into());
        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                0.into(),
                MapInfo {
                    map: RedBlackTreeMap::from_iter([
                        (TypedValue::int(1), TypedValue::int(1)),
                        (TypedValue::int(2), TypedValue::int(2))
                    ]),
                    key_type: Type::Int,
                    value_type: Type::Int
                }
            )])
        )
    }

    #[test]
    fn test_map_updates_to_storage() {
        let storage = &mut InMemoryLazyStorage::new();
        let map_id = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(1), Some(TypedValue::int(1)))
            .unwrap();
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id,
            overlay: RedBlackTreeMap::from_iter([
                (TypedValue::int(0), None),
                (TypedValue::int(1), Some(TypedValue::int(5))),
                (TypedValue::int(2), None),
                (TypedValue::int(3), Some(TypedValue::int(3))),
            ]),
        });
        let map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let map = dump_maps(storage, &[], vec![map], false).pop().unwrap();

        check_is_dumped_map(map, 0.into());
        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                0.into(),
                MapInfo {
                    map: RedBlackTreeMap::from_iter([
                        (TypedValue::int(1), TypedValue::int(5)),
                        (TypedValue::int(3), TypedValue::int(3))
                    ]),
                    key_type: Type::Int,
                    value_type: Type::Int
                }
            )])
        )
    }

    #[test]
    fn test_duplicate_ids() {
        let storage = &mut InMemoryLazyStorage::new();
        let map_id1 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        let map_id2 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id1.clone(),
            overlay: RedBlackTreeMap::from_iter([(
                TypedValue::int(11),
                Some(TypedValue::int(11)),
            )]),
        });
        let map1_1 = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id1,
            overlay: RedBlackTreeMap::from_iter([(
                TypedValue::int(12),
                Some(TypedValue::int(12)),
            )]),
        });
        let map1_2 = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id2,
            overlay: RedBlackTreeMap::from_iter([(
                TypedValue::int(2),
                Some(TypedValue::int(2)),
            )]),
        });
        let map2 = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let mut out =
            dump_maps(storage, &[], vec![map1_1, map1_2, map2], false).into_iter();
        let map1_1 = out.next().unwrap();
        let map1_2 = out.next().unwrap();
        let map2 = out.next().unwrap();

        check_is_dumped_map(map1_1, 0.into());
        check_is_dumped_map(map1_2, 2.into()); // newly created map
        check_is_dumped_map(map2, 1.into());

        assert_eq!(
            storage.big_maps,
            BTreeMap::from([
                (
                    0.into(),
                    MapInfo {
                        map: RedBlackTreeMap::from_iter([(
                            TypedValue::int(11),
                            TypedValue::int(11)
                        )]),
                        key_type: Type::Int,
                        value_type: Type::Int
                    }
                ),
                (
                    1.into(),
                    MapInfo {
                        map: RedBlackTreeMap::from_iter([(
                            TypedValue::int(2),
                            TypedValue::int(2)
                        )]),
                        key_type: Type::Int,
                        value_type: Type::Int
                    }
                ),
                (
                    2.into(),
                    MapInfo {
                        map: RedBlackTreeMap::from_iter([(
                            TypedValue::int(12),
                            TypedValue::int(12)
                        )]),
                        key_type: Type::Int,
                        value_type: Type::Int
                    }
                )
            ])
        );
    }

    #[test]
    fn test_remove_ids() {
        let storage = &mut InMemoryLazyStorage::new();
        let map_id1 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id1, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let map_id2 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&map_id2, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let content = BigMapContent::FromId(BigMapFromId {
            id: map_id1.clone(),
            overlay: RedBlackTreeMap::from_iter([(
                TypedValue::int(1),
                Some(TypedValue::int(1)),
            )]),
        });
        let map1 = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::Int,
        };
        dump_maps(storage, &[map_id1, map_id2], vec![map1], false);

        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                0.into(),
                MapInfo {
                    map: RedBlackTreeMap::from_iter([
                        (TypedValue::int(0), TypedValue::int(0)),
                        (TypedValue::int(1), TypedValue::int(1))
                    ]),
                    key_type: Type::Int,
                    value_type: Type::Int
                }
            )])
        );
    }

    /// Regression for the "internal origination assigns bigmap ids in
    /// reverse AST order" bug. When the slice contains `FromId` maps
    /// with temporary ids that were allocated in AST order
    /// (`-1, -2, -3`) — as happens when a parent contract dumps the
    /// storage of a `CREATE_CONTRACT` operation with `temporary=true`
    /// — the subsequent permanent dump must reassign ids in the same
    /// AST order: leftmost map gets the lowest fresh permanent id.
    #[test]
    fn test_temporary_from_id_ast_order() {
        let storage = &mut InMemoryLazyStorage::new();
        // Seed three empty temporary maps through the real allocator,
        // mirroring the state left behind by a prior
        // `dump_big_map_updates(..., temporary=true)` (e.g. the
        // temporary dump applied to a `CREATE_CONTRACT` operation's
        // storage at the end of the parent's execution).
        let temp_ids: Vec<BigMapId> = (0..3)
            .map(|_| storage.big_map_new(&Type::Int, &Type::Int, true).unwrap())
            .collect();
        assert_eq!(
            temp_ids,
            vec![(-1).into(), (-2).into(), (-3).into()],
            "sanity: temp ids allocated in AST order"
        );
        let make_map = |id: BigMapId| BigMap {
            content: BigMapContent::FromId(BigMapFromId {
                id,
                overlay: RedBlackTreeMap::new(),
            }),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let map_ast_first = make_map(temp_ids[0].clone());
        let map_ast_second = make_map(temp_ids[1].clone());
        let map_ast_third = make_map(temp_ids[2].clone());
        let mut out = dump_maps(
            storage,
            &[],
            vec![map_ast_first, map_ast_second, map_ast_third],
            false,
        )
        .into_iter();
        // Fresh permanent ids start at 0 (no prior permanent
        // allocations on this storage). The leftmost AST occurrence
        // must get the lowest fresh id.
        check_is_dumped_map(out.next().unwrap(), 0.into());
        check_is_dumped_map(out.next().unwrap(), 1.into());
        check_is_dumped_map(out.next().unwrap(), 2.into());
    }
}

#[cfg(test)]
mod review_verification {
    //! Tests added while reviewing the persistent-collections change
    //! (L2-1649): they pin the two properties the review leaned on — that
    //! the `rpds` trees iterate in the same order as the `std` maps they
    //! replaced, and that the read-only big-map walk reaches every position
    //! the mutable walk does.
    use super::*;
    use crate::ast::{CreateContract, MichelsonList, Or, TransferTokens, Type};
    use rpds::RedBlackTreeSet;

    /// A `big_map` leaf carrying `id` and nothing else — the shape the walk
    /// keys off, with no overlay to complicate what a rebuild has to preserve.
    fn leaf_big_map(id: i64) -> TypedValue<'static> {
        TypedValue::BigMap(BigMap {
            content: BigMapContent::FromId(BigMapFromId {
                id: id.into(),
                overlay: RedBlackTreeMap::new(),
            }),
            key_type: Type::Int,
            value_type: Type::Int,
        })
    }

    /// Consensus-critical invariant behind the whole migration: `rpds`
    /// `RedBlackTreeMap`/`RedBlackTreeSet` must iterate in ascending key
    /// order, byte-for-byte matching the `BTreeMap`/`BTreeSet` they replaced.
    /// PACK/serialization, `ITER`, `MAP` and the big-map durable layout all
    /// depend on this; if a future `rpds` bump changed it, this fails loudly.
    #[test]
    fn rpds_collections_iterate_in_btreemap_order() {
        // Deliberately unsorted, with negatives, a zero, and a duplicate.
        let ks = [
            TypedValue::int(5),
            TypedValue::int(-3),
            TypedValue::int(0),
            TypedValue::int(100),
            TypedValue::int(-1),
            TypedValue::int(2),
            TypedValue::int(-3),
        ];
        let mut rb = RedBlackTreeMap::new();
        let mut bt = BTreeMap::new();
        let mut rs = RedBlackTreeSet::new();
        let mut bs = BTreeSet::new();
        for (i, k) in ks.iter().enumerate() {
            rb.insert_mut(k.clone(), i);
            bt.insert(k.clone(), i);
            rs.insert_mut(k.clone());
            bs.insert(k.clone());
        }
        assert!(
            rb.iter().map(|(k, _)| k).eq(bt.keys()),
            "RedBlackTreeMap key order diverges from BTreeMap"
        );
        assert!(
            rs.iter().eq(bs.iter()),
            "RedBlackTreeSet order diverges from BTreeSet"
        );
        // Strictly ascending, independent of insertion order.
        let mut sorted: Vec<TypedValue> = ks.to_vec();
        sorted.sort();
        sorted.dedup();
        assert_eq!(rb.keys().cloned().collect::<Vec<_>>(), sorted);
    }

    /// A persisted (`FromId`) big map placed in every value position the
    /// mutable dump walk descends into must also be reached by the read-only
    /// [TypedValue::view_big_map_ids] walk — otherwise `started_with_map_ids`
    /// under-reports and [dump_big_map_updates]'s removal pass could leak a
    /// dropped big map. Guards the two walks against drifting apart.
    #[test]
    fn view_big_map_ids_covers_all_container_positions() {
        let bm = |id: i64| {
            TypedValue::BigMap(BigMap {
                content: BigMapContent::FromId(BigMapFromId {
                    id: id.into(),
                    overlay: RedBlackTreeMap::new(),
                }),
                key_type: Type::Int,
                value_type: Type::Int,
            })
        };
        // Nest a distinct FromId big map in: Pair (both sides), Or, Option,
        // List element, and Map value.
        let value = TypedValue::new_pair(
            TypedValue::new_pair(bm(0), bm(1)),
            TypedValue::new_pair(
                TypedValue::Or(Or::Left(Rc::new(bm(2)))),
                TypedValue::new_pair(
                    TypedValue::Option(Some(Rc::new(bm(3)))),
                    TypedValue::new_pair(
                        TypedValue::List(MichelsonList::from(vec![Rc::new(bm(4))])),
                        TypedValue::Map(RedBlackTreeMap::from_iter([(
                            Rc::new(TypedValue::int(0)),
                            Rc::new(bm(5)),
                        )])),
                    ),
                ),
            ),
        );
        let mut ids = vec![];
        value.view_big_map_ids(&mut ids);
        ids.sort();
        assert_eq!(
            ids,
            (0..=5).map(BigMapId::from).collect::<Vec<_>>(),
            "view_big_map_ids missed a big map in some container position"
        );
    }

    /// Sharing an operation's payload behind an `Rc` only helps if the walk
    /// then leaves it shared. Reaching the payload through
    /// `Rc::make_mut(&mut t.param)` would not: `make_mut` copies whenever the
    /// payload is shared, it runs before anything inspects the payload's type,
    /// and it writes the clone back into the field — and since
    /// `TypedValue::Bytes` holds its buffer inline, that "shallow" node clone
    /// *is* the whole payload for exactly the flat values this targets. The
    /// copy would be relocated from the instruction to the walk, not removed
    /// (L2-1836).
    ///
    /// The copy-on-write walk returns `None` for a payload with no big map
    /// under it, so the payload is never touched, however many operations
    /// share it — and `push_child` means it is walked once rather than once
    /// per operation.
    #[test]
    fn walk_leaves_shared_operation_payloads_shared() {
        let payload = Rc::new(TypedValue::Bytes(vec![0xab; 64]));
        let transfer = |param: Rc<TypedValue<'static>>, counter| {
            TypedValue::new_operation(
                crate::ast::Operation::TransferTokens(TransferTokens {
                    param,
                    destination_address:
                        crate::ast::michelson_address::Address::try_from(
                            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
                        )
                        .unwrap(),
                    amount: 0,
                }),
                counter,
            )
        };
        // One payload, three operations — the `DUP`ed `list operation` shape.
        let mut root = TypedValue::List(MichelsonList::from(vec![
            Rc::new(transfer(payload.clone(), 0)),
            Rc::new(transfer(payload.clone(), 1)),
            Rc::new(transfer(payload.clone(), 2)),
        ]));
        let shared_before = Rc::strong_count(&payload);

        walk_big_maps(&mut root, &mut |_| {
            panic!("the value holds no big map, so `f` must not be called")
        });

        assert_eq!(
            Rc::strong_count(&payload),
            shared_before,
            "the walk copied a shared operation payload"
        );
        let TypedValue::List(operations) = &root else {
            panic!("root is no longer a list")
        };
        for operation in operations.iter() {
            let TypedValue::Operation(info) = &**operation else {
                panic!("element is no longer an operation")
            };
            let crate::ast::Operation::TransferTokens(t) = &info.operation else {
                panic!("element is no longer a transfer")
            };
            assert!(
                Rc::ptr_eq(&t.param, &payload),
                "an operation's payload was replaced by a copy"
            );
        }
    }

    /// The mutable walk must not unshare a subtree that holds no big map:
    /// `DUP` is an `Rc::clone`, so a duplicated multi-gigabyte value would
    /// otherwise be deep-copied once per occurrence and exhaust the kernel's
    /// `wasm32` heap (L2-1831). Asserted on pointer identity rather than
    /// value equality, which a deep copy would still satisfy.
    #[test]
    fn walk_leaves_big_map_free_subtrees_shared() {
        let shared = Rc::new(TypedValue::Bytes(vec![0xab; 64]));
        // The value shape of the L2-1831 reproduction: one operation `DUP`ed
        // into a three-element `list operation`, all three elements pointing
        // at a single allocation.
        let mut root = TypedValue::List(MichelsonList::from(vec![
            shared.clone(),
            shared.clone(),
            shared.clone(),
        ]));
        let strong_before = Rc::strong_count(&shared);

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |_| visited += 1);

        assert_eq!(visited, 0, "no big map to visit");
        assert_eq!(
            Rc::strong_count(&shared),
            strong_before,
            "the walk copied a big-map-free subtree"
        );
        let TypedValue::List(elements) = &root else {
            panic!("root is no longer a list")
        };
        for element in elements.iter() {
            assert!(Rc::ptr_eq(element, &shared));
        }
    }

    /// Mutable-walk counterpart of
    /// [view_big_map_ids_covers_all_container_positions]: a big map sitting in
    /// any container position must be visited by `for_each_big_map_mut`, and
    /// in AST pre-order. [dump_one_big_map] allocates lazy-storage ids in
    /// visit order, so this order is consensus-observable — a container arm
    /// that visits out of order, or not at all, changes execution results.
    ///
    /// `test_big_map_to_storage_update::dump_maps` only ever builds a
    /// right-nested comb of `Pair`s, so without this the `Or` / `Option` /
    /// `List` / `Map` / `Operation` arms have no ordering coverage.
    #[test]
    fn mutable_walk_visits_every_container_position_in_order() {
        let bm = leaf_big_map;
        let operation_carrying = |inner: TypedValue<'static>| {
            TypedValue::new_operation(
                crate::ast::Operation::TransferTokens(TransferTokens {
                    param: Rc::new(inner),
                    destination_address:
                        crate::ast::michelson_address::Address::try_from(
                            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
                        )
                        .unwrap(),
                    // Nonzero, so a rebuild that dropped either would show up
                    // here rather than matching the zero default. What they
                    // survive is asserted in `walk_preserves_operation_fields`.
                    amount: 1_000,
                }),
                42,
            )
        };
        // Distinct ids, laid out so that AST pre-order is 0,1,2,3,4,5,6.
        let mut value = TypedValue::new_pair(
            TypedValue::new_pair(bm(0), bm(1)),
            TypedValue::new_pair(
                TypedValue::Or(Or::Left(Rc::new(bm(2)))),
                TypedValue::new_pair(
                    TypedValue::Option(Some(Rc::new(bm(3)))),
                    TypedValue::new_pair(
                        TypedValue::List(MichelsonList::from(vec![
                            Rc::new(bm(4)),
                            Rc::new(operation_carrying(bm(5))),
                        ])),
                        TypedValue::Map(RedBlackTreeMap::from_iter([(
                            Rc::new(TypedValue::int(0)),
                            Rc::new(bm(6)),
                        )])),
                    ),
                ),
            ),
        );

        let mut visit_order = vec![];
        walk_big_maps(&mut value, &mut |map| {
            if let BigMapContent::FromId(ref m) = map.content {
                visit_order.push(m.id.clone())
            }
        });
        assert_eq!(
            visit_order,
            (0..=6).map(BigMapId::from).collect::<Vec<_>>(),
            "the mutable walk missed a container position or visited out of order"
        );
        // The rebuilt value must still expose the same big maps in the same
        // order, i.e. the rebuild put every child back where it belongs.
        let mut ids_after = vec![];
        value.view_big_map_ids(&mut ids_after);
        assert_eq!(ids_after, (0..=6).map(BigMapId::from).collect::<Vec<_>>());
    }

    /// The walk descends once per level of a value whose depth the contract
    /// chooses, against the kernel's 1 MiB stack. It runs on an explicit
    /// worklist rather than the call stack, so depth costs heap — already
    /// bounded by the size of the value being walked — rather than stack, and
    /// `DEPTH` matches `drop_safety`'s to pin that.
    ///
    /// A recursive formulation did not survive the depths reachable here: a
    /// value only ever reaches this walk at a *declared* type (the returned
    /// storage, a callee's parameter, a `CREATE_CONTRACT` storage), and
    /// `MICHELSON_MAXIMUM_TYPE_SIZE` still permits about 2 000 levels through
    /// a `list` spine and about 1 000 through a `map` — well past what a
    /// frame of a few hundred bytes per level affords.
    ///
    /// Both paths are exercised for each container: a big-map-free spine,
    /// which returns `None` the whole way up and rebuilds nothing, and the
    /// same spine with a big map at its base, so every level is rebuilt.
    #[test]
    fn walk_survives_a_deep_spine_on_the_kernel_stack() {
        // Matches `drop_safety`: nothing about the walk is depth-bounded now.
        const DEPTH: usize = 100_000;

        fn deep_pair(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| TypedValue::new_pair(TypedValue::Unit, acc))
        }
        fn deep_list(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| {
                TypedValue::List(MichelsonList::from(vec![Rc::new(acc)]))
            })
        }
        fn deep_map(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| {
                TypedValue::Map(RedBlackTreeMap::from_iter([(
                    Rc::new(TypedValue::int(0)),
                    Rc::new(acc),
                )]))
            })
        }

        for build in [deep_pair, deep_list, deep_map] {
            for with_big_map in [false, true] {
                std::thread::Builder::new()
                    // The WASM kernel stack budget.
                    .stack_size(1024 * 1024)
                    .spawn(move || {
                        // Built inside the worker: `TypedValue` is `Rc`-backed
                        // and so not `Send`.
                        let base = if with_big_map {
                            leaf_big_map(0)
                        } else {
                            TypedValue::Unit
                        };
                        let mut value = build(base, DEPTH);
                        let mut visited = 0;
                        walk_big_maps(&mut value, &mut |_| visited += 1);
                        assert_eq!(visited, usize::from(with_big_map));
                    })
                    .unwrap()
                    .join()
                    .expect("worker thread completes without stack overflow");
            }
        }
    }

    /// [TypedValue::for_each_big_map] is still recursive, where the mutable
    /// walk is not, so its depth budget is a live constraint rather than a
    /// formality — and nothing pinned it.
    ///
    /// What makes it safe is that the value it walks is bounded twice over.
    /// Michelson has no recursive types, so a value is never deeper than its
    /// type; and this walk only ever sees the *committed* storage, parsed at a
    /// declared type, so [MICHELSON_MAXIMUM_TYPE_SIZE] applies to it — unlike
    /// instruction-synthesised types, which escape that cap. The depths below
    /// are what the cap actually permits, per shape: `list t` costs one type
    /// node per level, `pair a b` and `map k v` cost two. So these are the
    /// deepest values that can reach this function at all.
    ///
    /// The margin is real but not generous, which is the point of deriving the
    /// depths rather than picking them: `map` is the most expensive shape, and
    /// at twice its reachable depth this test overflows. Safety here is
    /// arithmetic — cap times frame size against 1 MiB — where
    /// [TypedValue::update_big_maps] is depth-independent by construction. A
    /// few extra locals in this function would eat the margin silently, and
    /// this is what would catch that.
    #[test]
    fn read_only_walk_survives_the_deepest_reachable_spine() {
        use crate::typechecker::MICHELSON_MAXIMUM_TYPE_SIZE as MAX_TY;

        /// `list t` is one type node per level, so it nests deepest.
        const LIST_DEPTH: usize = MAX_TY - 1;
        /// `pair a b` and `map k v` are two nodes per level.
        const PAIR_DEPTH: usize = (MAX_TY - 1) / 2;
        const MAP_DEPTH: usize = PAIR_DEPTH;

        fn deep_pair(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| TypedValue::new_pair(TypedValue::Unit, acc))
        }
        fn deep_list(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| {
                TypedValue::List(MichelsonList::from(vec![Rc::new(acc)]))
            })
        }
        fn deep_map(base: TypedValue<'static>, depth: usize) -> TypedValue<'static> {
            (0..depth).fold(base, |acc, _| {
                TypedValue::Map(RedBlackTreeMap::from_iter([(
                    Rc::new(TypedValue::int(0)),
                    Rc::new(acc),
                )]))
            })
        }

        for (build, depth) in [
            (
                deep_pair as fn(TypedValue<'static>, usize) -> TypedValue<'static>,
                PAIR_DEPTH,
            ),
            (deep_list, LIST_DEPTH),
            (deep_map, MAP_DEPTH),
        ] {
            std::thread::Builder::new()
                // The WASM kernel stack budget.
                .stack_size(1024 * 1024)
                .spawn(move || {
                    // Built inside the worker: `TypedValue` is `Rc`-backed and
                    // so not `Send`. A big map at the base, so the walk runs
                    // its `f` at the deepest point rather than stopping short.
                    let value = build(leaf_big_map(0), depth);
                    let mut ids = vec![];
                    value.view_big_map_ids(&mut ids);
                    assert_eq!(ids, vec![BigMapId::from(0)]);
                })
                .unwrap()
                .join()
                .expect("worker thread completes without stack overflow");
        }
    }

    /// The flip side of [walk_leaves_big_map_free_subtrees_shared]: a big map
    /// under a shared spine is still reached, still visited once per
    /// occurrence, and the rebuilt value carries `f`'s writes — while the
    /// big-map-free sibling stays shared.
    #[test]
    fn walk_updates_big_maps_under_a_shared_spine() {
        let big_map = leaf_big_map;
        let shared_payload = Rc::new(TypedValue::Bytes(vec![0xcd; 64]));
        let shared_spine = Rc::new(TypedValue::new_pair_rc(
            Rc::new(big_map(7)),
            shared_payload.clone(),
        ));
        let mut root = TypedValue::List(MichelsonList::from(vec![
            shared_spine.clone(),
            shared_spine.clone(),
        ]));

        // Renumber every visited big map, so each visit is observable.
        let mut visited = 0;
        walk_big_maps(&mut root, &mut |map| {
            visited += 1;
            if let BigMapContent::FromId(ref mut m) = map.content {
                m.id = BigMapId::from(100 + visited);
            }
        });

        assert_eq!(visited, 2, "each occurrence must be visited on its own");
        // Only the pair spine is rebuilt; its big-map-free half is carried
        // over as an `Rc::clone`, never copied.
        let TypedValue::List(elements) = &root else {
            panic!("root is no longer a list")
        };
        for element in elements.iter() {
            let TypedValue::Pair(_, payload) = &**element else {
                panic!("element is no longer a pair")
            };
            assert!(
                Rc::ptr_eq(payload, &shared_payload),
                "the big-map-free half of the pair was copied"
            );
        }
        let mut ids = vec![];
        root.view_big_map_ids(&mut ids);
        assert_eq!(ids, vec![BigMapId::from(101), BigMapId::from(102)]);
        // The original is untouched: rebuilding is copy-on-write.
        let mut original_ids = vec![];
        shared_spine.view_big_map_ids(&mut original_ids);
        assert_eq!(original_ids, vec![BigMapId::from(7)]);
    }

    /// Builds `LEVELS` nested two-element lists whose elements are, at every
    /// level, the *same* allocation — the in-memory shape of
    /// `DUP; NIL t; SWAP; CONS; SWAP; CONS` repeated. `LEVELS` nodes, `2^LEVELS`
    /// occurrences.
    fn shared_dag(leaf: TypedValue<'static>, levels: usize) -> TypedValue<'static> {
        let mut node = Rc::new(leaf);
        for _ in 0..levels {
            node = Rc::new(TypedValue::List(MichelsonList::from(vec![
                node.clone(),
                node,
            ])));
        }
        TypedValue::unwrap_rc(node)
    }

    /// A value's in-memory DAG can unfold to a tree exponentially larger than
    /// itself, and the walk must not pay for the unfolding. `DUP` is an
    /// `Rc::clone`, so the shape above doubles the occurrence count per level
    /// while the *type* grows by a single node — putting `2^K` occurrences
    /// within `O(K²)` gas, since `list t` costs one type node per level and so
    /// stays far under `MICHELSON_MAXIMUM_TYPE_SIZE`.
    ///
    /// Nothing else bounds it: the walk is charged no gas, and it no longer
    /// allocates per visit, so it cannot exhaust the heap the way the
    /// `Rc::make_mut` walk did. Only the big-map-free memo does.
    ///
    /// **Finishing at all is the assertion**, which is why the walk runs on a
    /// worker under a deadline rather than inline. At `LEVELS` 40 the DAG
    /// unfolds to `2^40` ≈ 10^12 occurrences, so a walk that explored each of
    /// them does not finish — measured at ~8 hours on release, and worse in
    /// debug. Nothing else here can express the property: the value holds no
    /// big map, so `f` is never called, nothing is rebuilt and nothing is
    /// allocated. Re-exploration has no observable effect except time.
    ///
    /// Without the deadline this test would *hang* instead of failing, and
    /// since `libtest` has no per-test timeout and the suite runs with
    /// `RUST_TEST_THREADS=1`, that means the whole binary stalls until CI kills
    /// the job an hour later — reporting no result for this test or any test
    /// after it. A regression would look like flaky infrastructure rather than
    /// like this function.
    ///
    /// The deadline is wall-clock, which is normally a poor thing to assert
    /// on. It is sound here only because the two outcomes are about six orders
    /// of magnitude apart: microseconds when memoized, hours when not. The
    /// deterministic form needs the walk to price its own steps, so it arrives
    /// with the per-node gas charge, where an unmemoized walk exhausts its
    /// budget immediately and the assertion becomes a visit count.
    ///
    /// Note in particular that asserting the two list elements still point at
    /// one allocation would *not* say anything about re-exploration — that is
    /// the copy-on-write property, and
    /// [walk_leaves_big_map_free_subtrees_shared] already covers it.
    #[test]
    fn walk_terminates_on_an_exponentially_shared_subtree() {
        const LEVELS: usize = 40;
        /// Far above the memoized cost and far below the unmemoized one, so
        /// neither a slow runner nor a warm cache can move the verdict.
        const BUDGET: std::time::Duration = std::time::Duration::from_secs(30);

        let (done, finished) = std::sync::mpsc::channel();
        std::thread::Builder::new()
            // The WASM kernel stack budget, as elsewhere in this module.
            .stack_size(1024 * 1024)
            .spawn(move || {
                // Built inside the worker: `TypedValue` is `Rc`-backed and so
                // not `Send`. Only the count crosses the channel.
                let mut root = shared_dag(TypedValue::Unit, LEVELS);
                let mut visited = 0;
                walk_big_maps(&mut root, &mut |_| visited += 1);
                let _ = done.send(visited);
            })
            .unwrap();

        // On timeout the worker is left running — a thread cannot be killed —
        // so it burns one core until the binary exits. That is the price of
        // failing at all, and it beats stalling the suite.
        match finished.recv_timeout(BUDGET) {
            Ok(visited) => assert_eq!(visited, 0, "no big map to visit"),
            Err(_) => panic!(
                "the walk did not finish within {BUDGET:?}; at {LEVELS} levels \
                 the value's in-memory DAG unfolds to 2^{LEVELS} occurrences, \
                 so it is exploring shared subtrees once per path instead of \
                 once — the big-map-free memo has regressed"
            ),
        }
    }

    /// The flip side of [walk_terminates_on_an_exponentially_shared_subtree]:
    /// the memo
    /// records only the *absence* of a big map, so a shared subtree that holds
    /// one is still visited once per occurrence. That multiplicity is
    /// consensus-observable — [dump_one_big_map] gives each occurrence its own
    /// id and keys its dedup off having seen the source before.
    ///
    /// Deliberately shallow, and deliberately unmetered: this one really is
    /// exponential by design, and what stops it in the kernel is gas — see
    /// [metered_walk_runs_out_of_gas_on_an_exponentially_shared_big_map].
    #[test]
    fn walk_still_visits_every_occurrence_of_a_shared_big_map() {
        const LEVELS: u32 = 4;

        let mut root = shared_dag(leaf_big_map(0), LEVELS as usize);

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |_| visited += 1);

        assert_eq!(
            visited,
            2usize.pow(LEVELS),
            "the memo skipped an occurrence of a shared big map"
        );
    }

    /// [InMemoryLazyStorage] with a gas budget, so a test can watch the walk
    /// exhaust one. The in-memory storage is unmetered by design, and the
    /// kernel's own metered implementation lives in another crate.
    struct MeteredStorage<'a> {
        inner: InMemoryLazyStorage<'a>,
        gas: crate::gas::Gas,
    }

    impl<'a> LazyStorage<'a> for MeteredStorage<'a> {
        fn consume_gas(&mut self, milligas: u32) -> Result<(), LazyStorageError> {
            Ok(self.gas.consume(milligas)?)
        }

        fn big_map_get(
            &mut self,
            arena: &'a Arena<Micheline<'a>>,
            id: &BigMapId,
            key: &TypedValue,
            value_type: &Type,
        ) -> Result<std::option::Option<TypedValue<'a>>, LazyStorageError> {
            self.inner.big_map_get(arena, id, key, value_type)
        }

        fn big_map_mem(
            &mut self,
            id: &BigMapId,
            key: &TypedValue,
        ) -> Result<bool, LazyStorageError> {
            self.inner.big_map_mem(id, key)
        }

        fn big_map_update(
            &mut self,
            id: &BigMapId,
            key: TypedValue<'a>,
            value: std::option::Option<TypedValue<'a>>,
        ) -> Result<(), LazyStorageError> {
            self.inner.big_map_update(id, key, value)
        }

        fn big_map_new(
            &mut self,
            key_type: &Type,
            value_type: &Type,
            temporary: bool,
        ) -> Result<BigMapId, LazyStorageError> {
            self.inner.big_map_new(key_type, value_type, temporary)
        }

        fn big_map_copy(
            &mut self,
            id: &BigMapId,
            temporary: bool,
        ) -> Result<BigMapId, LazyStorageError> {
            self.inner.big_map_copy(id, temporary)
        }

        fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError> {
            self.inner.big_map_remove(id)
        }
    }

    /// The memo bounds a big-map-*free* DAG, but put a big map under the
    /// shared spine and every result is `Some`, so nothing is cacheable and
    /// the `2^K` unfolding is back — that is
    /// [walk_still_visits_every_occurrence_of_a_shared_big_map], and the
    /// multiplicity is required, since each occurrence needs its own id.
    ///
    /// What bounds *that* is the per-node gas charge. `DUP` is an `Rc::clone`
    /// and `list t` costs one type node per level, so the shape is `O(K²)`
    /// gas to build for `2^K` occurrences; the walk must run out of gas
    /// rather than out of heap or time. At `LEVELS` 20 an unmetered walk
    /// would visit over a million nodes.
    #[test]
    fn metered_walk_runs_out_of_gas_on_an_exponentially_shared_big_map() {
        const LEVELS: usize = 20;
        const BUDGET: u32 = 1_000_000;

        let mut storage = MeteredStorage {
            inner: InMemoryLazyStorage::new(),
            gas: crate::gas::Gas::new(BUDGET),
        };
        let mut root = shared_dag(leaf_big_map(0), LEVELS);

        let mut visited = 0;
        let result = root.for_each_big_map_mut(&mut storage, &mut |_, _| {
            visited += 1;
            Ok(())
        });

        assert!(
            matches!(result, Err(LazyStorageError::OutOfGasError(_))),
            "the walk completed instead of exhausting its budget: {result:?}"
        );
        // It stopped when the budget ran out, not after unfolding the DAG.
        let affordable = (BUDGET / tc_cost::LAZY_STORAGE_CYCLE) as usize;
        assert!(
            visited <= affordable,
            "visited {visited} big maps, but the budget only pays for {affordable} nodes"
        );
        assert!(
            visited < 2usize.pow(LEVELS as u32),
            "the walk unfolded the whole DAG despite running out of gas"
        );
    }

    /// The charge must not price ordinary contracts out: a storage of a few
    /// hundred nodes costs a few hundred cycles, not a meaningful fraction of
    /// an operation's budget.
    #[test]
    fn metered_walk_completes_on_an_ordinary_value() {
        const WIDTH: usize = 200;

        let mut storage = MeteredStorage {
            inner: InMemoryLazyStorage::new(),
            gas: crate::gas::Gas::new(crate::gas::DEFAULT_GAS_AMOUNT),
        };
        let mut root = TypedValue::List(MichelsonList::from(
            (0..WIDTH)
                .map(|i| Rc::new(TypedValue::int(i as i64)))
                .collect::<Vec<_>>(),
        ));

        let before = storage.gas.milligas().expect("budget is set");
        root.for_each_big_map_mut(&mut storage, &mut |_, _| Ok(()))
            .expect("an ordinary value must not exhaust the budget");
        let spent = before - storage.gas.milligas().expect("budget is set");

        // One cycle for the list itself plus one per element.
        assert_eq!(spent, (WIDTH as u32 + 1) * tc_cost::LAZY_STORAGE_CYCLE);
    }

    /// Only the elements holding a big map are rebuilt; everything around them
    /// is carried over as an `Rc::clone`. The big map sits in the *middle*, so
    /// both the prefix carried over before it and the suffix after it are
    /// exercised — a big map at either end would leave one of them empty.
    #[test]
    fn walk_carries_over_unchanged_list_elements() {
        let first = Rc::new(TypedValue::Bytes(vec![0x01; 32]));
        let last = Rc::new(TypedValue::Bytes(vec![0x02; 32]));
        let mut root = TypedValue::List(MichelsonList::from(vec![
            first.clone(),
            Rc::new(leaf_big_map(5)),
            last.clone(),
        ]));

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |map| {
            visited += 1;
            if let BigMapContent::FromId(ref mut m) = map.content {
                m.id = BigMapId::from(50);
            }
        });

        assert_eq!(visited, 1);
        let TypedValue::List(elements) = &root else {
            panic!("root is no longer a list")
        };
        let elements: Vec<_> = elements.iter().collect();
        assert_eq!(elements.len(), 3, "the rebuild changed the list length");
        assert!(Rc::ptr_eq(elements[0], &first), "the prefix was copied");
        assert!(Rc::ptr_eq(elements[2], &last), "the suffix was copied");
        let mut ids = vec![];
        root.view_big_map_ids(&mut ids);
        assert_eq!(ids, vec![BigMapId::from(50)]);
    }

    /// `Map` values are visited in ascending key order, and the rebuild must
    /// put each one back under the key it came from. A single-entry map — all
    /// the other tests here build one — cannot catch a key/value mispairing,
    /// nor an out-of-order visit.
    ///
    /// **The ascending order pins MIR's behaviour, which diverges from L1.**
    /// L1's `extract_lazy_storage_updates` walks a map's values *descending*:
    /// it builds its worklist with `M.OPS.fold (fun k v bs -> (k, v) :: bs)`,
    /// and since `fold` is ascending, prepending reverses it — then it folds
    /// left over the result without reversing back. (The same idiom in
    /// `script_ir_unparser.ml` *does* reverse, via `unparse_items`, which is
    /// how Michelson map literals come out ascending — so the direction is
    /// deliberate on both sides.) Because ids are allocated at visit time, a
    /// `map k (big_map …)` with several entries therefore gets different ids
    /// on MIR than on L1, and a different `lazy_storage_diff` order.
    ///
    /// Pre-existing — the `m.keys()` walk this replaced was ascending too —
    /// and out of scope here, since changing it is consensus-observable and
    /// touches the receipt-ordering work of L2-1735. This test exists so the
    /// current order cannot drift silently while that is decided; it is not
    /// an assertion that ascending is right.
    #[test]
    fn walk_visits_map_values_in_key_order_and_keeps_them_paired() {
        let key = |k: i64| Rc::new(TypedValue::int(k));
        // Inserted out of order on purpose: iteration order must come from the
        // tree, not from insertion.
        let mut root = TypedValue::Map(RedBlackTreeMap::from_iter([
            (key(2), Rc::new(leaf_big_map(2))),
            (key(0), Rc::new(leaf_big_map(0))),
            (key(1), Rc::new(leaf_big_map(1))),
        ]));

        // Renumber each big map to id + 10, so the rebuilt map shows where
        // each visited value landed.
        let mut visit_order = vec![];
        walk_big_maps(&mut root, &mut |map| {
            if let BigMapContent::FromId(ref mut m) = map.content {
                visit_order.push(m.id.clone());
                let BigMapId {
                    value: Zarith(ref id),
                } = m.id;
                let renumbered: i64 = id.try_into().expect("small test id");
                m.id = BigMapId::from(renumbered + 10);
            }
        });

        assert_eq!(
            visit_order,
            (0..=2).map(BigMapId::from).collect::<Vec<_>>(),
            "map values were visited out of key order"
        );
        let TypedValue::Map(m) = &root else {
            panic!("root is no longer a map")
        };
        assert_eq!(m.size(), 3, "the rebuild changed the map size");
        for k in 0..=2 {
            let mut ids = vec![];
            m.get(&key(k))
                .expect("key went missing from the rebuilt map")
                .view_big_map_ids(&mut ids);
            assert_eq!(
                ids,
                vec![BigMapId::from(k + 10)],
                "the rebuilt map paired a value with the wrong key"
            );
        }
    }

    /// Only one entry is rebuilt; the others stay shared with the original
    /// map, which is what makes the rebuild copy-on-write rather than a copy.
    #[test]
    fn walk_rebuilds_only_the_changed_map_entry() {
        let key = |k: i64| Rc::new(TypedValue::int(k));
        let low = Rc::new(TypedValue::Bytes(vec![0x0a; 32]));
        let high = Rc::new(TypedValue::Bytes(vec![0x0b; 32]));
        let mut root = TypedValue::Map(RedBlackTreeMap::from_iter([
            (key(0), low.clone()),
            (key(1), Rc::new(leaf_big_map(9))),
            (key(2), high.clone()),
        ]));

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |_| visited += 1);

        assert_eq!(visited, 1);
        let TypedValue::Map(m) = &root else {
            panic!("root is no longer a map")
        };
        assert!(
            Rc::ptr_eq(m.get(&key(0)).unwrap(), &low),
            "a big-map-free entry was copied"
        );
        assert!(
            Rc::ptr_eq(m.get(&key(2)).unwrap(), &high),
            "a big-map-free entry was copied"
        );
    }

    /// The `Operation` arms rebuild their struct field by field, so the
    /// compiler enforces that no field is *missing* — but not that each is
    /// carried over rather than defaulted. `counter` is the one that matters
    /// most: it is the DFS nonce that orders receipts, so a regression to `0`
    /// is consensus-observable, and every other test here builds its operation
    /// with `counter: 0` and never reads it back.
    #[test]
    fn walk_preserves_operation_fields() {
        let destination = crate::ast::michelson_address::Address::try_from(
            "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw",
        )
        .unwrap();
        let mut root = TypedValue::new_operation(
            crate::ast::Operation::TransferTokens(TransferTokens {
                param: Rc::new(leaf_big_map(3)),
                destination_address: destination.clone(),
                amount: 123_456,
            }),
            77,
        );

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |map| {
            visited += 1;
            if let BigMapContent::FromId(ref mut m) = map.content {
                m.id = BigMapId::from(30);
            }
        });

        assert_eq!(visited, 1, "the transfer parameter was not visited");
        let TypedValue::Operation(info) = &root else {
            panic!("root is no longer an operation")
        };
        assert_eq!(info.counter, 77, "the operation nonce was not carried over");
        let crate::ast::Operation::TransferTokens(t) = &info.operation else {
            panic!("the operation is no longer a transfer")
        };
        assert_eq!(t.amount, 123_456, "the amount was not carried over");
        assert_eq!(
            t.destination_address, destination,
            "the destination was not carried over"
        );
        let mut ids = vec![];
        root.view_big_map_ids(&mut ids);
        assert_eq!(ids, vec![BigMapId::from(30)]);
    }

    /// Same, for the `CREATE_CONTRACT` arm, which had no coverage at all: its
    /// storage must be walked and its five other fields carried over.
    #[test]
    fn walk_preserves_create_contract_fields() {
        use tezos_crypto_rs::hash::ContractKt1Hash;

        let micheline_code = Micheline::Seq(&[]);
        let code = Rc::new(crate::ast::ContractScript {
            parameter: Type::Unit,
            storage: Type::Unit,
            code: crate::ast::Instruction::Seq(Vec::new()),
            annotations: Default::default(),
            views: Default::default(),
        });
        let delegate =
            crate::ast::PublicKeyHash::try_from("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw")
                .unwrap();
        let address =
            ContractKt1Hash::try_from("KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg").unwrap();
        let mut root = TypedValue::new_operation(
            crate::ast::Operation::CreateContract(CreateContract {
                delegate: Some(delegate.clone()),
                amount: 999,
                storage: Rc::new(leaf_big_map(4)),
                code: code.clone(),
                micheline_code: &micheline_code,
                address: address.clone(),
            }),
            88,
        );

        let mut visited = 0;
        walk_big_maps(&mut root, &mut |map| {
            visited += 1;
            if let BigMapContent::FromId(ref mut m) = map.content {
                m.id = BigMapId::from(40);
            }
        });

        assert_eq!(visited, 1, "the originated storage was not visited");
        let TypedValue::Operation(info) = &root else {
            panic!("root is no longer an operation")
        };
        assert_eq!(info.counter, 88, "the operation nonce was not carried over");
        let crate::ast::Operation::CreateContract(cc) = &info.operation else {
            panic!("the operation is no longer an origination")
        };
        assert_eq!(cc.delegate, Some(delegate));
        assert_eq!(cc.amount, 999);
        assert_eq!(cc.address, address);
        assert!(Rc::ptr_eq(&cc.code, &code), "the code was copied");
        assert_eq!(cc.micheline_code, &micheline_code);
        let mut ids = vec![];
        root.view_big_map_ids(&mut ids);
        assert_eq!(ids, vec![BigMapId::from(40)]);
    }
}
