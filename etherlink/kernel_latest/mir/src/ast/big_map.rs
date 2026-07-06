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

use super::{Micheline, Type, TypedValue};
use crate::gas::OutOfGas;
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
    /// map inside it. Big maps nested in a map value are reached via
    /// [RedBlackTreeMap::get_mut], which path-copies the node so `f`'s writes
    /// stick.
    fn for_each_big_map_mut(&mut self, f: &mut impl FnMut(&mut BigMap<'a>)) {
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
                Rc::make_mut(l).for_each_big_map_mut(f);
                Rc::make_mut(r).for_each_big_map_mut(f);
            }
            Or(p) => match p {
                Left(l) => Rc::make_mut(l).for_each_big_map_mut(f),
                Right(r) => Rc::make_mut(r).for_each_big_map_mut(f),
            },
            Option(p) => {
                if let Some(x) = p.as_mut() {
                    Rc::make_mut(x).for_each_big_map_mut(f)
                }
            }
            List(l) => l
                .iter_mut()
                .for_each(|v| Rc::make_mut(v).for_each_big_map_mut(f)),
            Set(_) => {
                // Elements are comparable and so have no big maps
            }
            Map(m) => {
                // Keys are comparable (no big maps); visit each value via
                // get_mut. Collect the keys first so we can call get_mut,
                // which borrows the map mutably, one at a time.
                let keys: Vec<Rc<TypedValue<'a>>> = m.keys().cloned().collect();
                for k in keys {
                    if let Some(v) = m.get_mut(&k) {
                        Rc::make_mut(v).for_each_big_map_mut(f)
                    }
                }
            }
            BigMap(m) => f(m),
            Ticket(_) => {
                // Value is comparable, has no big map
            }
            Lambda(_) => {
                // Can contain only pushable values, thus no big maps
            }
            Operation(op) => match &mut op.as_mut().operation {
                crate::ast::Operation::TransferTokens(t) => {
                    t.param.for_each_big_map_mut(f)
                }
                crate::ast::Operation::SetDelegate(_) => {}
                crate::ast::Operation::Emit(_) => {
                    // Can contain only pushable values, thus no big maps
                }
                crate::ast::Operation::CreateContract(cc) => {
                    cc.storage.for_each_big_map_mut(f)
                }
            },
        }
    }

    /// Read-only counterpart of [TypedValue::for_each_big_map_mut]: applies
    /// `f` to every big map inside self in AST order without mutating it. Map
    /// values are walked via [RedBlackTreeMap::values].
    ///
    /// Kept exhaustive (no catch-all) and in lock-step with
    /// [TypedValue::for_each_big_map_mut] on purpose: this read-only walk feeds
    /// `started_with_map_ids` while the mutable walk feeds `seen_source_ids`,
    /// and [dump_big_map_updates] removes ids present in the former but not the
    /// latter. A new big-map-carrying variant must be added to both walks or a
    /// dropped big map could leak; a wildcard here would silently skip it while
    /// the mutable walk still compile-errored, so the two would diverge.
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
    // The per-big-map work is fallible, but for_each_big_map_mut takes an
    // infallible closure, so the first error is captured here and later big
    // maps are skipped (the tree walk still completes harmlessly).
    let mut result: Result<(), LazyStorageError> = Ok(());
    root.for_each_big_map_mut(&mut |map: &mut BigMap<'a>| {
        if result.is_ok() {
            result = dump_one_big_map(
                map,
                storage,
                temporary,
                seen_source_ids,
                &mut deferred_in_place_updates,
            );
        }
    });
    result?;

    // Return the deferred updates so a multi-walk caller applies them only
    // after every walk has copied from the pre-update source (L2-1761).
    Ok(deferred_in_place_updates)
}

/// Dump a single big map to the lazy storage, threading the dedup state across
/// maps. Split from [dump_big_map_walk] so the fallible per-map work can use
/// `?` inside the infallible [TypedValue::for_each_big_map_mut] closure.
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
        root.for_each_big_map_mut(&mut |m| {
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
