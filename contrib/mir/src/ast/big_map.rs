/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use num_bigint::BigInt;
use std::{
    collections::{btree_map::Entry, BTreeMap},
    fmt::Display,
    mem,
};
use typed_arena::Arena;

use super::{Micheline, Type, TypedValue};

/// Id of big map in the lazy storage.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BigMapId(pub BigInt);

impl Display for BigMapId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Represents a big_map value.
///
/// Big map is split into two parts - one is in the lazy storage, and another is
/// an in-memory overlay that carries a diff from the map in the storage.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BigMap<'a> {
    /// Id of the big map in the lazy storage.
    ///
    /// Big map can be backed by no map in the lazy storage and yet stay fully
    /// in memory, in such case this field is `None`.
    pub id: Option<BigMapId>,

    /// In-memory part, carries the diff that is to be applied to the map in the
    /// storage.
    ///
    /// Normally, execution of all writing instructions update this part, and at
    /// certain key points like the end of the contract execution this diff is
    /// dumped into the storage. Change in storage can be applied in-place or,
    /// if necessary, with copy of the stored map.
    pub overlay: BTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>,

    pub key_type: Type,
    pub value_type: Type,
}

impl<'a> BigMap<'a> {
    /// Michelson's `GET`.
    pub fn get(
        &self,
        arena: &'a Arena<Micheline<'a>>,
        key: &TypedValue,
        storage: &impl LazyStorage<'a>,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        Ok(match self.overlay.get(key) {
            // If the key is mentioned in the overlay, the associated value is
            // always used, even if it is `None` (and `get` returned
            // `Some(None)`) which means removal.
            Some(change) => change.clone(),
            None => match &self.id {
                Some(id) => storage.big_map_get(arena, id, key)?,
                None => None,
            },
        })
    }

    /// Michelson's `MEM`.
    pub fn mem(
        &self,
        key: &TypedValue,
        storage: &impl LazyStorage<'a>,
    ) -> Result<bool, LazyStorageError> {
        Ok(match self.overlay.get(key) {
            // If the key is mentioned in the overlay, the associated value is
            // always used, even if it is `None` (and `get` returned
            // `Some(None)`) which means removal.
            Some(change) => change.is_some(),
            None => match &self.id {
                Some(id) => storage.big_map_mem(id, key)?,
                None => false,
            },
        })
    }

    /// Michelson's `UPDATE`.
    pub fn update(&mut self, key: TypedValue<'a>, value: Option<TypedValue<'a>>) {
        self.overlay.insert(key, value);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum LazyStorageError {
    #[error("decode failed {0}")]
    DecodingError(String),
    #[error("{0}")]
    OtherError(String),
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
        &self,
        arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError>;

    /// Check whether a value is present under the given key of the given big
    /// map.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    /// Key type must match the type of key of the stored map.
    fn big_map_mem(&self, id: &BigMapId, key: &TypedValue) -> Result<bool, LazyStorageError>;

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

    /// Get key and value types of the map.
    ///
    /// This returns None if the map with such ID is not present in the storage.
    fn big_map_get_type(&self, id: &BigMapId) -> Result<Option<(&Type, &Type)>, LazyStorageError>;

    /// Allocate a new empty big map.
    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<BigMapId, LazyStorageError>;

    /// Allocate a new big map, filling it with the contents from another map
    /// in the lazy storage.
    ///
    /// The specified big map id must point to a valid map in the lazy storage.
    fn big_map_copy(&mut self, id: &BigMapId) -> Result<BigMapId, LazyStorageError>;

    /// Remove a big map.
    ///
    /// The caller is obliged to never use this big map ID in the given
    /// storage.
    fn big_map_remove(&mut self, id: &BigMapId) -> Result<(), LazyStorageError>;
}

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

impl<'a, T: LazyStorage<'a>> LazyStorageBulkUpdate<'a> for T {}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct MapInfo<'a> {
    map: BTreeMap<TypedValue<'a>, TypedValue<'a>>,
    key_type: Type,
    value_type: Type,
}

/// Simple implementation for [LazyStorage].
#[derive(Clone)]
pub struct InMemoryLazyStorage<'a> {
    next_id: BigInt,
    big_maps: BTreeMap<BigMapId, MapInfo<'a>>,
}

impl<'a> InMemoryLazyStorage<'a> {
    pub fn new() -> Self {
        InMemoryLazyStorage {
            next_id: 0.into(),
            big_maps: BTreeMap::new(),
        }
    }

    fn get_next_id(&mut self) -> BigMapId {
        let id = BigMapId(self.next_id.clone());
        self.next_id += 1;
        id
    }
}

impl<'a> Default for InMemoryLazyStorage<'a> {
    fn default() -> Self {
        InMemoryLazyStorage::new()
    }
}

impl<'a> InMemoryLazyStorage<'a> {
    fn access_big_map(&self, id: &BigMapId) -> Result<&MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get(id)
            .ok_or_else(|| panic!("Non-existent big map by id {id}"))
    }

    fn access_big_map_mut(&mut self, id: &BigMapId) -> Result<&mut MapInfo<'a>, LazyStorageError> {
        self.big_maps
            .get_mut(id)
            .ok_or_else(|| panic!("Non-existent big map by id {id}"))
    }
}

impl<'a> LazyStorage<'a> for InMemoryLazyStorage<'a> {
    fn big_map_get(
        &self,
        _arena: &'a Arena<Micheline<'a>>,
        id: &BigMapId,
        key: &TypedValue,
    ) -> Result<Option<TypedValue<'a>>, LazyStorageError> {
        let info = self.access_big_map(id)?;
        Ok(info.map.get(key).cloned())
    }

    fn big_map_mem(&self, id: &BigMapId, key: &TypedValue) -> Result<bool, LazyStorageError> {
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
                info.map.remove(&key);
            }
            Some(value) => {
                info.map.insert(key, value);
            }
        }
        Ok(())
    }

    fn big_map_get_type(&self, id: &BigMapId) -> Result<Option<(&Type, &Type)>, LazyStorageError> {
        Ok(self
            .big_maps
            .get(id)
            .map(|info| (&info.key_type, &info.value_type)))
    }

    fn big_map_new(
        &mut self,
        key_type: &Type,
        value_type: &Type,
    ) -> Result<BigMapId, LazyStorageError> {
        let id = self.get_next_id();
        self.big_maps.insert(
            id.clone(),
            MapInfo {
                map: BTreeMap::new(),
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

    fn big_map_copy(&mut self, copied_id: &BigMapId) -> Result<BigMapId, LazyStorageError> {
        let id = self.get_next_id();
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
        let map = BigMap {
            id: None,
            overlay: BTreeMap::from([(TypedValue::int(1), Some(TypedValue::int(1)))]),
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
        let map_id = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(1), Some(TypedValue::int(1)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(2), Some(TypedValue::int(2)))
            .unwrap();
        let map = BigMap {
            id: Some(map_id),
            overlay: BTreeMap::from([
                (TypedValue::int(1), Some(TypedValue::int(-1))),
                (TypedValue::int(2), None),
                (TypedValue::int(3), Some(TypedValue::int(3))),
            ]),
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
    /// Traverses a `TypedValue` and applies the `put_res` function on all big
    /// maps inside it.
    fn collect_big_maps<'b>(&'b mut self, put_res: &mut impl FnMut(&'b mut BigMap<'a>)) {
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
            Bls12381Fr(_) => {}
            Bls12381G1(_) => {}
            Bls12381G2(_) => {}
            Pair(p) => {
                p.0.collect_big_maps(put_res);
                p.1.collect_big_maps(put_res);
            }
            Or(p) => match p.as_mut() {
                Left(l) => l.collect_big_maps(put_res),
                Right(r) => r.collect_big_maps(put_res),
            },
            Option(p) => match p {
                Some(x) => x.collect_big_maps(put_res),
                None => {}
            },
            List(l) => l.iter_mut().for_each(|v| v.collect_big_maps(put_res)),
            Set(_) => {
                // Elements are comparable and so have no big maps
            }
            Map(m) => m.iter_mut().for_each(|(_k, v)| {
                // Key is comparable as so has no big map, skipping it
                v.collect_big_maps(put_res)
            }),
            BigMap(m) => put_res(m),
            Ticket(_) => {
                // Value is comparable, has no big map
            }
            Lambda(_) => {
                // Can contain only pushable values, thus no big maps
            }
            Operation(op) => match &mut op.as_mut().operation {
                crate::ast::Operation::TransferTokens(t) => t.param.collect_big_maps(put_res),
                crate::ast::Operation::SetDelegate(_) => {}
            },
        }
    }

    pub fn view_big_maps_mut<'b>(&'b mut self, out: &mut Vec<&'b mut BigMap<'a>>) {
        self.collect_big_maps(&mut |m| out.push(m));
    }

    pub fn view_big_map_ids<T>(&mut self, out: &mut Vec<BigMapId>) {
        self.collect_big_maps(&mut |m| {
            if let Some(id) = &m.id {
                out.push(id.clone())
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
    storage: &mut impl LazyStorage<'a>,
    started_with_map_ids: &[BigMapId],
    finished_with_maps: &mut [&mut BigMap<'a>],
) -> Result<(), LazyStorageError> {
    // Note: this function is similar to `extract_lazy_storage_diff` from the
    // Tezos protocol implementation. The difference is that we don't have
    // their's `to_duplicate` argument.
    //
    // Temporarily we go with a simpler solution where each ID in
    // `started_with_map_ids` is guaranteed to be used by only one big map, a
    // big map that comes from parameter or storage value; these IDs are
    // expected to be not used by big maps in other contracts. Consequences of
    // this:
    // * If a contract produces an operation with a big map, we immediately
    // deduplicate big map ID there too (the Tezos protocol implementation does
    // not).
    // * There is no need to implement temporary lazy storage for now.

    // The `finished_with_maps` vector above is supposed to contain all big maps
    // remaining on stack at the end of contract execution. After this function
    // call, we want the provided big maps to satisfy the following invariants:
    // * No `BigMapId` appears twice. This ensures that a big map in the storage
    //   cannot be updated in-parallel via different `Value::BigMap` values.
    // * Big maps, whose IDs are gone, are removed from the lazy storage.
    // * Best effort is made to avoid copying big maps in the lazy storage, big
    //   maps are updated in-place when possible.

    // First, we find big maps that are related to same big map IDs in the
    // storage. This is necessary to understand which maps will be updated in
    // lazy storage in-place and which have to be copied.
    //
    // With big map IDs we associate a non-empty list of big maps.
    // Where "non-empty" is kept `(T, Vec<T>)` for convenience. Note
    // that in the vast majority of the real-life cases big maps are not
    // de-facto copied, so the vector will usually stay empty and produce no
    // allocations.
    type NonEmpty<T> = (T, Vec<T>);
    let mut grouped_maps: BTreeMap<BigMapId, NonEmpty<&mut BigMap>> = BTreeMap::new();
    for map in finished_with_maps {
        match map.id {
            Some(ref id) => {
                // Insert to grouped_maps
                match grouped_maps.entry(id.clone()) {
                    Entry::Vacant(e) => {
                        e.insert((map, Vec::new()));
                    }
                    Entry::Occupied(e) => e.into_mut().1.push(map),
                }
            }
            None => {
                // ID is empty, meaning that the entire big map is still in
                // memory. We have to create a new map in the storage.
                let id = storage.big_map_new(&map.key_type, &map.value_type)?;
                storage.big_map_bulk_update(&id, mem::take(&mut map.overlay))?;
                map.id = Some(id)
            }
        };
    }

    // Remove big maps that were gone.
    for map_id in started_with_map_ids {
        // If not found in `finished_with_maps`...
        if !grouped_maps.contains_key(map_id) {
            storage.big_map_remove(map_id)?
        }
    }

    // Update lazy storage with data from overlay.
    for (id, (main_map, other_maps)) in grouped_maps {
        // If there are any big maps with duplicate ID, we first copy them in
        // the storage.
        for map in other_maps {
            let new_id = storage.big_map_copy(&id)?;
            storage.big_map_bulk_update(&new_id, mem::take(&mut map.overlay))?;
            map.id = Some(new_id)
        }
        // The only remaining big map we update in the lazy storage in-place.
        storage.big_map_bulk_update(&id, mem::take(&mut main_map.overlay))?
    }

    Ok(())
}

#[cfg(test)]
mod test_big_map_to_storage_update {
    use crate::ast::Type;

    use super::*;

    #[track_caller]
    fn check_is_dumped_map(map: BigMap, id: BigMapId) {
        assert_eq!((map.id, map.overlay), (Some(id), BTreeMap::new()));
    }

    #[test]
    fn test_map_from_memory() {
        let storage = &mut InMemoryLazyStorage::new();
        let mut map = BigMap {
            id: None,
            overlay: BTreeMap::from([
                (TypedValue::int(1), Some(TypedValue::int(1))),
                (TypedValue::int(2), Some(TypedValue::int(2))),
            ]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        dump_big_map_updates(storage, &[], &mut [&mut map]).unwrap();

        check_is_dumped_map(map, BigMapId(0.into()));
        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                BigMapId(0.into()),
                MapInfo {
                    map: BTreeMap::from([
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
        let map_id = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        storage
            .big_map_update(&map_id, TypedValue::int(1), Some(TypedValue::int(1)))
            .unwrap();
        let mut map = BigMap {
            id: Some(map_id),
            overlay: BTreeMap::from([
                (TypedValue::int(0), None),
                (TypedValue::int(1), Some(TypedValue::int(5))),
                (TypedValue::int(2), None),
                (TypedValue::int(3), Some(TypedValue::int(3))),
            ]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        dump_big_map_updates(storage, &[], &mut [&mut map]).unwrap();

        check_is_dumped_map(map, BigMapId(0.into()));
        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                BigMapId(0.into()),
                MapInfo {
                    map: BTreeMap::from([
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
        let map_id1 = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        let map_id2 = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        let mut map1_1 = BigMap {
            id: Some(map_id1.clone()),
            overlay: BTreeMap::from([(TypedValue::int(11), Some(TypedValue::int(11)))]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let mut map1_2 = BigMap {
            id: Some(map_id1),
            overlay: BTreeMap::from([(TypedValue::int(12), Some(TypedValue::int(12)))]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        let mut map2 = BigMap {
            id: Some(map_id2),
            overlay: BTreeMap::from([(TypedValue::int(2), Some(TypedValue::int(2)))]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        dump_big_map_updates(storage, &[], &mut [&mut map1_1, &mut map1_2, &mut map2]).unwrap();

        check_is_dumped_map(map1_1, BigMapId(0.into()));
        check_is_dumped_map(map1_2, BigMapId(2.into())); // newly created map
        check_is_dumped_map(map2, BigMapId(1.into()));

        assert_eq!(
            storage.big_maps,
            BTreeMap::from([
                (
                    BigMapId(0.into()),
                    MapInfo {
                        map: BTreeMap::from([(TypedValue::int(11), TypedValue::int(11))]),
                        key_type: Type::Int,
                        value_type: Type::Int
                    }
                ),
                (
                    BigMapId(1.into()),
                    MapInfo {
                        map: BTreeMap::from([(TypedValue::int(2), TypedValue::int(2))]),
                        key_type: Type::Int,
                        value_type: Type::Int
                    }
                ),
                (
                    BigMapId(2.into()),
                    MapInfo {
                        map: BTreeMap::from([(TypedValue::int(12), TypedValue::int(12))]),
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
        let map_id1 = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        storage
            .big_map_update(&map_id1, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let map_id2 = storage.big_map_new(&Type::Int, &Type::Int).unwrap();
        storage
            .big_map_update(&map_id2, TypedValue::int(0), Some(TypedValue::int(0)))
            .unwrap();
        let mut map1 = BigMap {
            id: Some(map_id1.clone()),
            overlay: BTreeMap::from([(TypedValue::int(1), Some(TypedValue::int(1)))]),
            key_type: Type::Int,
            value_type: Type::Int,
        };
        dump_big_map_updates(storage, &[map_id1, map_id2], &mut [&mut map1]).unwrap();

        assert_eq!(
            storage.big_maps,
            BTreeMap::from([(
                BigMapId(0.into()),
                MapInfo {
                    map: BTreeMap::from([
                        (TypedValue::int(0), TypedValue::int(0)),
                        (TypedValue::int(1), TypedValue::int(1))
                    ]),
                    key_type: Type::Int,
                    value_type: Type::Int
                }
            )])
        );
    }
}
